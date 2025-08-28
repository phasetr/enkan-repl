;;; enkan-repl.el --- Enhanced repl utilities for enkan -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Version: 0.10.0
;; Package-Requires: ((emacs "28.1") (eat "0.9.4"))
;; Keywords: enkan ai tools convenience
;; URL: https://github.com/phasetr/enkan-repl
;; License: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; enkan-repl revives interactive development culture for the AI era.
;; Send text to terminal sessions with `send-region`, interact with AI tools,
;; and deepen your thinking further - a new REPL experience woven by Emacs.
;;
;; Core Features:
;; - Text sending capabilities (region, buffer, line, etc.)
;; - Persistent org-mode input files with template system
;; - Directory-based terminal session targeting
;; - M-x driven workflow with no default keybindings
;; - File path support for AI tool direct reading
;;
;; Quick Start:
;;   M-x enkan-repl-start-eat
;;   M-x enkan-repl-open-project-input-file
;;
;; This package builds upon eat terminal emulator.  We extend our deepest
;; gratitude to their authors and contributors.

;;; Code:

(require 'cl-lib)

;; Load utility functions (require for template generation and cheat-sheet)
(when (locate-library "enkan-repl-utils")
  (require 'enkan-repl-utils))

;; Load macOS specific notifications if on macOS
(when (and (eq system-type 'darwin) (locate-library "enkan-repl-mac-notify"))
  (require 'enkan-repl-mac-notify))

;; Declare external functions to avoid byte-compile warnings
(declare-function enkan-repl-utils--extract-function-info "enkan-repl-utils" (file-path))
(declare-function enkan-repl--find-session-buffer-pure "enkan-repl-utils" (selected-name buffer-info-list))
(declare-function enkan-repl--collect-sessions-pure "enkan-repl-utils" (buffer-info-list))
(declare-function enkan-repl--format-numbered-sessions-pure "enkan-repl-utils" (sessions))

;; Declare external functions to avoid byte-compiler warnings
(declare-function eat "eat" (&optional program))
(declare-function eat--send-string "eat" (process string))
(defvar eat--process)
(defvar eat-mode)

;;;; Custom Variables

(defgroup enkan-repl nil
  "Enhanced REPL utilities for enkan."
  :group 'tools
  :prefix "enkan-repl-")

;;;; Constants

(defconst enkan-repl-path-separator "--"
  "String to replace path separators (/) in filenames.
This is a fixed part of the enkan-repl filename encoding scheme
and should not be changed to maintain compatibility and security.")

(defconst enkan-repl-default-template-filename "default.org"
  "Default template filename for enkan-repl input files.
This file should be located in the package directory and contains
the standard template structure for new project input files.")

(defconst enkan-repl-file-prefix "enkan"
  "Prefix for enkan-repl encoded filenames.
This is a fixed part of the filename encoding scheme and should not be changed
to maintain compatibility with existing project files.")

(defconst enkan-repl-command-categories
  '("Command Palette"
    "Text Sender"
    "Session Controller"
    "Utilities")
  "Command category names for enkan-repl package.
Command Palette is positioned first as the primary interface.
This structure is used for documentation generation and organization.")

;; Declare external variable from constants file
(defvar enkan-repl-cheat-sheet-candidates nil
  "Precompiled list of cheat-sheet candidates from constants file.")

(defun enkan-repl--find-template-directory ()
  "Find directory containing default template file.
Returns the directory path where default.org is located."
  (or
   ;; Try load-file-name first (when loading with load command)
   (and load-file-name
        (let ((dir (file-name-directory load-file-name)))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; Try buffer-file-name (when evaluating in buffer)
   (and buffer-file-name
        (let ((dir (file-name-directory buffer-file-name)))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; Try locate-library result directory
   (and (locate-library "enkan-repl")
        (let ((dir (file-name-directory (locate-library "enkan-repl"))))
          (when (file-exists-p (expand-file-name enkan-repl-default-template-filename dir))
            dir)))
   ;; For straight.el: try repos directory if build directory doesn't have template
   (and (locate-library "enkan-repl")
        (let* ((build-dir (file-name-directory (locate-library "enkan-repl")))
               (straight-build-p (string-match-p "/straight/build/" build-dir))
               (repos-dir (and straight-build-p
                               (replace-regexp-in-string "/straight/build/" "/straight/repos/" build-dir))))
          (when (and repos-dir (file-exists-p (expand-file-name enkan-repl-default-template-filename repos-dir)))
            repos-dir)))
   ;; Search for default.org in load-path directories
   (cl-some
    (lambda (dir)
      (let ((expanded-dir (expand-file-name dir))
            (default-org-path (expand-file-name enkan-repl-default-template-filename (expand-file-name dir))))
        (when (file-exists-p default-org-path)
          expanded-dir)))
    load-path)
   ;; Final fallback to current directory
   default-directory))

(defvar enkan-repl--package-directory
  (enkan-repl--find-template-directory)
  "Package directory determined at load time.")

(defcustom enkan-repl-template-file nil
  "Template file path for enkan-repl input files.
When nil, uses the default template (default.org).
When set to a file path, uses that file as the template."
  :type
  '(choice
    (const :tag "Use default template" nil)
    (file :tag "Custom template file"))
  :group 'enkan-repl)

;;;; Multi-buffer access variables

(defcustom enkan-repl-center-file nil
  "Center file path for multi-session management.
When nil, multi-session functionality is disabled."
  :type '(choice (const :tag "Multi-session functionality disabled" nil)
                 (file :tag "Center file path"))
  :group 'enkan-repl)

(defcustom enkan-repl-project-aliases nil
  "Project name alias definitions,
auto-generated by the custom variable enkan-repl-center-project-registry.
Supports both global and buffer-local configuration.
Example: \\='((\"pt\" . \"pt-tools\") (\"er\" . \"enkan-repl\"))"
  :type '(alist :key-type string :value-type string)
  :group 'enkan-repl)

(defcustom enkan-repl-center-project-registry nil
  "Project startup registry for center file functionality.
Each element is in the format (alias . (project-name . project-path)).
Example: \\='((\"pt\" . (\"pt-tools\" . \"/path/to/pt-tools/\"))
      (\"er\" . (\"enkan-repl\" . \"/path/to/enkan-repl/\"))
      (\"eat\" . (\"emacs-eat\" . \"/path/to/emacs-eat/\")))"
  :type '(alist :key-type string
                :value-type (cons string string))
  :group 'enkan-repl)

(defcustom enkan-repl-center-multi-project-layouts nil
  "Configuration list for frequently used multi-project simultaneous startup.
Each element is in the format (configuration-name . alias-list).
Aliases are specified from left to right according to window layout.
Example: \\='((\"web-dev\" . (\"er\" \"pt\" \"cc\"))
      (\"data-analysis\" . (\"pt\" \"jupyter\" \"postgres\")))"
  :type '(alist :key-type string
                :value-type (repeat string))
  :group 'enkan-repl)

(defcustom enkan-repl-session-list nil
  "List of managed sessions.
Each element is in the format (number . project-name).
Uses session numbers 1,2 directly.
Example: \\='((1 . \"pt-tools\") (2 . \"enkan-repl\"))"
  :type '(alist :key-type integer :value-type string)
  :group 'enkan-repl)

(defcustom enkan-repl-default-session-projects nil
  "Alist of default session projects to open.
Example: \\='((1 . \"project1\") (2 . \"project2\") (3 . \"project3\") (4 . \"project4\"))."
  :type '(alist :key-type integer :value-type string)
  :group 'enkan-repl)

;;;; Multi-buffer access variables (continued)

(defvar enkan-repl--session-counter 0
  "Session startup counter (for automatic numbering).")

(defvar enkan-repl--current-multi-project-layout nil
  "Currently selected multi-project-layout name.
When nil, executes normal setup behavior.")

;;;; Core Functions

;;;; File Naming and Path Management

(defun enkan-repl--encode-full-path (path)
  "Encode PATH by replacing forward slashes with the configured separator.
Example: \\='/Users/phasetr/project1/\\=' -> \\='enkan--Users--phasetr--project1\\='"
  (enkan-repl--encode-full-path-pure
   path
   enkan-repl-file-prefix
   enkan-repl-path-separator))

(defun enkan-repl--decode-full-path (encoded-name)
  "Decode ENCODED-NAME back to original path.
Example: \\='enkan--Users--phasetr--project1\\=' -> \\='/Users/phasetr/project1/\\='"
  (enkan-repl--decode-full-path-pure
   encoded-name
   enkan-repl-file-prefix
   enkan-repl-path-separator))

(defun enkan-repl--get-project-file-path (&optional directory)
  "Get the full path for the project file based on DIRECTORY.
If DIRECTORY is nil, use the current `default-directory'."
  (let*
      ((target-dir (or directory default-directory))
       (encoded-name (enkan-repl--encode-full-path target-dir))
       (filename (concat encoded-name ".org")))
    (expand-file-name filename target-dir)))

;;;; Template Loading Functions

(defun enkan-repl--get-package-directory ()
  "Get package directory for template files.
Returns directory path where default templates are located."
  (or (enkan-repl--find-template-directory)
      enkan-repl--package-directory
      default-directory))

(defvar enkan-repl--testing-mode nil
  "When non-nil, disable interactive prompts for testing.")

(defun enkan-repl--handle-missing-template (template-path)
  "Handle missing template file with interactive user choices.
TEMPLATE-PATH is the path to the missing template file.
Returns the template path to use, or nil to use default template."
  ;; In testing mode, just return nil to use default template
  (if
      enkan-repl--testing-mode
      nil
    (let
        ((choice
          (read-char-choice
           (format "Template file not found: %s\n\nChoose action:\n(d) Use default template\n(s) Select different file\n(c) Create new template file\n(q) Quit\n\nYour choice: "
                   template-path)
           '(?d ?s ?c ?q))))
      (cl-case choice
        (?d
         (message "Using default template instead")
         nil)  ; Use default template
        (?s
         (let
             ((selected-file
               (read-file-name "Select template file: "
                               user-emacs-directory nil t nil
                               (lambda (name) (string-suffix-p ".org" name)))))
           (if
               (file-exists-p selected-file)
               selected-file
             (progn
               (message "Selected file does not exist. Using default template.")
               nil))))
        (?c
         (when
             (y-or-n-p
              (format "Create new template file at %s? " template-path))
           (let
               ((dir (file-name-directory template-path)))
             ;; Ensure directory exists
             (unless
                 (file-exists-p dir)
               (make-directory dir t))
             ;; Create template with dynamically generated command list
             (with-temp-file template-path
               (insert "* Quick Start\n\n")
               (insert "** Custom Template\n")
               (insert "This is your custom template.\n")
               (insert "Edit this content to match your needs.\n\n")
               (insert "** Available Commands\n\n")
               ;; Insert static function list
               (insert "- ~M-x enkan-repl-send-region~ - Send selected text to Claude\n")
               (insert "- ~M-x enkan-repl-send-buffer~ - Send entire buffer content\n")
               (insert "- ~M-x enkan-repl-send-rest-of-buffer~ - Send from cursor position to end\n")
               (insert "- ~M-x enkan-repl-send-line~ - Send current line\n")
               (insert "- ~M-x enkan-repl-send-enter~ - Send enter key for prompts\n")
               (insert "- ~M-x enkan-repl-send-1~ - Send '1' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-2~ - Send '2' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-3~ - Send '3' for numbered choice prompts\n")
               (insert "- ~M-x enkan-repl-send-escape~ - Send ESC key to interrupt operations\n")
               (insert "- ~M-x enkan-repl-open-project-input-file~ - Open or create project input file\n")
               (insert "- ~M-x enkan-repl-start-eat~ - Start eat terminal session\n")
               (insert "- ~M-x enkan-repl-setup~ - Set up convenient window layout\n")
               (insert "- ~M-x enkan-repl-output-template~ - Export template for customization\n")
               (insert "- ~M-x enkan-repl-status~ - Show diagnostic information\n")
               (insert "\n** Working Notes\n")
               (insert "Write your thoughts and notes here.\n")
               (insert "Send specific parts to a eat buffer using the commands above.\n"))
             (message "Created new template file: %s" template-path)
             template-path)))
        (?q
         (user-error "Operation cancelled by user"))))))

(defun enkan-repl--load-template ()
  "Load template content based on enkan-repl-template-file setting.
Returns template content as string, using embedded template as fallback."
  (let
      ((template-path
        (cond
         ;; If custom template file is specified, check if it exists
         (enkan-repl-template-file
          (let ((custom-path (expand-file-name enkan-repl-template-file)))
            (if (file-exists-p custom-path)
                custom-path
              ;; Handle missing custom template interactively
              (or (enkan-repl--handle-missing-template custom-path)
                  ;; Fall back to default if user chooses default
                  (expand-file-name enkan-repl-default-template-filename
                                    (enkan-repl--get-package-directory))))))
         ;; Otherwise, look for default template in package directory
         (t (expand-file-name enkan-repl-default-template-filename
                              (enkan-repl--get-package-directory))))))
    (if (and template-path (file-exists-p template-path))
        (with-temp-buffer
          (insert-file-contents template-path)
          (buffer-string))
      ;; Fall back to embedded template
      (enkan-repl--get-embedded-template))))

(defun enkan-repl--get-categorized-functions ()
  "Get categorized function list from constants file.
Returns categorized functions as string, or falls back to static list."
  (condition-case nil
      (progn
        ;; Try to load constants file if not already loaded
        (unless (featurep 'enkan-repl-constants)
          (require 'enkan-repl-constants))
        ;; Group functions by category
        (let ((categories (make-hash-table :test 'equal))
              (category-order '("Command Palette" "Text Sender" "Session Controller" "Utilities")))
          ;; Group functions by category
          (dolist (candidate enkan-repl-cheat-sheet-candidates)
            (let* ((func-name (car candidate))
                   (description (cdr candidate))
                   (category (if (string-match "Category: \\([^\"]+\\)" description)
                                 (match-string 1 description)
                               "Other")))
              (unless (gethash category categories)
                (puthash category nil categories))
              (puthash category
                       (cons (cons func-name description) (gethash category categories))
                       categories)))
          ;; Generate categorized output
          (let ((result ""))
            (dolist (category category-order)
              (when (gethash category categories)
                (setq result (concat result "** " category "\n\n"))
                (dolist (func (reverse (gethash category categories)))
                  (let* ((func-name (car func))
                         (description (cdr func))
                         (clean-desc (replace-regexp-in-string "  Category: [^\"]*" "" description)))
                    (setq result (concat result "- ~M-x " func-name "~ - " clean-desc "\n"))))
                (setq result (concat result "\n"))))
            result)))
    ;; Fallback to static list if constants file is unavailable
    (error (enkan-repl--get-static-functions))))

(defun enkan-repl--get-static-functions ()
  "Return static function list as fallback when constants unavailable."
  (concat "** Command Palette\n\n"
          "- ~M-x enkan-repl-cheat-sheet~ - Display interactive cheat-sheet for enkan-repl commands.\n\n"
          "** Text Sender\n\n"
          "- ~M-x enkan-repl-send-region~ - Send the text in region from START to END to eat session.\n"
          "- ~M-x enkan-repl-send-buffer~ - Send the entire current buffer to eat session.\n"
          "- ~M-x enkan-repl-send-rest-of-buffer~ - Send rest of buffer from cursor position to end to eat session.\n"
          "- ~M-x enkan-repl-send-line~ - Send the current line to eat session.\n"
          "- ~M-x enkan-repl-send-enter~ - Send enter key to eat session buffer.\n"
          "- ~M-x enkan-repl-send-1~ - Send \\='1\\=' to eat session buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-2~ - Send \\='2\\=' to eat session buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-3~ - Send \\='3\\=' to eat session buffer for numbered choice prompts.\n"
          "- ~M-x enkan-repl-send-escape~ - Send ESC key to eat session buffer.\n\n"
          "** Session Controller\n\n"
          "- ~M-x enkan-repl-start-eat~ - Start eat terminal and change to appropriate directory.\n"
          "- ~M-x enkan-repl-setup~ - Set up window layout with org file on left and eat on right.\n\n"
          "** Utilities\n\n"
          "- ~M-x enkan-repl-open-project-input-file~ - Open or create project input file for current directory.\n"
          "- ~M-x enkan-repl-output-template~ - Output current template content to a new buffer for customization.\n"
          "- ~M-x enkan-repl-status~ - Show detailed diagnostic information for troubleshooting connection issues.\n"))

(defun enkan-repl--get-embedded-template ()
  "Return embedded default template content as string."
  (concat "#+TITLE: Enkan Input File\n\n"
          "This is the default project template for enkan-repl.\n\n"
          "* Quick Start\n\n"
          "Describe your main objective here.\n\n"
          "* Context\n\n"
          "Provide relevant background information, constraints, or requirements.\n\n"
          "* Approach\n\n"
          "Outline your planned approach or methodology.\n\n"
          "* Functions/Commands\n\n"
          "We open the following functions/commands.\n\n"
          (enkan-repl--get-categorized-functions)
          "* Notes\n\n"
          "Add your thoughts, observations, or important points here.\n\n"
          "* Next Steps\n\n"
          "- [ ] Item 1\n"
          "- [ ] Item 2\n"
          "- [ ] Item 3\n"))

;;;; Pure Functions for Directory/Buffer Detection

(defun enkan-repl--encode-full-path-pure (path prefix separator)
  "Pure function: Encode PATH with PREFIX and SEPARATOR.
PATH: Directory path to encode.
PREFIX: Prefix string to add (e.g., \\='enkan\\=').
SEPARATOR: String to replace '/' with (e.g., '--').
Example: \='/Users/project\=' + \='enkan\=' + \='--\=' -> \='enkan--Users--project\='"
  (let*
      ((expanded-path (expand-file-name path))
       (cleaned-path
        (if
            (string-suffix-p "/" expanded-path)
            (substring expanded-path 0 -1)
          expanded-path)))
    (concat prefix (replace-regexp-in-string "/" separator cleaned-path))))

(defun enkan-repl--decode-full-path-pure (encoded-name prefix separator)
  "Pure function: Decode ENCODED-NAME with PREFIX and SEPARATOR.
ENCODED-NAME: Encoded filename to decode.
PREFIX: Expected prefix string (e.g., \\='enkan\\=').
SEPARATOR: String to replace with '/' (e.g., '--').
Example: \='enkan--Users--project\=' + \='enkan\=' + \='--\=' -> \='/Users/project/\='"
  (when (string-prefix-p prefix encoded-name)
    (let
        ((path-part (substring encoded-name (length prefix))))  ; Remove prefix
      (concat (replace-regexp-in-string (regexp-quote separator) "/" path-part) "/"))))

(defun enkan-repl--sanitize-content (content)
  "Sanitize CONTENT to ensure it can be safely sent to eat session.
This function handles edge cases with special characters and ensures
proper formatting for terminal input.  Also addresses Claude Code
interpretation issues and Mac region selection problems."
  (when content
    (let ((sanitized content))
      ;; Convert all line ending variations to LF
      (setq sanitized (replace-regexp-in-string "\r\n\\|\r" "\n" sanitized))
      ;; Remove other problematic control characters
      ;; Keep only \n (newline) and \t (tab) among control characters
      (setq
       sanitized
       (replace-regexp-in-string
        "[[:cntrl:]]"
        (lambda (match)
          (cond
           ;; Keep normal newlines and tabs
           ((or (string= match "\n") (string= match "\t")) match)
           ;; Remove other control characters
           (t "")))
        sanitized))
      ;; Remove Unicode line separators that might cause issues
      ;; U+0085 (NEL), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR)
      (setq sanitized (replace-regexp-in-string "[\u0085\u2028\u2029]" "" sanitized))
      ;; Final cleanup of any remaining problematic characters at end
      (setq sanitized (replace-regexp-in-string "[\x0B\x0C\x0E-\x1F]+\\'" "" sanitized))
      ;; File path interpretation workaround
      (when
          (and
           (string-match-p "~/[^[:space:]]*\\.[a-zA-Z0-9]+\\'" sanitized)
           (not (string-match-p "[.!?]\\'" sanitized)))
        (setq sanitized (concat sanitized "\n(This text is added by enkan-repl as a workaround for Claude Code's special interpretation of file paths)")))
      ;; Only remove trailing whitespace/newlines
      (setq sanitized (replace-regexp-in-string "[[:space:]]+\\'" "" sanitized))
      sanitized)))

;;;; Target Directory Detection Functions

(defun enkan-repl--get-target-directory-for-buffer ()
  "Get the target directory for current buffer.
For persistent files, extract directory from filename.
For other buffers, use current `default-directory'."
  (cond
   ;; Center file case - use current directory
   ((and buffer-file-name
         enkan-repl-center-file
         (string= (expand-file-name buffer-file-name)
                  (expand-file-name enkan-repl-center-file)))
    default-directory)
   ;; Regular persistent file case
   ((and buffer-file-name
         (string-match-p "enkan-.+\\.org$" (file-name-nondirectory buffer-file-name)))
    ;; This is a persistent file, extract directory from filename
    (enkan-repl--decode-full-path
     (file-name-base (file-name-nondirectory buffer-file-name))))
   ;; Default case
   (t default-directory)))

;;;; Multi-buffer access core functions

(defun enkan-repl--extract-project-name (buffer-name-or-path)
  "Extract final directory name from buffer name or path for use as project name.
Example: \\='*enkan:/path/to/pt-tools/*\\=' -> \\='pt-tools\\='"
  (let ((path (if (string-match "\\*enkan:\\(.+\\)\\*" buffer-name-or-path)
                  (match-string 1 buffer-name-or-path)
                buffer-name-or-path)))
    (file-name-nondirectory (directory-file-name path))))

(defun enkan-repl--resolve-project-name (name-or-alias)
  "Resolve project name or alias to canonical project name.
References alias definitions, returns original name if not found."
  (or (cdr (assoc name-or-alias enkan-repl-project-aliases))
      name-or-alias))

(defun enkan-repl--center-resolve-project-name (alias)
  "Alias resolution for center file functionality.
Resolves alias to canonical project name from project aliases."
  (or (cdr (assoc alias enkan-repl-project-aliases))
      alias))

(defun enkan-repl--get-project-directory-from-registry (alias)
  "Get directory path from project registry.
Returns: Directory path or nil"
  (let ((project-info (cdr (assoc alias enkan-repl-center-project-registry))))
    (when project-info
      (cdr project-info))))  ; project-path part

(defun enkan-repl--register-session (session-number project-name)
  "Register project to session number.
Order is maintained by session number (ascending)."
  (let ((updated-list (assq-delete-all session-number enkan-repl-session-list))
        (new-entry (cons session-number project-name)))
    (setq enkan-repl-session-list
          (sort (cons new-entry updated-list)
                (lambda (a b) (< (car a) (car b)))))))

(defun enkan-repl--extract-directory-from-buffer-name-pure (buffer-name)
  "Pure function to extract expanded directory path from enkan buffer name.
Returns expanded directory path or nil if buffer name is not valid enkan format."
  (when (and (stringp buffer-name) (string-match "^\\*enkan:\\(.*\\)\\*$" buffer-name))
    (let ((raw-path (match-string 1 buffer-name)))
      (file-name-as-directory (expand-file-name raw-path)))))

(defun enkan-repl--find-directory-by-project-name (project-name)
  "Search for corresponding directory by project name.
Looks for matching directory from existing enkan-repl buffers.
Returns: Directory path or nil"
  (cl-block search-buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (string-match "^\\*enkan:" (buffer-name))
                   (string-equal project-name
                                (enkan-repl--extract-project-name (buffer-name))))
          (cl-return-from search-buffers
            (enkan-repl--extract-directory-from-buffer-name-pure (buffer-name))))))))

(defun enkan-repl--buffer-matches-directory-pure (buffer-name target-directory)
  "Pure function to check if buffer name matches target directory.
Returns t if buffer is enkan buffer for target directory, nil otherwise."
  (and (stringp buffer-name)
    (stringp target-directory)
    (string-match-p "^\\*enkan:" buffer-name)
    (let ((expanded-target (expand-file-name target-directory)))
      (string= buffer-name (concat "*enkan:" expanded-target "*")))))

(defun enkan-repl--get-buffer-for-directory (&optional directory)
  "Get the eat buffer for DIRECTORY if it exists and is live.
If DIRECTORY is nil, use current `default-directory'."
  (let
      ((target-dir (or directory default-directory))
       (matching-buffer nil))
    (cl-block search-buffers
      (dolist (buf (buffer-list))
        (let
            ((name (buffer-name buf))
             (eat-mode
              (with-current-buffer buf
                (and (boundp 'eat-mode) eat-mode))))
          (when
              (and (buffer-live-p buf)
                   name     ; Ensure name is not nil
                   ;; Check for directory-specific enkan buffer using pure function
                   (enkan-repl--buffer-matches-directory-pure name target-dir))
            (setq matching-buffer buf)
            (cl-return-from search-buffers)))))
    matching-buffer))

(defun enkan-repl--buffer-matches-directory (buffer target-dir)
  "Check if BUFFER's working directory matches TARGET-DIR."
  (with-current-buffer buffer
    (when (boundp 'default-directory)
      (string=
       (file-truename default-directory)
       (file-truename target-dir)))))

;;;; Send Functions - Internal Helpers

(defun enkan-repl--can-send-text (&optional directory)
  "Check if text can actually be sent to eat session (strict check).
If DIRECTORY is provided, check for eat session in that directory.
Otherwise, use current `default-directory'."
  (let ((session-buffer (enkan-repl--get-buffer-for-directory directory)))
    (when session-buffer
      (with-current-buffer session-buffer
        (and
         (boundp 'eat--process)
         eat--process
         (process-live-p eat--process))))))

(defun enkan-repl--send-text (text &optional directory)
  "Send TEXT to eat session buffer.
If DIRECTORY is provided, send to eat session in that directory.
Otherwise, use current `default-directory'."
  (let
    ((session-buffer
       (enkan-repl--get-buffer-for-directory directory)))
    (when
        (and session-buffer
          (with-current-buffer session-buffer
            (and (boundp 'eat--process)
              eat--process
              (process-live-p eat--process))))
      (with-current-buffer session-buffer
        (eat--send-string eat--process text)
        (eat--send-string eat--process "\r")
        ;; Move cursor to bottom after eat processes the output
        (run-at-time 0.01 nil
          (lambda (buf)
            (with-current-buffer buf
              (goto-char (point-max))))
          session-buffer)
        t))))

(defun enkan-repl--send-numbered-choice (number)
  "Send NUMBER as string to eat session buffer for numbered choice prompt.
NUMBER should be a string (e.g., \\='1\\=', \\='2\\=', \\='3\\=') or empty string for enter."
  (let
      ((target-dir (enkan-repl--get-target-directory-for-buffer)))
    (if
        (enkan-repl--can-send-text target-dir)
        (progn
          (enkan-repl--send-text number target-dir)
          (if
              (= (length number) 0)
              (message "Sent enter to session")
            (message "Sent '%s' to session" number)))
      (message "❌ Cannot send - no matching eat session found for this directory"))))

(defun enkan-repl--send-escape-directly ()
  "Send ESC key to eat session buffer directly."
  (let
      ((session-buffer
        (enkan-repl--get-buffer-for-directory
         (enkan-repl--get-target-directory-for-buffer))))
    (if
        (and session-buffer
             (with-current-buffer session-buffer
               (and (boundp 'eat--process)
                    eat--process
                    (process-live-p eat--process))))
        (progn
          (with-current-buffer session-buffer
            (eat--send-string eat--process "\e")
            ;; Move cursor to bottom after eat processes the output
            (run-at-time 0.01 nil
                         (lambda (buf)
                           (with-current-buffer buf
                             (goto-char (point-max))))
                         session-buffer))
          (message "Sent ESC to session"))
      (message "❌ Cannot send - no matching eat session found for this directory"))))

(defun enkan-repl--send-buffer-content (start end &optional target-directory)
  "Send buffer content from START to END.
START and END define the region to send.
TARGET-DIRECTORY specifies target directory for eat session."
  (let* ((raw-content (buffer-substring-no-properties start end))
         (content (enkan-repl--sanitize-content raw-content))
         (target-dir (or target-directory (enkan-repl--get-target-directory-for-buffer))))
    (if (and content (not (= (length content) 0)))
        (progn
          (if (enkan-repl--send-text content target-dir)
            (message "We can send a message!")
            (message "❌ Cannot send - no matching eat session found for this directory")))
      (message "No content to send (empty or whitespace only)"))))

;;;; Public API - Send Functions

;;;###autoload
(defun enkan-repl-send-region (start end)
  "Send the text in region from START to END to eat session.

Category: Text Sender"
  (interactive "r")
  (when (use-region-p)
    (enkan-repl--send-buffer-content start end)))

;;;###autoload
(defun enkan-repl-send-line ()
  "Send the current line to eat session.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-buffer-content
   (line-beginning-position) (line-end-position)))

;;;###autoload
(defun enkan-repl-send-enter ()
  "Send enter key to eat session buffer.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice ""))

;;;###autoload
(defun enkan-repl-send-1 ()
  "Send \\='1\\=' to eat session buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "1"))

;;;###autoload
(defun enkan-repl-send-2 ()
  "Send \\='2\\=' to eat session buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "2"))

;;;###autoload
(defun enkan-repl-send-3 ()
  "Send \\='3\\=' to eat session buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "3"))

;;;###autoload
(defun enkan-repl-send-4 ()
  "Send \\='4\\=' to eat session buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "4"))

;;;###autoload
(defun enkan-repl-send-5 ()
  "Send \\='5\\=' to eat session buffer for numbered choice prompt.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-numbered-choice "5"))

;;;###autoload
(defun enkan-repl-send-escape ()
  "Send ESC key to eat session buffer.

Category: Text Sender"
  (interactive)
  (enkan-repl--send-escape-directly))

;;;###autoload
(defun enkan-repl-recenter-bottom ()
  "Move cursor to bottom of eat buffer for current directory.

Category: Utilities"
  (interactive)
  (let* ((target-dir (enkan-repl--get-target-directory-for-buffer))
         (session-buffer (enkan-repl--get-buffer-for-directory target-dir))
         (original-window (selected-window)))
    (if session-buffer
        (let ((window (get-buffer-window session-buffer)))
          (if window
              ;; If window is visible, switch to it and move cursor
              (progn
                (select-window window)
                (with-current-buffer session-buffer
                  (goto-char (point-max))
                  (recenter -1))
                ;; Return to original window
                (select-window original-window)
                (message "Cursor moved to bottom in eat buffer"))
            ;; If not visible, just move cursor without recenter
            (with-current-buffer session-buffer
              (goto-char (point-max))
              (message "Cursor moved to bottom in eat buffer (not visible)"))))
      (message "No eat session found for directory: %s" target-dir))))

(defun enkan-repl--create-project-input-file (target-directory)
  "Create project input file for TARGET-DIRECTORY from template.
Returns the created file path."
  (let*
      ((file-path
        (enkan-repl--get-project-file-path target-directory))
       (template-content
        (enkan-repl--load-template)))
    ;; Ensure target directory exists
    (unless (file-exists-p (file-name-directory file-path))
      (make-directory (file-name-directory file-path) t))
    ;; Write template content to target location
    (with-temp-file file-path
      (insert template-content))
    file-path))

;;;###autoload
(defun enkan-repl-open-project-input-file (&optional directory)
  "Open or create project input file for DIRECTORY.
If DIRECTORY is nil, use current `default-directory'.
If project input file exists, open it directly.
If not exists, create from template then open.

Category: Utilities"
  (interactive)
  (let*
      ((target-dir
        (or directory default-directory))
       (file-path
        (enkan-repl--get-project-file-path target-dir)))
    ;; Create file if it doesn't exist
    (unless
        (file-exists-p file-path)
      (enkan-repl--create-project-input-file target-dir))
    ;; Open the file
    (let ((buffer (find-file file-path)))
      (with-current-buffer buffer
        ;; Set up org mode
        (when (fboundp 'org-mode)
          (unless (eq major-mode 'org-mode)
            (let ((org-mode-hook nil))
              (ignore org-mode-hook)  ; Suppress unused variable warning
              (org-mode)))))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (message "Project input file ready: %s" (file-name-nondirectory file-path)))))

(defun enkan-repl--get-session-info ()
  "Get session information for current buffer.
Returns (target-dir existing-buffer can-send) as a list."
  (let ((target-dir (enkan-repl--get-target-directory-for-buffer)))
    (let ((existing-buffer (enkan-repl--get-buffer-for-directory target-dir)))
      (let ((can-send (enkan-repl--can-send-text target-dir)))
        (list target-dir existing-buffer can-send)))))

(defun enkan-repl--handle-dead-session (existing-buffer target-dir prompt-format restart-func)
  "Handle dead session with user confirmation.
EXISTING-BUFFER is the dead buffer to handle.
TARGET-DIR is the target directory.
PROMPT-FORMAT is the format string for `y-or-n-p' prompt.
RESTART-FUNC is a zero-argument function to call for restart.
  When nil, only cleanup is performed (buffer is killed).
  When provided, the function is called after cleanup for restart."
  (when (y-or-n-p (format prompt-format target-dir))
    (kill-buffer existing-buffer)
    (if restart-func
        (funcall restart-func)
      (message "Removed dead eat session buffer in: %s" target-dir))))

;;;###autoload
(defun enkan-repl-start-eat (&optional force)
  "Start eat terminal emulator session.
Determines directory from current buffer filename if it's a persistent file.
Checks for existing sessions to prevent double startup.

When FORCE is non-nil, automatically restart dead sessions without user confirmation.

Category: Session Controller"
  (interactive)
  ;; Try to load eat, checking various package managers
  (unless (or (require 'eat nil t)
              (and (fboundp 'straight-use-package)
                   (progn
                     (straight-use-package 'eat)
                     (require 'eat nil t)))
              (and (fboundp 'package-installed-p)
                   (package-installed-p 'eat)
                   (require 'eat nil t)))
    (error "Eat package is not installed.  Please install it first"))
  (let* ((session-info (enkan-repl--get-session-info))
         (target-dir (nth 0 session-info))
         (existing-buffer (nth 1 session-info))
         (can-send (nth 2 session-info))
         (buffer-name (concat "*enkan:" (expand-file-name target-dir) "*")))
    (cond
     ;; Active session already exists
     (can-send
      (message "Eat session already running in: %s (buffer: %s)"
               target-dir (buffer-name existing-buffer)))
     ;; Dead session exists - offer to restart
     (existing-buffer
      (if force
          ;; Force restart without user confirmation
          (progn
            (kill-buffer existing-buffer)
            (enkan-repl-start-eat))
        ;; Ask user for confirmation
        (enkan-repl--handle-dead-session
         existing-buffer target-dir
         "Dead eat session found in %s. Restart? "
         #'enkan-repl-start-eat)))
     ;; No existing session - start new one
     (t
      (let ((default-directory target-dir)
            (window-count (length (window-list)))
            (new-buffer-name buffer-name)  ; Store buffer-name in a clear local variable
            (eat-buffer nil)
            (target-window nil))
        ;; Determine target window first
        (cond
         ;; Single window: will split and use new window
         ((= window-count 1)
          (split-window-right)
          (other-window 1)
          (setq target-window (selected-window)))
         ;; Two windows: use the right window (like setup-window-layout)
         ((= window-count 2)
          ;; Move to the other window
          (other-window 1)
          (setq target-window (selected-window)))
         ;; Three or more windows: split current window
         (t
          (split-window-right)
          (other-window 1)
          (setq target-window (selected-window))))
        ;; Create eat buffer in target window (already selected)
        (setq eat-buffer (eat))
        (when eat-buffer
          ;; Safely rename the eat buffer with the stored name
          (with-current-buffer eat-buffer
            (condition-case err
                (rename-buffer new-buffer-name t)
              (error
               (message "Warning: Failed to rename eat buffer: %s" (error-message-string err))
               nil))
            ;; Setup bell handler for Claude Code notifications
            (when (and (eq system-type 'darwin)
                       (fboundp 'enkan-repl-setup-bell-handler))
              (run-at-time 0.5 nil #'enkan-repl-setup-bell-handler)))
          ;; Ensure buffer is displayed in target window
          (set-window-buffer target-window eat-buffer))
        ;; Return to original window (input file)
        (other-window -1)
        (message "Started eat session in: %s" target-dir))))))

;;;###autoload
(defun enkan-repl-finish-eat ()
  "Terminate eat session and close its buffer.
Determines directory from current buffer filename if it's a persistent file.

Category: Session Controller"
  (interactive)
  (let* ((session-info (enkan-repl--get-session-info))
         (target-dir (nth 0 session-info))
         (existing-buffer (nth 1 session-info))
         (can-send (nth 2 session-info)))
    (cond
     ;; No session found
     ((not existing-buffer)
      (message "No eat session found for directory: %s" target-dir))
     ;; Active or dead session - terminate
     (t
      (when (or (not can-send)
                (y-or-n-p (format "Terminate eat session in %s? " target-dir)))
        (kill-buffer existing-buffer)
        (message "Terminated eat session in: %s" target-dir))))))

;;;###autoload
(defun enkan-repl-revive-current-buffer ()
  "Revive the current enkan buffer by restarting its eat process.
This function can restore functionality to dead enkan buffers."
  (interactive)
  (let ((buffer-name (buffer-name)))
    ;; Check if this is an enkan buffer
    (unless (string-match "^\\*enkan:" buffer-name)
      (error "Not an enkan buffer: %s" buffer-name))
    ;; Simply force restart the eat session
    (enkan-repl-start-eat t)
    (message "Revived enkan buffer: %s" buffer-name)))


;;;###autoload
(defun enkan-repl-setup ()
  "Set up window layout based on current multi-project configuration or default layout.
For multi-project layouts, calls appropriate examples layout function.
For simple layouts, sets up org file on left and eat on right.

Category: Session Controller"
  (interactive)
  (if enkan-repl--current-multi-project-layout
    ;; Multi-project layout is active - call appropriate examples layout function
    (let* ((alias-list (cdr (assoc enkan-repl--current-multi-project-layout
                              enkan-repl-center-multi-project-layouts)))
            (session-count (length alias-list)))
      (delete-other-windows)
      ;; Open center file first
      (when enkan-repl-center-file
        (find-file enkan-repl-center-file))
      ;; Call appropriate layout function from examples
      (cond
        ((= session-count 1)
          (if (fboundp 'enkan-repl-setup-1session-layout)
            (enkan-repl-setup-1session-layout)
            (error "enkan-repl-setup-1session-layout not available. Load center-window-navigation.el from examples.")))
        ((= session-count 2)
          (if (fboundp 'enkan-repl-setup-2session-layout)
            (enkan-repl-setup-2session-layout)
            (error "enkan-repl-setup-2session-layout not available. Load center-window-navigation.el from examples.")))
        ((= session-count 3)
          (if (fboundp 'enkan-repl-setup-3session-layout)
            (enkan-repl-setup-3session-layout)
            (error "enkan-repl-setup-3session-layout not available. Load center-window-navigation.el from examples.")))
        ((= session-count 4)
          (if (fboundp 'enkan-repl-setup-4session-layout)
            (enkan-repl-setup-4session-layout)
            (error "enkan-repl-setup-4session-layout not available. Load center-window-navigation.el from examples.")))
        (t (error "No layout function available for %d sessions. Consider implementing enkan-repl-setup-%dsession-layout." session-count session-count)))
      (message "Multi-project window layout setup complete: %s (%d sessions)"
        enkan-repl--current-multi-project-layout session-count))
    ;; Normal setup behavior
    (let ((target-dir (enkan-repl--get-target-directory-for-buffer)))
      (delete-other-windows)
      (split-window-right)
      (other-window 1)
      (let ((session-buf (enkan-repl--get-buffer-for-directory target-dir)))
        (if session-buf
          (switch-to-buffer session-buf)
          (message "eat session buffer not found. Run (enkan-repl-start-eat) first.")))
      (other-window -1)
      (message "Window layout setup complete"))))

;;;###autoload
(defun enkan-repl-output-template ()
  "Output current template content to a new buffer for customization.

Category: Utilities"
  (interactive)
  (let*
      ((template-content
        (enkan-repl--load-template))
       (template-file
        (or enkan-repl-template-file "default"))
       (buffer-name
        (format "*enkan-repl-template-%s*"
                (file-name-base template-file))))
    (if template-content
        (progn
          (with-output-to-temp-buffer buffer-name
            (princ template-content))
          (with-current-buffer buffer-name
            (when (fboundp 'org-mode)
              (let ((org-mode-hook nil))
                (ignore org-mode-hook)  ; Suppress unused variable warning
                (org-mode))))
          (message "Template output to buffer: %s" buffer-name)
          t)
      (message "Template not found: %s" template-file)
      nil)))

;;; Debug and Utility Functions

;; These functions are primarily for debugging and troubleshooting.
;; They can be safely removed in production environments if needed.

;;;###autoload
(defun enkan-repl-status ()
  "Show detailed diagnostic information for troubleshooting connection issues.

Category: Utilities"
  (interactive)
  (let*
      ((target-dir (enkan-repl--get-target-directory-for-buffer))
       (session-buffer (enkan-repl--get-buffer-for-directory target-dir))
       (can-send (enkan-repl--can-send-text target-dir))
       (expected-session (concat "*enkan:" (expand-file-name target-dir) "*"))
       (all-buffers (buffer-list))
       (enkan-sessions
        (seq-filter
         (lambda (buf)
           (string-match-p "^\\*enkan:" (buffer-name buf)))
         all-buffers)))
    (with-output-to-temp-buffer
        "*enkan-repl-diagnostic*"
      (princ "═══ enkan-repl Diagnostic Report ═══\n\n")
      ;; Basic information
      (princ (format "Target directory: %s\n" target-dir))
      (princ (format "Current buffer: %s\n\n" (buffer-name)))
      ;; Session status
      (if
          can-send
          (princ
           (format "[✅] Session status: CONNECTED\n  Active session: %s\n\n"
                   (buffer-name session-buffer)))
        (princ "[❌] Session status: NO MATCHING SESSION FOUND\n\n"))
      ;; Target directory analysis
      (princ "Target directory analysis:\n")
      (princ (format "  Expected session: %s\n" expected-session))
      (if
          (get-buffer expected-session)
          (let
              ((process-status
                (with-current-buffer expected-session
                  (if
                      (and (boundp 'eat--process)
                           eat--process
                           (process-live-p eat--process))
                      "alive" "dead"))))
            (princ (format "  → Session exists (process: %s)\n\n" process-status)))
        (princ "  → This session does not exist\n\n"))
      ;; Path encoding check
      (if
          (and
           (buffer-file-name)
           (string-match-p "enkan-.+\\.org$" (file-name-nondirectory (buffer-file-name))))
          (let*
              ((encoded-name (file-name-base (file-name-nondirectory (buffer-file-name))))
               (decoded-path (enkan-repl--decode-full-path encoded-name))
               (re-encoded (enkan-repl--encode-full-path decoded-path)))
            (princ "[✅] Path encoding: OK\n")
            (princ (format "  File: %s\n" (file-name-nondirectory (buffer-file-name))))
            (princ (format "  → Decoded to: %s\n" decoded-path))
            (if (string= encoded-name re-encoded)
                (princ "  → Re-encoding: ✓ Consistent\n")
              (princ (format "  → Re-encoding: ⚠️  Inconsistent (%s)\n" re-encoded)))
            (when (string-match-p "--.*--" (file-name-nondirectory (buffer-file-name)))
              (princ "  ⚠️  Warning: Path contains multiple '--' which may cause parsing issues\n"))
            (princ "\n"))
        (princ "[ℹ️] Path encoding: Not applicable (not a persistent file)\n\n"))
      ;; Found sessions
      (princ "Found enkan sessions:\n")
      (if enkan-sessions
          (dolist (buf enkan-sessions)
            (let*
                ((name (buffer-name buf))
                 (process-status
                  (with-current-buffer buf
                    (if
                        (and (boundp 'eat--process)
                             eat--process
                             (process-live-p eat--process))
                        "alive" "dead"))))
              (princ (format "  - %s (process: %s)\n" name process-status))))
        (princ "  (no enkan sessions found)\n"))
      (princ "\n")
      ;; Recommended actions
      (princ "Recommended actions:\n")
      (cond
       (can-send
        (princ "✓ Connection is working. You can send text to session.\n"))
       ((not enkan-sessions)
        (princ "1. Start eat session in target directory: (enkan-repl-start-eat)\n")
        (princ "2. Or run: M-x enkan-repl-start-eat\n"))
       (t
        (princ "1. Start eat session for target directory: (enkan-repl-start-eat)\n")
        (princ "2. Or switch to existing session directory:\n")
        (dolist (buf enkan-sessions)
          (let
              ((session-path
                (replace-regexp-in-string
                 "^\\*enkan:\\(.*\\)\\*$" "\\1"
                 (buffer-name buf))))
            (princ (format "   - %s\n" session-path))))))
      (princ "\nFor more help: M-x enkan-repl-open-project-input-file\n"))))

(defun enkan-repl--get-buffer-info-list ()
  "Get buffer info list for all buffers (side-effect: reads `buffer-list')."
  (mapcar
   (lambda (buf)
     (let ((name (buffer-name buf))
           (live-p (buffer-live-p buf))
           (process-info (with-current-buffer buf
                           (cons (boundp 'eat--process)
                                 (and (boundp 'eat--process)
                                      eat--process
                                      (process-live-p eat--process))))))
       (list :name name
             :live-p live-p
             :has-eat-process (car process-info)
             :process-live-p (cdr process-info)
             :buffer buf)))
   (buffer-list)))


;;;###autoload
(defun enkan-repl-list-sessions ()
  "Display a list of active enkan sessions with interactive selection.
Users can select a session, then choose action: (s)witch, (d)elete, or (q)uit.

Category: Session Controller"
  (interactive)
  (require 'enkan-repl-utils)
  (let* ((buffer-info-list (enkan-repl--get-buffer-info-list))
         (sessions (enkan-repl--collect-sessions-pure buffer-info-list)))
    (if (null sessions)
        (message "No active sessions found")
      ;; Prepare candidates for completing-read with annotations
      (let* ((candidates (mapcar
                          (lambda (session)
                            (plist-get session :name))
                          sessions))
             (completion-extra-properties
              `(:annotation-function
                (lambda (candidate)
                  (let ((session (cl-find-if
                                  (lambda (s)
                                    (string= (plist-get s :name) candidate))
                                  ',sessions)))
                    (when session
                      (format " — Directory: %s, Status: %s"
                              (plist-get session :directory)
                              (plist-get session :status))))))))
        ;; Get session selection using completing-read (like cheat-sheet)
        (let ((selected-name (completing-read "Select enkan session: " candidates nil t)))
          (when (and selected-name (not (string= selected-name "")))
            (let ((buf (enkan-repl--find-session-buffer-pure selected-name buffer-info-list)))
              (when buf
                ;; Ask for action after selection
                (let ((action (read-char-choice
                               (format "Session %s - (s)witch, (d)elete, (q)uit: " selected-name)
                               '(?s ?d ?q))))
                  (cl-case action
                    (?s
                     (switch-to-buffer buf)
                     (message "Switched to session: %s" selected-name))
                    (?d
                     (when (y-or-n-p (format "Delete session %s? " selected-name))
                       (kill-buffer buf)
                       (message "Session deleted: %s" selected-name)))
                    (?q
                     (message "Cancelled"))))))))))))

;;; Interactive Cheat-sheet Feature

;;;###autoload
(defun enkan-repl-cheat-sheet ()
  "Display interactive cheat-sheet for enkan-repl commands.

Category: Command Palette"
  (interactive)
  (unless (featurep 'enkan-repl-constants)
    (require 'enkan-repl-constants))
  (let ((candidates enkan-repl-cheat-sheet-candidates))
    (let ((completion-extra-properties
           `(:annotation-function
             (lambda (candidate)
               (let ((description (alist-get candidate ',candidates nil nil #'string=)))
                 (when description
                   (format " — %s" description)))))))
      (let ((selected-command (completing-read "enkan-repl commands: " candidates)))
        (when selected-command
          (call-interactively (intern selected-command)))))))

;;;; Global Minor Mode for Center File Operations

(defvar enkan-center-file-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") 'enkan-repl-center-send-escape)
    (define-key map (kbd "C-c C-f") 'enkan-toggle-center-file-global-mode)
    (define-key map (kbd "C-x g") 'enkan-repl-center-magit)
    (define-key map (kbd "C-M-e") 'enkan-repl-center-send-enter)
    (define-key map (kbd "C-M-i") 'enkan-repl-center-send-line)
    (define-key map (kbd "C-M-<return>") 'enkan-repl-center-send-region)
    (define-key map (kbd "C-M-@") 'enkan-repl-center-open-project-directory)
    (define-key map (kbd "C-M-t") 'other-window)
    (define-key map (kbd "C-M-b") 'enkan-repl-center-recenter-bottom)
    (define-key map (kbd "C-M-s") 'enkan-repl-center-auto-setup)
    (define-key map (kbd "C-M-f") 'enkan-repl-center-finish-all-sessions)
    map)
  "Global keymap for center file operations.")

;; Store original keybindings for safe restoration
(defvar enkan-center-file-original-keybindings nil
  "Stores original keybindings before center file mode overrides them.")

(define-minor-mode enkan-center-file-global-mode
  "Global minor mode for center file operations.
When enabled, center file keybindings are available across all buffers."
  :init-value nil
  :global t
  :lighter " ECF"
  :keymap enkan-center-file-global-mode-map
  ;; Do nothing dangerous to global keymap - let minor mode keymap handle it
  ;; This avoids overriding critical keybindings like M-x
  (message (if enkan-center-file-global-mode
               "✅ Center file global mode enabled"
             "❌ Center file global mode disabled")))

;;;###autoload
(defun enkan-toggle-center-file-global-mode ()
  "Toggle center file global mode on/off."
  (interactive)
  (enkan-center-file-global-mode 'toggle))

;;;; Auto Setup Functions

(defun enkan-repl--get-project-info-from-registry (alias)
  "Get project info from registry for ALIAS.
Return (project-name . project-path) or nil if not found."
  (cdr (assoc alias enkan-repl-center-project-registry)))

(defun enkan-repl--get-session-project-name (session-number session-list)
  "Pure function to get project name from session number."
  (cdr (assoc session-number session-list)))

(defun enkan-repl--get-project-path-from-registry (project-name project-registry)
  "Pure function to get project path from registry by project name."
  (let ((project-info (cl-find-if (lambda (entry)
                                    (string= (car (cdr entry)) project-name))
                                  project-registry)))
    (when project-info
      (cdr (cdr project-info)))))

(defun enkan-repl--get-session-project-paths (session-numbers session-list project-registry)
  "Pure function to get list of project paths for multiple session numbers.
Returns: list of (session-number . project-path) for valid paths"
  (cl-loop for session-number in session-numbers
           for project-name = (enkan-repl--get-session-project-name session-number session-list)
           for project-path = (when project-name
                                (enkan-repl--get-project-path-from-registry project-name project-registry))
           when project-path
           collect (cons session-number project-path)))

(defun enkan-repl--setup-project-session (alias session-number)
  "Setup project session for given ALIAS and SESSION-NUMBER.
Implemented as pure function, side effects are handled by upper functions."
  (let ((project-info (enkan-repl--get-project-info-from-registry alias)))
    (if project-info
        (let ((project-name (car project-info))
              (project-path (cdr project-info)))
          (cons project-name project-path))
      (error "Project alias '%s' not found in registry" alias))))

;;;###autoload
(defun enkan-repl-center-auto-setup (layout-name)
  "Auto start eat sessions using multi-project layout configuration.
LAYOUT-NAME is the configuration name defined in enkan-repl-center-multi-project-layouts.
This function only starts eat sessions - use enkan-repl-setup (C-M-l) to arrange windows."
  (interactive
   (list (completing-read "Layout: "
                          (mapcar #'car enkan-repl-center-multi-project-layouts))))
  (let ((buffer-name "*ENKAN-REPL Auto Setup*")
        (old-layout enkan-repl--current-multi-project-layout)
        (old-session-list (copy-tree enkan-repl-session-list))
        (old-counter enkan-repl--session-counter))
    (with-output-to-temp-buffer buffer-name
      (princ (format "=== ENKAN-REPL AUTO SETUP: %s ===\n\n" layout-name))
      ;; Enable global center file mode automatically
      (unless enkan-center-file-global-mode
        (enkan-center-file-global-mode 1)
        (princ "🚀 Auto-enabled center file global mode for multi-project workflow\n\n"))
      ;; Display current state before changes
      (princ (format "🔧 Current state before setup:\n"))
      (princ (format "  Layout (enkan-repl--current-multi-project-layout): %s\n" (or old-layout "nil")))
      (princ (format "  Sessions (enkan-repl-session-list): %s\n" (or old-session-list "nil")))
      (princ (format "  Counter (enkan-repl--session-counter): %d\n\n" old-counter))
      ;; Set the new layout configuration
      (setq enkan-repl--current-multi-project-layout layout-name)
      (let ((alias-list (cdr (assoc layout-name enkan-repl-center-multi-project-layouts))))
        (unless alias-list
          (error "Layout '%s' not found" layout-name))
        (let ((session-count (length alias-list)))
          (delete-other-windows)
          ;; Clear session list and reset any previous layout configuration
          (setq enkan-repl-session-list nil)
          (setq enkan-repl--session-counter 0)
          (setq enkan-repl--current-multi-project-layout nil)
          (princ "🧹 Reset previous configuration\n\n")
          ;; Setup project aliases based on layout configuration
          (let ((project-aliases '()))
            (dolist (alias alias-list)
              (let ((project-info (enkan-repl--get-project-info-from-registry alias)))
                (when project-info
                  (let ((project-name (car project-info)))
                    (push (cons alias project-name) project-aliases)))))
            (setq enkan-repl-project-aliases (nreverse project-aliases)))
          (princ (format "🔧 Setup project aliases (enkan-repl-project-aliases): %s\n\n" enkan-repl-project-aliases))
          ;; Start sessions for each project
          (princ "🚀 Starting eat sessions:\n")
          (let ((session-number 1)) ; Session numbers start from 1
            (dolist (alias alias-list)
              (let ((project-info (enkan-repl--setup-project-session alias session-number)))
                (let ((project-name (car project-info))
                      (project-path (expand-file-name (cdr project-info)))
                      (default-directory (expand-file-name (cdr project-info))))
                  ;; Register session
                  (enkan-repl--register-session session-number project-name)
                  (princ (format "  Session %d: %s (%s)\n" session-number alias project-name))
                  ;; Start eat session in current directory (force restart if needed)
                  (enkan-repl-start-eat t)
                  (setq session-number (1+ session-number))))))
          ;; Set final layout configuration
          (setq enkan-repl--current-multi-project-layout layout-name)
          ;; Display final state
          (princ (format "\n✅ Setup completed!\n"))
          (princ (format "  Layout (enkan-repl--current-multi-project-layout): %s\n" layout-name))
          (princ (format "  Sessions (enkan-repl-session-list): %s\n" enkan-repl-session-list))
          (princ (format "  Counter (enkan-repl--session-counter): %d\n" enkan-repl--session-counter))
          (princ (format "\nEat sessions started for layout: %s (%d sessions).\n" layout-name session-count))
          (princ "Use enkan-repl-setup to arrange windows.\n\n")
          (princ "=== END SETUP ===\n"))))))

(defun enkan-repl-center-finish-all-sessions ()
  "Terminate all registered center file sessions.

Category: Center File Multi-buffer Access"
  (interactive)
  (let ((buffer-name "*ENKAN-REPL Finish Sessions*"))
    (if (null enkan-repl-session-list)
      (message "No registered sessions to terminate")
      (with-output-to-temp-buffer buffer-name
        (princ "=== ENKAN-REPL FINISH ALL SESSIONS ===\n\n")
        ;; Display current state before termination
        (princ "🔧 Current state before termination:\n")
        (princ (format "  Layout (enkan-repl--current-multi-project-layout): %s\n" (or enkan-repl--current-multi-project-layout "nil")))
        (princ (format "  Sessions (enkan-repl-session-list): %s\n" enkan-repl-session-list))
        (princ (format "  Counter (enkan-repl--session-counter): %d\n\n" enkan-repl--session-counter))
        (let ((terminated-count 0))
          (when (y-or-n-p (format "Terminate all %d registered sessions? "
                            (length enkan-repl-session-list)))
            (princ "🚫 Terminating sessions:\n")
            (dolist (session enkan-repl-session-list)
              (let* ((session-number (car session))
                      (project-name (cdr session))
                      ;; Get project directory from registry using correct function
                      (project-path (enkan-repl--get-project-path-from-registry project-name enkan-repl-center-project-registry)))
                (when project-path
                  ;; Use the same buffer discovery method as enkan-repl-finish-eat
                  (let ((buffer (enkan-repl--get-buffer-for-directory project-path)))
                    (if buffer
                      (progn
                        (kill-buffer buffer)
                        (setq terminated-count (1+ terminated-count))
                        (princ (format "  ✅ Session %d: %s (terminated)\n" session-number project-name)))
                      (princ (format "  ⚠️ Session %d: %s (buffer not found)\n" session-number project-name)))))))
            ;; Clear session list and reset session counter
            (setq enkan-repl-session-list nil)
            (setq enkan-repl--session-counter 0)
            (setq enkan-repl--current-multi-project-layout nil)
            (setq enkan-repl-project-aliases nil)
            ;; Auto-disable global center file mode
            (when enkan-center-file-global-mode
              (enkan-center-file-global-mode -1)
              (princ "\n🔄 Auto-disabled center file global mode\n"))
            ;; Display final state
            (princ "\n🧹 Configuration reset:\n")
            (princ "  Session list (enkan-repl-session-list): nil\n")
            (princ "  Session counter (enkan-repl--session-counter): 0\n")
            (princ "  Current layout (enkan-repl--current-multi-project-layout): nil\n")
            (princ "  Project aliases (enkan-repl-project-aliases): nil\n" (or enkan-repl-project-aliases "nil"))
            (princ (format "\n✅ Terminated %d sessions, cleared session list and reset layout configuration.\n" terminated-count))
            (princ "\n=== END FINISH SESSIONS ===\n")))))))

;;;###autoload
(defun enkan-repl-center-recenter-bottom ()
  "Recenter all enkan terminal buffers at bottom from center file.

Category: Center File Multi-buffer Access"
  (interactive)
  (let ((original-window (selected-window))
         (enkan-buffers (seq-filter
                          (lambda (buf)
                            (string-match-p "^\\*enkan:" (buffer-name buf)))
                          (buffer-list)))
         (recentered-count 0))
    (dolist (buffer enkan-buffers)
      (let ((window (get-buffer-window buffer)))
        (when window
          (with-selected-window window
            (goto-char (point-max))
            (recenter -1)
            (setq recentered-count (1+ recentered-count))))))
    ;; Return to original window
    (select-window original-window)
    (if (> recentered-count 0)
      (message "Recentered %d enkan session(s) at bottom" recentered-count)
      (message "No enkan sessions found to recenter"))))

;;;###autoload
(defun enkan-repl--collect-enkan-buffers-pure (buffer-list)
  "Pure function to collect enkan buffers from BUFFER-LIST.
Returns list of buffers whose names match enkan pattern."
  (seq-filter (lambda (buf)
                (and (bufferp buf)
                     (buffer-name buf)
                     (string-match-p "^\\*enkan:" (buffer-name buf))))
              buffer-list))

(defun enkan-repl--get-buffer-process-info-pure (buffer)
  "Pure function to get process info for BUFFER.
Returns plist with :buffer, :name, :live-p, :has-process, :process."
  (when (bufferp buffer)
    (let* ((name (buffer-name buffer))
           (live-p (buffer-live-p buffer))
           (process-info (when live-p
                          (with-current-buffer buffer
                            (list :bound (boundp 'eat--process)
                                  :process (if (boundp 'eat--process) eat--process nil))))))
      (list :buffer buffer
            :name name
            :live-p live-p
            :has-process (and process-info (plist-get process-info :bound) (plist-get process-info :process))
            :process (when process-info (plist-get process-info :process))))))

(defun enkan-repl--filter-valid-buffers-pure (enkan-buffers)
  "Pure function to filter buffers that have active eat processes.
ENKAN-BUFFERS is a list of buffer objects.
Returns list of buffers that have live eat processes."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (boundp 'eat--process)
                       eat--process
                       (process-live-p eat--process))))
              enkan-buffers))

(defun enkan-repl--build-buffer-selection-choices-pure (buffers)
  "Pure function to build selection choices from BUFFERS.
Returns list of cons cells (display-name . buffer) for selection UI."
  (mapcar (lambda (buffer)
            (let ((info (enkan-repl--get-buffer-process-info-pure buffer)))
              (cons (format "%s%s"
                           (plist-get info :name)
                           (if (plist-get info :has-process) " [ACTIVE]" " [INACTIVE]"))
                    buffer)))
          buffers))


(defun enkan-repl--get-buffer-by-index-pure (buffers index)
  "Pure function to get buffer from BUFFERS by INDEX (1-based).
Returns buffer if valid index, nil otherwise."
  (when (and (integerp index)
             (> index 0)
             (<= index (length buffers)))
    (nth (1- index) buffers)))

(defun enkan-repl--parse-prefix-arg-pure (prefix-arg)
  "Pure function to parse PREFIX-ARG into action type.
Returns plist with :action and :index."
  (cond
   ((null prefix-arg)
    (list :action 'select :index nil))
   ((integerp prefix-arg)
    (list :action 'index :index prefix-arg))
   (t
    (list :action 'invalid :index nil))))

(defun enkan-repl--should-show-buffer-selection-pure (action-type valid-buffers)
  "Pure function to determine if buffer selection should be shown.
Returns t if selection UI needed, nil for direct index access."
  (and (eq action-type 'select)
       (> (length valid-buffers) 0)))

(defun enkan-repl--send-number-to-buffer-pure (number buffer)
  "Pure function to determine parameters for sending NUMBER to BUFFER.
Returns plist with :can-send, :number, :buffer, :message."
  (let ((info (enkan-repl--get-buffer-process-info-pure buffer)))
    (list :can-send (plist-get info :has-process)
          :number number
          :buffer buffer
          :message (if (plist-get info :has-process)
                      (format "Will send '%s' to %s" number (plist-get info :name))
                    (format "Cannot send to inactive buffer: %s" (plist-get info :name))))))

(defun enkan-repl--validate-number-input-pure (number)
  "Pure function to validate NUMBER input for sending.
Returns plist with :valid, :number, :message."
  (cond
   ((null number)
    (list :valid nil :number nil :message "Number is required"))
   ((not (stringp number))
    (list :valid nil :number nil :message "Number must be a string"))
   ((string-empty-p number)
    (list :valid nil :number nil :message "Number cannot be empty"))
   ((not (string-match-p "^[0-9]$" number))
    (list :valid nil :number nil :message "Number must be a single digit (0-9)"))
   (t
    (list :valid t :number number :message (format "Valid number: %s" number)))))

(defun enkan-repl--center-send-text-to-buffer (text buffer)
  "Center file specific function to send TEXT to BUFFER.
Sends text followed by carriage return, with cursor positioning."
  (when (and (bufferp buffer)
             (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (boundp 'eat--process)
                    eat--process
                    (process-live-p eat--process))))
    (with-current-buffer buffer
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\r")
      ;; Move cursor to bottom after eat processes the output
      (run-at-time 0.01 nil
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (goto-char (point-max)))))
                   buffer)
      t)))

;;;###autoload
(defun enkan-repl-center-send-escape (&optional prefix-arg)
  "Send ESC key to eat session buffer from center file or current enkan buffer.
- If called from enkan buffer: Send ESC to current buffer
- If called from center file without prefix: Select from available enkan buffers
- With numeric prefix: Send to buffer at that index (1-based)

Category: Center File Multi-buffer Access"
  (interactive "P")
  (cond
   ;; Check if current buffer is an enkan buffer
   ((string-match-p "^\\*enkan:" (buffer-name))
    ;; Send ESC to current enkan buffer directly
    (enkan-repl--send-escape-to-buffer (current-buffer) nil))
   ((numberp prefix-arg)
    ;; C-u numeric for layout index specification
    (let* ((current-layout (cdr (assoc enkan-repl--current-multi-project-layout enkan-repl-center-multi-project-layouts)))
           (layout-count (length current-layout)))
      (if (and (<= 1 prefix-arg) (<= prefix-arg layout-count))
          (let* ((target-alias (nth (1- prefix-arg) current-layout))
                 (enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
                 (resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure target-alias enkan-buffers)))
            (message "Sending ESC to layout index %d (alias: %s)" prefix-arg target-alias)
            (if resolved-buffer
                (enkan-repl--send-escape-to-buffer resolved-buffer nil)
              (message "No buffer found for alias '%s'" target-alias)))
        (message "Invalid layout index %d (valid range: 1-%d)" prefix-arg layout-count))))
   (t
    ;; No argument case shows buffer selection UI
    (enkan-repl--center-send-escape-internal nil))))

(defun enkan-repl--center-send-escape-internal (&optional buffer-index-or-skip-ui)
  "Internal helper to send ESC key to eat session buffer from center file.
BUFFER-INDEX-OR-SKIP-UI:
- nil: Show buffer selection UI.
- integer: Send to buffer at that index (1-based).
- 'skip-ui: Skip buffer selection UI and send to default/first active buffer."
  (message "Starting enkan-repl--center-send-escape-internal with arg: %s" buffer-index-or-skip-ui)
  (let* ((enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
         (valid-buffers (enkan-repl--filter-valid-buffers-pure enkan-buffers)))
    (message "Found %d enkan buffers, %d valid"
             (length enkan-buffers) (length valid-buffers))
    (cond
     ((= (length valid-buffers) 0)
      (message "No active enkan sessions found"))
     ((integerp buffer-index-or-skip-ui)
      (let* ((index buffer-index-or-skip-ui)
             (target-buffer (enkan-repl--get-buffer-by-index-pure valid-buffers index)))
        (message "Attempting to send ESC to buffer index %d" index)
        (if target-buffer
            (enkan-repl--send-escape-to-buffer target-buffer index)
          (message "Invalid buffer index %d (valid range: 1-%d)" index (length valid-buffers)))))
     ((eq 'skip-ui buffer-index-or-skip-ui)
      (if valid-buffers
          (enkan-repl--send-escape-to-buffer (car valid-buffers) 1)
        (message "No active enkan sessions found to send ESC directly.")))
     (t
      (let* ((choices (enkan-repl--build-buffer-selection-choices-pure valid-buffers))
             (selected-display (completing-read "Select buffer to send ESC: " choices nil t))
             (selected-buffer (cdr (assoc selected-display choices))))
        (when selected-buffer
          (message "User selected buffer: %s" (buffer-name selected-buffer))
          (enkan-repl--send-escape-to-buffer selected-buffer nil)))))))

(defun enkan-repl--send-escape-to-buffer (buffer &optional index)
  "Send ESC key to BUFFER. INDEX is for logging purposes."
  (let ((info (enkan-repl--get-buffer-process-info-pure buffer)))
    (with-current-buffer buffer
      (eat--send-string (plist-get info :process) "\e"))
    (message "Sent ESC to buffer %s: %s" (if index (format "%d" index) "selected") (plist-get info :name))))

;;;###autoload
(defun enkan-repl-center-open-project-directory ()
  "Open project directory in dired from current multi-project layout.

Category: Center File Multi-buffer Access"
  (interactive)
  (if enkan-repl--current-multi-project-layout
    (let* ((current-layout (cdr (assoc enkan-repl--current-multi-project-layout enkan-repl-center-multi-project-layouts)))
            (project-choices '()))
      (if current-layout
        (progn
          ;; Build choices list with alias and directory
          (dolist (alias current-layout)
            (let ((project-info (enkan-repl--get-project-info-from-registry alias)))
              (when project-info
                (let ((project-name (car project-info))
                       (project-path (cdr project-info)))
                  (push (cons (format "%s (%s)" alias project-path) project-path) project-choices)))))
          (if project-choices
            (let* ((selected-display (completing-read "Select project directory to open: " project-choices nil t))
                    (selected-path (cdr (assoc selected-display project-choices))))
              (if (file-directory-p selected-path)
                (dired selected-path)
                (message "Directory does not exist: %s" selected-path)))
            (message "No valid project directories found in layout")))
        (message "Current layout '%s' not found in configurations" enkan-repl--current-multi-project-layout)))
    (message "No multi-project layout is currently active")))

;;;###autoload
(defun enkan-repl-center-send-enter (&optional prefix-arg)
  "Send enter key to eat session buffer from center file.
Always requires buffer specification:
- Without prefix: Select from available enkan buffers
- With numeric prefix: Send to buffer at that index (1-based)

Category: Center File Multi-buffer Access"
  (interactive "P")
  (enkan-repl--center-send-text-with-selection "" prefix-arg))

(defun enkan-repl--center-send-text-with-selection (text prefix-arg)
  "Internal function to send TEXT with buffer selection logic using PREFIX-ARG."
  (message "Starting enkan-repl-center-send-text with prefix-arg: %s" prefix-arg)
  (let* ((enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
         (valid-buffers (enkan-repl--filter-valid-buffers-pure enkan-buffers))
         (parsed-arg (enkan-repl--parse-prefix-arg-pure prefix-arg)))
    (message "Found %d enkan buffers, %d valid for text sending"
             (length enkan-buffers) (length valid-buffers))
    (cond
     ((= (length valid-buffers) 0)
      (message "No active enkan sessions found"))
     ((eq 'invalid (plist-get parsed-arg :action))
      (message "Invalid prefix argument"))
     ((eq 'index (plist-get parsed-arg :action))
      (let* ((index (plist-get parsed-arg :index))
             (target-buffer (enkan-repl--get-buffer-by-index-pure valid-buffers index)))
        (message "Attempting to send text '%s' to buffer index %d" text index)
        (if target-buffer
            (let ((info (enkan-repl--get-buffer-process-info-pure target-buffer)))
              (if (plist-get info :has-process)
                  (progn
                    (enkan-repl--center-send-text-to-buffer text target-buffer)
                    (message "Sent '%s' to buffer %d: %s" text index (plist-get info :name)))
                (message "Cannot send to inactive buffer %d: %s" index (plist-get info :name))))
          (message "Invalid buffer index %d (valid range: 1-%d)" index (length valid-buffers)))))
     ((enkan-repl--should-show-buffer-selection-pure (plist-get parsed-arg :action) valid-buffers)
      (let* ((choices (enkan-repl--build-buffer-selection-choices-pure valid-buffers))
             (selected-display (completing-read (format "Select buffer to send '%s': "
                                                        (if (string-empty-p text) "ENTER" text))
                                               choices nil t))
             (selected-buffer (cdr (assoc selected-display choices))))
        (message "User selected buffer for text '%s': %s" text (buffer-name selected-buffer))
        (let ((info (enkan-repl--get-buffer-process-info-pure selected-buffer)))
          (if (plist-get info :has-process)
              (progn
                (enkan-repl--center-send-text-to-buffer text selected-buffer)
                (message "Sent '%s' to selected buffer: %s" text (plist-get info :name)))
            (message "Cannot send to selected buffer: %s" (plist-get info :name))))))
     (t
      (message "No valid action determined for text sending")))))

(defun enkan-repl--analyze-center-send-content-pure (content prefix-arg)
  "Pure function to analyze center-send content and determine action.
CONTENT is the text content to analyze.
PREFIX-ARG is the numeric prefix argument.
Returns plist with :action and :data."
  (let ((trimmed-content (string-trim content)))
    (cond
     ;; Numeric prefix argument takes priority
     ((and (numberp prefix-arg) (<= 1 prefix-arg 2))
      (list :action 'prefix-number :data prefix-arg))
     ;; Check if content contains only :esc
     ((string= trimmed-content ":esc")
      (list :action 'escape-directly))
     ;; Check if content starts with :alias (colon + word + space)
     ((string-match "^:\\([a-zA-Z0-9_.-]+\\) \\(.*\\)$" trimmed-content)
      ;; This is :alias something format - always use alias-command
      (list :action 'alias-command :data trimmed-content))
     ;; Default behavior
     (t
      (list :action 'default-send :data content)))))

;; Alias command parsing functions

(defun enkan-repl-center--parse-alias-command-pure (input-string)
  "Pure function to parse alias command format: ':alias esc' or ':alias :ret'.
INPUT-STRING is the command string to parse.
Returns plist with :valid, :alias, :command, :text, :message."
  (cond
   ((not (stringp input-string))
    (list :valid nil :message "Input must be a string"))
   ((string-empty-p input-string)
    (list :valid nil :message "Must start with : prefix"))
   ((not (string-match-p "^:" input-string))
    (list :valid nil :message "Must start with : prefix"))
   ((string-match "^:\\([^ ]+\\)\\(?: \\(.*\\)\\)?$" input-string)
    (let* ((alias (match-string 1 input-string))
           (rest (match-string 2 input-string)))
      (if (string-empty-p alias)
          (list :valid nil :message "Invalid format. Alias cannot be empty")
        (cond
         ((null rest)
          ;; Only alias specified
          (list :valid t :alias alias :command :empty :text ""))
         ((equal rest "esc")
          ;; Escape command
          (list :valid t :alias alias :command :esc :text nil))
         ((equal rest ":ret")
          ;; Return command
          (list :valid t :alias alias :command :ret :text nil))
         (t
          ;; Text to send
          (list :valid t :alias alias :command :text :text rest))))))
   (t
    (list :valid nil :message "Invalid format. Use: :alias [text|esc|:ret]"))))

(defun enkan-repl-center--resolve-alias-to-buffer-pure (alias enkan-buffers)
  "Pure function to resolve alias to buffer from ENKAN-BUFFERS.
ALIAS is the alias string to match against buffer names.
ENKAN-BUFFERS is list of available enkan buffers.
Returns buffer object or nil if not found."
  ;; First check if alias exists in project aliases
  (let ((alias-entry (assoc alias enkan-repl-project-aliases)))
    (if alias-entry
        ;; Alias found, resolve to project name and search buffers
        (let* ((resolved-project (cdr alias-entry))
               (matching-buffers (seq-filter
                                 (lambda (buf)
                                   ;; Extract project name from buffer name and do exact match
                                   (let ((buffer-project (enkan-repl--extract-project-name (buffer-name buf))))
                                     (string= resolved-project buffer-project)))
                                 enkan-buffers)))
          (if matching-buffers
              ;; Return first match
              ;; Could be enhanced to show selection UI
              (car matching-buffers)
            nil))
      ;; Alias not found in project aliases
      nil)))

(defun enkan-repl-center-send-line ()
  "Send current line to a suitable eat session.
See enkan-repl-center-send-region.

Category: Text Sender"
  (interactive)
  (enkan-repl-center-send-region
    (line-beginning-position) (line-end-position)))

;; Pure functions for center file operations (used by enkan-repl-center-open-file)
(defun enkan-center-file-validate-path-pure (file-path)
  "Validate center FILE-PATH for opening.
Returns plist with :valid, :message."
  (cond
   ((null file-path)
    (list :valid nil :message "Center file path not configured"))
   ((not (stringp file-path))
    (list :valid nil :message "Center file path must be a string"))
   ((string-empty-p file-path)
    (list :valid nil :message "Center file path is empty"))
   (t
    (list :valid t :message "Valid center file path"))))

(defun enkan-center-file-check-exists-pure (file-path)
  "Check if center FILE-PATH exists.
Returns plist with :exists, :action."
  (if (file-exists-p file-path)
      (list :exists t :action "open")
    (list :exists nil :action "create")))

(defun enkan-center-file-determine-action-pure (file-path)
  "Determine action to take for center FILE-PATH.
Returns plist with :valid, :action, :message."
  (let ((validation (enkan-center-file-validate-path-pure file-path)))
    (if (plist-get validation :valid)
        (let ((exists-check (enkan-center-file-check-exists-pure file-path)))
          (list :valid t
                :action (plist-get exists-check :action)
                :message (if (plist-get exists-check :exists)
                            "Opening existing center file"
                          "Creating new center file")))
      validation)))

(defun enkan-repl-center-open-file ()
  "Open or create the center file based on enkan-repl-center-file configuration.

Category: Center File Operations"
  (interactive)
  (let ((result (enkan-center-file-determine-action-pure enkan-repl-center-file)))
    (if (plist-get result :valid)
        (progn
          (message "%s" (plist-get result :message))
          (find-file enkan-repl-center-file))
      (error "%s" (plist-get result :message)))))

;; Pure functions for magit project selection (used by enkan-repl-center-magit)
(defun enkan-repl--get-magit-project-list-pure (project-registry)
  "Get list of projects for magit selection from PROJECT-REGISTRY.
Returns list of (project-name . project-path) pairs."
  (unless project-registry
    (error "Project registry is empty"))
  (mapcar (lambda (entry)
            (let ((project-name (car entry))
                  (project-path (if (stringp (cdr entry))
                                    (cdr entry)
                                  ;; Handle nested cons structure
                                  (if (consp (cdr entry))
                                      (cdr (cdr entry))
                                    (cdr entry)))))
              (cons project-name (expand-file-name project-path))))
          project-registry))

(defun enkan-repl--validate-magit-project-path-pure (project-path)
  "Validate PROJECT-PATH for magit operation.
Returns plist with :valid, :message."
  (cond
   ((null project-path)
    (list :valid nil :message "Project path is null"))
   ((not (stringp project-path))
    (list :valid nil :message "Project path must be a string"))
   ((not (file-directory-p project-path))
    (list :valid nil :message "Project path does not exist or is not a directory"))
   (t
    (list :valid t :message "Valid project path"))))

(defun enkan-repl--create-magit-completion-list-pure (project-list)
  "Create completion list for magit project selection from PROJECT-LIST.
Returns list of strings for completing-read."
  (unless project-list
    (error "Project list is empty"))
  (mapcar (lambda (entry)
            (format "%s (%s)" (car entry) (cdr entry)))
          project-list))

(defun enkan-repl--parse-selected-magit-project-pure (selected-string project-list)
  "Parse SELECTED-STRING to get project info from PROJECT-LIST.
Returns plist with :project-name, :project-path."
  (let ((matched-entry (cl-find-if
                        (lambda (entry)
                          (string= selected-string
                                   (format "%s (%s)" (car entry) (cdr entry))))
                        project-list)))
    (if matched-entry
        (list :project-name (car matched-entry)
              :project-path (cdr matched-entry))
      (list :project-name nil :project-path nil))))

(defun enkan-repl-center-magit ()
  "Open magit for selected project from active sessions only.

Category: Center File Operations"
  (interactive)
  (let* ((enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
          (valid-buffers (enkan-repl--filter-valid-buffers-pure enkan-buffers)))
    (if (= (length valid-buffers) 0)
      (message "No active enkan sessions found for magit")
      (let* ((choices (enkan-repl--build-buffer-selection-choices-pure valid-buffers))
              (selected-display (completing-read "Select project for magit: " choices nil t))
              (selected-buffer (cdr (assoc selected-display choices))))
        (when selected-buffer
          (let ((project-path (enkan-repl--extract-directory-from-buffer-name-pure
                                (buffer-name selected-buffer))))
            (if project-path
              (let ((validation (enkan-repl--validate-magit-project-path-pure project-path)))
                (if (plist-get validation :valid)
                  (progn
                    (let ((default-directory project-path))
                      (magit-status))
                    (message "Opened magit for project: %s" project-path))
                  (error "Invalid project path: %s" (plist-get validation :message))))
              (error "Failed to extract project path from buffer: %s" (buffer-name selected-buffer)))))))))

(defun enkan-repl-center-send-region (start end &optional action-string)
  "Send region to center file buffer with action specification.

Category: Center File Multi-buffer Access"
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (if (string-match "^:\\([^ ]+\\) " region-text)
        ;; Alias format: ":alias content"
        (let* ((alias (match-string 1 region-text))
               (remaining-part (substring region-text (match-end 0)))
               (enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
               (resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure alias enkan-buffers)))
          (if resolved-buffer
            (if (string= remaining-part "esc")
              ;; esc case: send center-send-escape with skip-ui
              (enkan-repl--center-send-escape-internal 'skip-ui)
              ;; non-esc case: send string with enkan-repl--send-buffer-content
              (let ((target-directory (enkan-repl--extract-directory-from-buffer-name-pure
                                        (buffer-name resolved-buffer)))
                     (start-no-alias (+ start (length alias) 2)))
                (if (enkan-repl--send-buffer-content start-no-alias end target-directory)
                  (message "Sent string to alias '%s' buffer" alias)
                  (message "Failed to send string to alias '%s' buffer" alias))))
            (message "No buffer found for alias '%s'" alias)))
      ;; No alias: select buffer and send
      (let* ((enkan-buffers (enkan-repl--collect-enkan-buffers-pure (buffer-list)))
             (valid-buffers (enkan-repl--filter-valid-buffers-pure enkan-buffers)))
        (if (= (length valid-buffers) 0)
            (message "No active enkan sessions found")
          (let* ((choices (enkan-repl--build-buffer-selection-choices-pure valid-buffers))
                 (selected-display (completing-read "Select buffer for region send: " choices nil t))
                 (target-buffer (cdr (assoc selected-display choices))))
            (let ((target-directory (enkan-repl--extract-directory-from-buffer-name-pure
                                      (buffer-name target-buffer))))
              (if (enkan-repl--send-buffer-content start end target-directory)
                  (message "Region sent to buffer: %s" (buffer-name target-buffer))
                (message "Failed to send region to buffer: %s" (buffer-name target-buffer))))))))))

;;;###autoload
(defun enkan-repl-center-print-setup-to-buffer ()
  "Print current setup variables for debugging.
Displays enkan-repl-center-multi-project-layouts, enkan-repl-center-project-registry,
enkan-repl-project-aliases, and current session state.

Category: Debugging"
  (interactive)
  (let ((buffer-name "*ENKAN-REPL Setup Debug*"))
    (with-output-to-temp-buffer buffer-name
      (princ "=== ENKAN-REPL CENTER SETUP DEBUG ===\n\n")
      (princ "Multi-project layouts:\n")
      (if enkan-repl-center-multi-project-layouts
          (dolist (layout enkan-repl-center-multi-project-layouts)
            (princ (format "  %s: %s\n" (car layout) (cdr layout))))
        (princ "  <empty>\n"))
      (princ "\nProject registry:\n")
      (if enkan-repl-center-project-registry
          (dolist (entry enkan-repl-center-project-registry)
            (princ (format "  %s: project=%s, path=%s\n"
                           (car entry)
                           (car (cdr entry))
                           (cdr (cdr entry)))))
        (princ "  <empty>\n"))
      (princ "\nProject aliases:\n")
      (if enkan-repl-project-aliases
          (dolist (alias enkan-repl-project-aliases)
            (princ (format "  %s -> %s\n" (car alias) (cdr alias))))
        (princ "  <empty>\n"))
      (princ "\nCurrent session state:\n")
      (princ (format "  Current layout: %s\n" (or enkan-repl--current-multi-project-layout "<none>")))
      (princ (format "  Session list: %s\n" (or enkan-repl-session-list "<empty>")))
      (princ (format "  Session counter: %d\n" enkan-repl--session-counter))
      (princ "\n=== END DEBUG ===\n"))))

(provide 'enkan-repl)

;;; enkan-repl.el ends here

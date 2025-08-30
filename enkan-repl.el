;;; enkan-repl.el --- Enhanced repl utilities for enkan -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Version: 0.12.0
;; Package-Requires: ((emacs "28.2") (eat "0.9.4"))
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

;; Load horizontal menu interface
(require 'hmenu)

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
  "Center file path for project management.
When nil, multi-session functionality is disabled."
  :type '(choice (const :tag "Multi-session functionality disabled" nil)
                 (file :tag "Center file path"))
  :group 'enkan-repl)

(defcustom enkan-repl-project-aliases nil
  "Project name alias definitions,
auto-generated by the custom variable enkan-repl-target-directories.
Supports both global and buffer-local configuration.
Example: \\='((\"pt\" . \"pt-tools\") (\"er\" . \"enkan-repl\"))"
  :type '(alist :key-type string :value-type string)
  :group 'enkan-repl)

(defcustom enkan-repl-target-directories nil
  "Project startup directories for center file functionality.
Each element is in the format (alias . (project-name . project-path)).
Example: \\='((\"pt\" . (\"pt-tools\" . \"/path/to/pt-tools/\"))
      (\"er\" . (\"enkan-repl\" . \"/path/to/enkan-repl/\"))
      (\"eat\" . (\"emacs-eat\" . \"/path/to/emacs-eat/\")))"
  :type '(alist :key-type string
                :value-type (cons string string))
  :group 'enkan-repl)

(defcustom enkan-repl-projects nil
  "Configuration list for frequently used simultaneous project startup.
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

(defvar enkan-repl--current-project nil
  "Currently selected project name.
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

(defun enkan-repl--get-project-directory-from-directories (alias)
  "Get directory path from project directories.
Returns: Directory path or nil"
  (let ((project-info (cdr (assoc alias enkan-repl-target-directories))))
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

;;;; Public API - Send Functions

;;;###autoload
(defun enkan-repl-send-region (start end &optional prefix-arg)
  "Send region text (from START to END) to enkan session buffer.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "r\nP")
  (enkan-repl--send-unified
   (buffer-substring-no-properties start end) prefix-arg nil))

;;;###autoload
(defun enkan-repl-send-line (&optional prefix-arg)
  "Send current line to enkan session buffer.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified
   (buffer-substring-no-properties (line-beginning-position) (line-end-position)) prefix-arg nil))

;;;###autoload
(defun enkan-repl-send-enter (&optional prefix-arg)
  "Send enter key to enkan session buffer.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg :enter))

;;;###autoload
(defun enkan-repl-send-1 (&optional prefix-arg)
  "Send \\='1\\=' to enkan session buffer for numbered choice prompt.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg 1))

;;;###autoload
(defun enkan-repl-send-2 (&optional prefix-arg)
  "Send \\='2\\=' to enkan session buffer for numbered choice prompt.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg 2))

;;;###autoload
(defun enkan-repl-send-3 (&optional prefix-arg)
  "Send \\='3\\=' to enkan session buffer for numbered choice prompt.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg 3))

;;;###autoload
(defun enkan-repl-send-4 (&optional prefix-arg)
  "Send \\='4\\=' to enkan session buffer for numbered choice prompt.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg 4))

;;;###autoload
(defun enkan-repl-send-5 (&optional prefix-arg)
  "Send \\='5\\=' to enkan session buffer for numbered choice prompt.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (enkan-repl--send-unified "" prefix-arg 5))

;;;###autoload
(defun enkan-repl-send-escape (&optional prefix-arg)
  "Send ESC key to enkan session buffer.
- From enkan buffer: Send to current buffer
- From other buffer without prefix: Interactive buffer selection
- With numeric prefix: Send to buffer at index (1-based)

Uses unified backend with smart buffer detection.

Category: Text Sender"
  (interactive "P")
  (cond
   ;; Special case: if current buffer is enkan buffer, send ESC directly
   ((string-match-p "^\\*enkan:" (buffer-name))
    (let ((send-data (enkan-repl--send-primitive-pure "" :escape)))
      (enkan-repl--send-primitive-action (current-buffer) send-data)))
   ;; Otherwise use unified backend
   (t
    (enkan-repl--send-unified "" prefix-arg :escape))))

;;;###autoload
(defun enkan-repl-recenter-bottom ()
  "Recenter all enkan terminal buffers at bottom.

Category: Utilities"
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
    (message "Recentered %d enkan session(s) at bottom" recentered-count)))

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
  "Start eat terminal emulator session in current directory.
Simplified version for use within setup functions only.
FORCE parameter ignored - always starts new session.

Category: Session Controller"
  (interactive)
  ;; Simple eat package loading - fail fast if not available
  (require 'eat)
  ;; Always start new eat session in current directory
  (let* ((target-dir default-directory)
         (buffer-name (concat "*enkan:" (expand-file-name target-dir) "*"))
         (eat-buffer (eat)))
    ;; Simple buffer renaming - no error handling
    (when eat-buffer
      (with-current-buffer eat-buffer
        (rename-buffer buffer-name t))
      (message "Started eat session in: %s" target-dir))))

;;;###autoload
(defun enkan-repl-teardown ()
  "Terminate eat session(s) based on context.
- Standard input file: terminate single eat session for current directory
- Center file: terminate all registered sessions

Category: Session Controller"
  (interactive)
  ;; Check if current buffer filename matches standard input file format
  (let* ((current-file (buffer-file-name))
         (is-standard-file (enkan-repl--is-standard-file-path-pure current-file default-directory)))
    (if is-standard-file
        ;; Standard input file mode: terminate single session
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
              (message "Terminated eat session in: %s" target-dir)))))
      ;; Center file mode: terminate all sessions
      (if (enkan-repl--is-center-file-path-pure enkan-repl-center-file enkan-repl-projects)
          ;; Center file mode implementation
          (let ((buffer-name "*ENKAN-REPL Finish Sessions*"))
            (if (null enkan-repl-session-list)
                (message "No registered sessions to terminate")
              (with-output-to-temp-buffer buffer-name
                (princ "=== ENKAN-REPL FINISH ALL SESSIONS ===\n\n")
                ;; Display current state before termination
                (princ "🔧 Current state before termination:\n")
                (princ (enkan-repl--format-session-state-display
                        (enkan-repl--get-current-session-state-info)))
                (princ "\n")
                (let ((terminated-count 0)
                      (original-session-list enkan-repl-session-list)) ; Capture for y-or-n-p
                  (when (y-or-n-p (format "Terminate all %d registered sessions? "
                                          (length original-session-list)))
                    (princ "🚫 Terminating sessions:\n")
                    ;; Terminate all session buffers
                    (let* ((termination-result (enkan-repl--terminate-all-session-buffers
                                                original-session-list
                                                enkan-repl-target-directories))
                           (actual-terminated-count (car termination-result))
                           (session-termination-details (cdr termination-result)))
                      (setq terminated-count actual-terminated-count)
                      ;; Display termination results
                      (dolist (detail session-termination-details)
                        (let ((session-number (cdr (assoc :session-number detail)))
                              (project-name (cdr (assoc :project-name detail)))
                              (status (cdr (assoc :status detail))))
                          (cond
                           ((eq status 'terminated)
                            (princ (format "  ✅ Session %d: %s (terminated)\n" session-number project-name)))
                           ((eq status 'buffer-not-found)
                            (princ (format "  ⚠️ Session %d: %s (buffer not found)\n" session-number project-name)))
                           ((eq status 'project-path-not-found)
                            (princ (format "  ❌ Session %d: %s (project path not found)\n" session-number project-name)))))))
                    ;; Reset global configuration
                    (enkan-repl--reset-global-session-variables)
                    ;; Auto-disable global center file mode
                    ;; (when (enkan-repl--disable-global-minor-mode-if-active)
                    ;;   (princ "\n🔄 Auto-disabled center file global mode\n"))
                    ;; Display final state
                    (princ "\n🧹 Configuration reset:\n")
                    (princ (enkan-repl--format-session-state-display
                            (enkan-repl--get-current-session-state-info)))
                    (princ (format "\n✅ Terminated %d sessions, cleared session list and reset project configuration.\n" terminated-count))
                    (princ "\n=== END FINISH SESSIONS ===\n"))))))
        (message "Not in standard file or center file mode")))))

(defun enkan-repl--is-standard-file-path-pure (file-path directory-name)
  "Decide the standard input file or not."
  (when file-path
    (let* ((base-name (file-name-sans-extension (file-name-nondirectory file-path)))
           (decoded-path (enkan-repl--decode-full-path base-name)))
      (and (not (string= "" decoded-path)) (string= decoded-path directory-name)))))

(defun enkan-repl--is-center-file-path-pure (center-file-path projects)
  "Decide the center file or not."
  (and center-file-path
       (stringp center-file-path)
       (not (string= "" center-file-path))
       projects))

(defun enkan-repl--setup-log-state (buffer-name state-type layout sessions counter)
  "Log current or final state to BUFFER-NAME.
STATE-TYPE: 'Current' or 'Final'
PROJECT: current project
SESSIONS: session list
COUNTER: session counter"
  (with-current-buffer buffer-name
    (princ (format "🔧 %s state %s:\n" state-type (if (string= state-type "Current") "before setup" "after setup")))
    (princ (format "  Layout (enkan-repl--current-project): %s\n" (or layout "nil")))
    (princ (format "  Sessions (enkan-repl-session-list): %s\n" (or sessions "nil")))
    (princ (format "  Counter (enkan-repl--session-counter): %d\n\n" counter))))

(defun enkan-repl--setup-enable-global-mode (buffer-name)
  "Enable global center file mode if not already enabled and log to BUFFER-NAME."
  (unless enkan-repl-global-minor-mode
    (enkan-repl-global-minor-mode 1)
    (with-current-buffer buffer-name
      (princ "🚀 Auto-enabled center file global mode for project workflow\n\n"))))

(defun enkan-repl--setup-reset-config (buffer-name)
  "Reset session configuration and log to BUFFER-NAME."
  (setq enkan-repl-session-list nil)
  (setq enkan-repl--session-counter 0)
  (setq enkan-repl--current-project nil)
  (with-current-buffer buffer-name
    (princ "🧹 Reset previous configuration\n\n")))

(defun enkan-repl--setup-set-project-aliases (project-name alias-list buffer-name)
  "Set project aliases from ALIAS-LIST for PROJECT-NAME and log to BUFFER-NAME."
  (let ((project-aliases '()))
    (dolist (alias alias-list)
      (let ((project-info (enkan-repl--get-project-info-from-directories alias)))
        (when project-info
          (let ((proj-name (car project-info)))
            (push (cons alias proj-name) project-aliases)))))
    (setq enkan-repl-project-aliases (nreverse project-aliases))
    (with-current-buffer buffer-name
      (princ (format "🔧 Setup project aliases (enkan-repl-project-aliases): %s\n\n" enkan-repl-project-aliases)))))

(defun enkan-repl--setup-start-sessions (alias-list buffer-name)
  "Start eat sessions for each alias in ALIAS-LIST and log to BUFFER-NAME.
Includes error handling for individual session failures."
  (with-current-buffer buffer-name
    (princ "🚀 Starting eat sessions:\n"))
  (let ((session-number 1)
        (success-count 0)
        (failure-count 0))
    (dolist (alias alias-list)
      (condition-case err
          (let ((project-info (enkan-repl--setup-project-session alias)))
            (let ((project-name (car project-info))
                  (project-path (expand-file-name (cdr project-info)))
                  (default-directory (expand-file-name (cdr project-info))))
              ;; Register session
              (enkan-repl--register-session session-number project-name)
              ;; Start eat session in current directory (force restart if needed)
              (enkan-repl-start-eat t)
              (with-current-buffer buffer-name
                (princ (format "  ✅ Session %d: %s (%s) - SUCCESS\n" session-number alias project-name)))
              (setq success-count (1+ success-count))))
        (error
         (with-current-buffer buffer-name
           (princ (format "  ❌ Session %d: %s - FAILED (%s)\n" session-number alias (error-message-string err))))
         (setq failure-count (1+ failure-count))))
      (setq session-number (1+ session-number)))
    (with-current-buffer buffer-name
      (princ (format "\n📊 Session start summary: %d success, %d failed\n\n" success-count failure-count)))))

(defun enkan-repl--setup-project-session (alias)
  "Setup project session for given ALIAS.
Implemented as pure function, side effects are handled by upper functions."
  (let ((project-info (enkan-repl--get-project-info-from-directories alias)))
    (if project-info
        (let ((project-name (car project-info))
              (project-path (cdr project-info)))
          (cons project-name project-path))
      (error "Project alias '%s' not found in registry" alias))))

(defun enkan-repl--setup-log-state (buffer-name state-type layout sessions counter)
  "Log current or final state to BUFFER-NAME.
STATE-TYPE: 'Current' or 'Final'
PROJECT: current project
SESSIONS: session list
COUNTER: session counter"
  (with-current-buffer buffer-name
    (princ (format "🔧 %s state %s:\n" state-type (if (string= state-type "Current") "before setup" "after setup")))
    (princ (format "  Layout (enkan-repl--current-project): %s\n" (or layout "nil")))
    (princ (format "  Sessions (enkan-repl-session-list): %s\n" (or sessions "nil")))
    (princ (format "  Counter (enkan-repl--session-counter): %d\n\n" counter))))

(defun enkan-repl--setup-enable-global-mode (buffer-name)
  "Enable global center file mode if not already enabled and log to BUFFER-NAME."
  (unless enkan-repl-global-minor-mode
    (enkan-repl-global-minor-mode 1)
    (with-current-buffer buffer-name
      (princ "🚀 Auto-enabled center file global mode for project workflow\n\n"))))

(defun enkan-repl--setup-reset-config (buffer-name)
  "Reset session configuration and log to BUFFER-NAME."
  (setq enkan-repl-session-list nil)
  (setq enkan-repl--session-counter 0)
  (setq enkan-repl--current-project nil)
  (with-current-buffer buffer-name
    (princ "🧹 Reset previous configuration\n\n")))

(defun enkan-repl--setup-set-project-aliases (project-name alias-list buffer-name)
  "Set project aliases from ALIAS-LIST for PROJECT-NAME and log to BUFFER-NAME."
  (let ((project-aliases '()))
    (dolist (alias alias-list)
      (let ((project-info (enkan-repl--get-project-info-from-directories alias)))
        (when project-info
          (let ((proj-name (car project-info)))
            (push (cons alias proj-name) project-aliases)))))
    (setq enkan-repl-project-aliases (nreverse project-aliases))
    (with-current-buffer buffer-name
      (princ (format "🔧 Setup project aliases (enkan-repl-project-aliases): %s\n\n" enkan-repl-project-aliases)))))

(defun enkan-repl--setup-start-sessions (alias-list buffer-name)
  "Start eat sessions for each alias in ALIAS-LIST and log to BUFFER-NAME.
Includes error handling for individual session failures."
  (with-current-buffer buffer-name
    (princ "🚀 Starting eat sessions:\n"))
  (let ((session-number 1)
        (success-count 0)
        (failure-count 0))
    (dolist (alias alias-list)
      (condition-case err
          (let ((project-info (enkan-repl--setup-project-session alias)))
            (let ((project-name (car project-info))
                  (project-path (expand-file-name (cdr project-info)))
                  (default-directory (expand-file-name (cdr project-info))))
              ;; Register session
              (enkan-repl--register-session session-number project-name)
              ;; Start eat session in current directory (force restart if needed)
              (enkan-repl-start-eat t)
              (with-current-buffer buffer-name
                (princ (format "  ✅ Session %d: %s (%s) - SUCCESS\n" session-number alias project-name)))
              (setq success-count (1+ success-count))))
        (error
         (with-current-buffer buffer-name
           (princ (format "  ❌ Session %d: %s - FAILED (%s)\n" session-number alias (error-message-string err))))
         (setq failure-count (1+ failure-count))))
      (setq session-number (1+ session-number)))
    (with-current-buffer buffer-name
      (princ (format "\n📊 Session start summary: %d success, %d failed\n\n" success-count failure-count)))))

;;;###autoload
(defun enkan-repl-setup ()
  "Set up window layout based on context.
- Standard input file: basic window layout with project input file on left and eat session on right
- Center file: auto start eat sessions using project configuration

Category: Session Controller"
  (interactive)
  ;; Check if current buffer filename matches standard input file format
  (let* ((current-file (buffer-file-name))
         (is-standard-file (enkan-repl--is-standard-file-path-pure current-file default-directory)))
    (if is-standard-file
        ;; Standard input file mode: simple window layout
        (progn
          (delete-other-windows)
          (split-window-right)
          ;; Move to right window and start eat session
          (other-window 1)
          (enkan-repl-start-eat)
          ;; Move back to left window and open project input file
          (other-window -1)
          (enkan-repl-open-project-input-file)
          (message "Basic window layout setup complete"))
      ;; Center file mode: check if center file is specified as non-empty string
      (if (enkan-repl--is-center-file-path-pure enkan-repl-center-file enkan-repl-projects)
          (let ((project-name (hmenu "Project:" (mapcar #'car enkan-repl-projects)))
                (buffer-name "*ENKAN-REPL Auto Setup*")
                (old-state (list enkan-repl--current-project
                                 (copy-tree enkan-repl-session-list)
                                 enkan-repl--session-counter)))
            (with-output-to-temp-buffer buffer-name
              (princ (format "=== ENKAN-REPL AUTO SETUP: %s ===\n\n" project-name))
              (condition-case err
                  (progn
                    ;; Log initial state
                    (enkan-repl--setup-log-state buffer-name "Current"
                                                 (nth 0 old-state)
                                                 (nth 1 old-state)
                                                 (nth 2 old-state))
                    ;; Enable global mode
                    (enkan-repl--setup-enable-global-mode buffer-name)
                    ;; Reset configuration
                    (enkan-repl--setup-reset-config buffer-name)
                    ;; Set project aliases
                    (let ((alias-list (cdr (assoc project-name enkan-repl-projects))))
                      (unless alias-list
                        (error "Project '%s' not found" project-name))
                      (enkan-repl--setup-set-project-aliases project-name alias-list buffer-name)
                      ;; Start sessions
                      (enkan-repl--setup-start-sessions alias-list buffer-name))
                    ;; Set final project configuration
                    (setq enkan-repl--current-project project-name)
                    (princ (format "\n✅ Setup completed for project: %s\n" project-name))
                    (princ "Arrange your preferred window configuration!\n\n")
                    (princ "=== END SETUP ===\n")))))
        (message "Center file not configured or no projects defined")))))

;;; Debug and Utility Functions

;; These functions are primarily for debugging and troubleshooting.
;; They can be safely removed in production environments if needed.

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

;;;; Global Minor Mode

(defgroup enkan-repl nil
  "enkan-repl customization."
  :group 'tools)

(defvar enkan-repl-global-minor-mode-map (make-sparse-keymap)
  "Keymap for `enkan-repl-global-minor-mode'.")

(defun enkan-repl--build-map (bindings)
  "Build a sparse keymap from BINDINGS of (KEY . COMMAND)."
  (let ((m (make-sparse-keymap)))
    (dolist (kv bindings)
      (keymap-set m (car kv) (cdr kv)))
    m))

;; Store original keybindings for safe restoration
(defcustom enkan-repl-global-minor-bindings nil
  "Keybindings for `enkan-repl-global-minor-mode`."
  :type '(repeat (cons (string :tag "Key")
                       (function :tag "Command")))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'enkan-repl--refresh-global-minor-map)
           (enkan-repl--refresh-global-minor-map)))
  :group 'enkan-repl)

(defun enkan-repl--refresh-global-minor-map ()
  "Rebuild `enkan-repl-global-minor-mode-map' from `enkan-repl-global-minor-bindings'."
  (setq enkan-repl-global-minor-mode-map
        (enkan-repl--build-map enkan-repl-global-minor-bindings)))

;; 初期構築
(enkan-repl--refresh-global-minor-map)

(define-minor-mode enkan-repl-global-minor-mode
  "Global minor mode for enkan-repl operations.
When enabled, some keybindings are available across all buffers."
  :init-value nil
  :global t
  :lighter " ECF"
  :keymap enkan-repl-global-minor-mode-map
  ;; Do nothing dangerous to global keymap - let minor mode keymap handle it
  ;; This avoids overriding critical keybindings like M-x
  (message (if enkan-repl-global-minor-mode
               "✅ global mode enabled"
             "❌ global mode disabled")))

;;;###autoload
(defun enkan-repl-toggle-global-mode ()
  "Toggle enkan-repl global mode on/off."
  (interactive)
  (enkan-repl-global-minor-mode 'toggle))

;;;; Auto Setup Functions

(defun enkan-repl--get-project-info-from-directories (alias)
  "Get project info from directories for ALIAS.
Return (project-name . project-path) or nil if not found."
  (cdr (assoc alias enkan-repl-target-directories)))

(defun enkan-repl--get-project-path-from-directories (project-name target-directories)
  "Pure function to get project path from directories by project name."
  (let ((project-info (cl-find-if (lambda (entry)
                                    (string= (car (cdr entry)) project-name))
                                  target-directories)))
    (when project-info
      (cdr (cdr project-info)))))

;;; Helper functions for state management and formatting

(defun enkan-repl--get-current-session-state-info ()
  "Retrieve current session state information as an alist.
This is a pure function."
  (list
   (cons 'current-project enkan-repl--current-project)
   (cons 'session-list enkan-repl-session-list)
   (cons 'session-counter enkan-repl--session-counter)
   (cons 'project-aliases enkan-repl-project-aliases)))

(defun enkan-repl--format-session-state-display (state-info &optional prefix)
  "Format session state information for display.
STATE-INFO is an alist from `enkan-repl--get-current-session-state-info`.
PREFIX is an optional string to prepend to each line.
This is a pure function."
  (let ((current-project (cdr (assoc 'current-project state-info)))
        (session-list (cdr (assoc 'session-list state-info)))
        (session-counter (cdr (assoc 'session-counter state-info)))
        (project-aliases (cdr (assoc 'project-aliases state-info)))
        (prefix-str (or prefix "")))
    (concat
     (format "%s  Layout (enkan-repl--current-project): %s\n" prefix-str (or current-project "nil"))
     (format "%s  Sessions (enkan-repl-session-list): %s\n" prefix-str session-list)
     (format "%s  Counter (enkan-repl--session-counter): %d\n" prefix-str session-counter)
     (when project-aliases
       (format "%s  Permanent aliases (enkan-repl-project-aliases): %s\n" prefix-str project-aliases)))))

;;; Functions with side effects

(defun enkan-repl--terminate-all-session-buffers (session-list target-directories)
  "Terminate all buffers associated with sessions in SESSION-LIST.
TARGET-DIRECTORIES is a list of directories to search for project paths.
Returns an alist of (terminated-count . session-termination-results).
SESSION-TERMINATION-RESULTS is a list of alists, each containing
(:session-number N :project-name S :status (terminated | buffer-not-found | project-path-not-found)).
This function has the side effect of killing buffers."
  (let ((terminated-count 0)
        (termination-results '()))
    (dolist (session session-list)
      (let* ((session-number (car session))
             (project-name (cdr session))
             (project-path (enkan-repl--get-project-path-from-directories project-name target-directories)))
        (if project-path
            (let ((buffer (enkan-repl--get-buffer-for-directory project-path)))
              (if buffer
                  (progn
                    (kill-buffer buffer)
                    (setq terminated-count (1+ terminated-count))
                    (push (list (cons :session-number session-number)
                                (cons :project-name project-name)
                                (cons :status 'terminated))
                          termination-results))
                (push (list (cons :session-number session-number)
                            (cons :project-name project-name)
                            (cons :status 'buffer-not-found))
                      termination-results)))
          (push (list (cons :session-number session-number)
                      (cons :project-name project-name)
                      (cons :status 'project-path-not-found))
                termination-results))))
    (cons terminated-count (nreverse termination-results))))

(defun enkan-repl--reset-global-session-variables ()
  "Reset global session-related variables.
This function has the side effect of modifying global variables:
`enkan-repl-session-list`, `enkan-repl--session-counter`,
`enkan-repl--current-project`, `enkan-repl-project-aliases`."
  (setq enkan-repl-session-list nil)
  (setq enkan-repl--session-counter 0)
  (setq enkan-repl--current-project nil)
  (setq enkan-repl-project-aliases nil))

(defun enkan-repl--disable-global-minor-mode-if-active ()
  "Disable `enkan-repl-global-minor-mode` if it is active.
This function has the side effect of calling `enkan-repl-global-minor-mode`.
Returns t if mode was disabled, nil otherwise."
  (when enkan-repl-global-minor-mode
    (enkan-repl-global-minor-mode -1)
    t))

;;;###autoload
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


(defun enkan-repl--get-available-buffers-pure (buffer-list)
  "Pure function to get available enkan buffers from BUFFER-LIST.
Consolidates buffer collection and filtering into single function.
Returns list of valid enkan buffers with active eat processes."
  (seq-filter (lambda (buffer)
                (and (bufferp buffer)
                     (buffer-name buffer)
                     (string-match-p "^\\*enkan:" (buffer-name buffer))
                     (with-current-buffer buffer
                       (and (boundp 'eat--process)
                            eat--process
                            (process-live-p eat--process)))))
              buffer-list))

(defun enkan-repl--resolve-target-buffer-pure (prefix-arg alias buffers)
  "Pure function to resolve target buffer from multiple inputs.
PREFIX-ARG: numeric prefix for index-based selection
ALIAS: alias string for alias-based selection
BUFFERS: list of available buffers
Returns resolved buffer or nil if no match.
Resolution priority: prefix-arg → alias → nil (for interactive selection)."
  (cond
   ;; Priority 1: prefix-arg based selection
   ((and prefix-arg (numberp prefix-arg) (> prefix-arg 0))
    (when (and (<= prefix-arg (length buffers)))
      (nth (1- prefix-arg) buffers)))
   ;; Priority 2: alias based selection
   ((and alias (stringp alias) (not (string= "" alias)))
    (let ((alias-entry (assoc alias enkan-repl-project-aliases)))
      (when alias-entry
        (let* ((resolved-project (cdr alias-entry))
               (matching-buffers (seq-filter
                                  (lambda (buf)
                                    (let ((buffer-project (enkan-repl--extract-project-name (buffer-name buf))))
                                      (string= resolved-project buffer-project)))
                                  buffers)))
          (car matching-buffers)))))
   ;; Priority 3: return nil for interactive selection
   (t nil)))

(defun enkan-repl--send-primitive-pure (text special-key-type)
  "Pure function to prepare send content from TEXT and SPECIAL-KEY-TYPE.
TEXT: original text content
SPECIAL-KEY-TYPE: :enter, :escape, number 1-9, or nil
Returns plist with :content (string to send) and :action (action type)."
  (cond
   ((eq special-key-type :enter)
    (list :content "\r" :action 'key))
   ((eq special-key-type :escape)
    (list :content "\e" :action 'key))
   ((and (numberp special-key-type)
         (>= special-key-type 1)
         (<= special-key-type 9))
    (list :content (number-to-string special-key-type) :action 'number))
   ((null special-key-type)
    (list :content text :action 'text))
   (t
    (error "Invalid special-key-type: %s" special-key-type))))

(defun enkan-repl--send-primitive-action (buffer send-data)
  "Side-effect function to execute send action to BUFFER.
BUFFER: target buffer with eat process
SEND-DATA: plist from enkan-repl--send-primitive-pure
Returns t on success, nil on failure."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (and (boundp 'eat--process)
                 eat--process
                 (process-live-p eat--process))
        (let ((content (plist-get send-data :content)))
          (when content
            (eat--send-string eat--process content)
            ;; For text content, also send carriage return
            (when (eq (plist-get send-data :action) 'text)
              (eat--send-string eat--process "\r"))
            t))))))

(defun enkan-repl--send-unified (text &optional prefix-arg special-key-type)
  "Unified backend for all send commands.
TEXT: text content to send
PREFIX-ARG: numeric prefix for buffer selection (optional)
SPECIAL-KEY-TYPE: :enter, :escape, 1-9, or nil for normal text (optional)

Resolution priority: prefix-arg → alias parsing → interactive selection
Returns t on success, nil on failure."
  (let* ((available-buffers (enkan-repl--get-available-buffers-pure (buffer-list)))
         (parsed-command nil)
         (resolved-alias nil)
         (target-buffer nil)
         (final-text text))
    ;; Early return if no available buffers
    (if (null available-buffers)
        (progn
          (message "No active enkan sessions found. Start one with M-x enkan-repl-start-eat")
          nil)
      ;; Parse alias from text if no special-key-type
      (when (null special-key-type)
        (setq parsed-command (enkan-repl--parse-alias-command-pure text))
        (when (plist-get parsed-command :valid)
          (setq resolved-alias (plist-get parsed-command :alias))
          (setq final-text (plist-get parsed-command :text))
          ;; Handle special commands within alias
          (let ((command (plist-get parsed-command :command)))
            (cond
             ((eq command :esc)
              (setq special-key-type :escape)
              (setq final-text ""))
             ((eq command :ret)
              (setq special-key-type :enter)
              (setq final-text ""))
             ((and (stringp command) (string-match-p "^[1-9]$" command))
              (setq special-key-type (string-to-number command))
              (setq final-text ""))))))
      ;; Resolve target buffer
      (setq target-buffer (enkan-repl--resolve-target-buffer-pure
                           prefix-arg resolved-alias available-buffers))
      ;; Handle alias resolution failure
      (if (and resolved-alias (null target-buffer))
          ;; Explicit alias specified but not found - show error and return
          (progn
            (message "No buffer found for alias '%s'" resolved-alias)
            nil)
        ;; Interactive selection if no target resolved and no explicit alias
        (unless target-buffer
          (if (= 1 (length available-buffers))
              (setq target-buffer (car available-buffers))
            (let* ((choices (enkan-repl--build-buffer-selection-choices-pure available-buffers))
                   (selection (hmenu "Select buffer for send:" choices)))
              (message "DEBUG: hmenu returned: %S (type: %s)" selection (type-of selection))
              (message "DEBUG: choices alist: %S" choices)
              (setq target-buffer (cdr (assoc selection choices)))
              (message "DEBUG: resolved target-buffer: %S" target-buffer))))
        ;; Execute send
        (when target-buffer
          (let* ((send-data (enkan-repl--send-primitive-pure final-text special-key-type))
                 (success (enkan-repl--send-primitive-action target-buffer send-data)))
            (when success
              (message "Sent to buffer: %s" (buffer-name target-buffer)))
            success))))))

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
   ((string= "" number)
    (list :valid nil :number nil :message "Number cannot be empty"))
   ((not (string-match-p "^[0-9]$" number))
    (list :valid nil :number nil :message "Number must be a single digit (0-9)"))
   (t
    (list :valid t :number number :message (format "Valid number: %s" number)))))

(defun enkan-repl--send-text-to-buffer (text buffer)
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
(defun enkan-repl-send-escape (&optional prefix-arg)
  "Send ESC key to eat session buffer from center file or current enkan buffer.
- If called from enkan buffer: Send ESC to current buffer
- If called from center file without prefix: Select from available enkan buffers
- With numeric prefix: Send to buffer at that index (1-based)

Category: Center File Multi-buffer Access"
  (interactive "P")
  (cond
   ;; Special case: if current buffer is enkan buffer, send ESC directly
   ((string-match-p "^\\*enkan:" (buffer-name))
    (let ((send-data (enkan-repl--send-primitive-pure "" :escape)))
      (enkan-repl--send-primitive-action (current-buffer) send-data)))
   ;; Otherwise use unified backend
   (t
    (enkan-repl--send-unified "" prefix-arg :escape))))

;;;###autoload
(defun enkan-repl-open-project-directory ()
  "Open project directory in dired from current project.

Category: Center File Multi-buffer Access"
  (interactive)
  (if enkan-repl--current-project
      (let* ((current-project (cdr (assoc enkan-repl--current-project enkan-repl-projects)))
             (project-choices '()))
        (if current-project
            (progn
              ;; Build choices list with alias and directory
              (dolist (alias current-project)
                (let ((project-info (enkan-repl--get-project-info-from-directories alias)))
                  (when project-info
                    (let ((project-name (car project-info))
                          (project-path (cdr project-info)))
                      (push (cons (format "%s (%s)" alias project-path) project-path) project-choices)))))
              (if project-choices
                  (let* ((selected-display (hmenu "Select project directory to open:" project-choices))
                         (selected-path (cdr (assoc selected-display project-choices))))
                    (if (file-directory-p selected-path)
                        (dired selected-path)
                      (message "Directory does not exist: %s" selected-path)))
                (message "No valid project directories found in project")))
          (message "Current project '%s' not found in configurations" enkan-repl--current-project)))
    (message "No project is currently active")))

;;;###autoload
(defun enkan-repl--analyze-send-content-pure (content prefix-arg)
  "Pure function to analyze send content and determine action.
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

(defun enkan-repl--parse-alias-command-pure (input-string)
  "Pure function to parse alias command format: ':alias esc' or ':alias :ret'.
INPUT-STRING is the command string to parse.
Returns plist with :valid, :alias, :command, :text, :message."
  (cond
   ((not (stringp input-string))
    (list :valid nil :message "Input must be a string"))
   ((string= "" input-string)
    (list :valid nil :message "Must start with : prefix"))
   ((not (string-match-p "^:" input-string))
    (list :valid nil :message "Must start with : prefix"))
   ((string-match "^:\\([a-zA-Z0-9]+\\) \\(.*\\)" input-string)
    ;; Manual multiline extraction for alias with space and content
    (let* ((alias (match-string 1 input-string))
           (rest (substring input-string (+ 1 (length alias) 1))))  ; Skip :alias + space
      (if (string= "" alias)
          (list :valid nil :message "Invalid format. Alias cannot be empty")
        (cond
         ((equal rest "esc")
          ;; Escape command
          (list :valid t :alias alias :command :esc :text nil))
         ((equal rest ":ret")
          ;; Return command
          (list :valid t :alias alias :command :ret :text nil))
         (t
          ;; Text to send
          (list :valid t :alias alias :command :text :text rest))))))
   ((string-match "^:\\([a-zA-Z0-9]+\\)$" input-string)
    ;; Only alias specified
    (let ((alias (match-string 1 input-string)))
      (list :valid t :alias alias :command :empty :text "")))
   (t
    (list :valid nil :message "Invalid format. Use: :alias [text|esc|:ret]"))))

;; Pure functions for global operations
(defun enkan-repl-validate-path-pure (file-path)
  "Validate center FILE-PATH for opening.
Returns plist with :valid, :message."
  (cond
   ((null file-path)
    (list :valid nil :message "Center file path not configured"))
   ((not (stringp file-path))
    (list :valid nil :message "Center file path must be a string"))
   ((string= "" file-path)
    (list :valid nil :message "Center file path is empty"))
   (t
    (list :valid t :message "Valid center file path"))))

(defun enkan-repl-center-file-check-exists-pure (file-path)
  "Check if center FILE-PATH exists.
Returns plist with :exists, :action."
  (if (file-exists-p file-path)
      (list :exists t :action "open")
    (list :exists nil :action "create")))

(defun enkan-repl-determine-action-pure (file-path)
  "Determine action to take for center FILE-PATH.
Returns plist with :valid, :action, :message."
  (let ((validation (enkan-repl-validate-path-pure file-path)))
    (if (plist-get validation :valid)
        (let ((exists-check (enkan-repl-center-file-check-exists-pure file-path)))
          (list :valid t
                :action (plist-get exists-check :action)
                :message (if (plist-get exists-check :exists)
                             "Opening existing center file"
                           "Creating new center file")))
      validation)))

(defun enkan-repl-open-center-file ()
  "Open or create the center file based on enkan-repl-center-file configuration.

Category: Center File Operations"
  (interactive)
  (let ((result (enkan-repl-determine-action-pure enkan-repl-center-file)))
    (if (plist-get result :valid)
        (progn
          (message "%s" (plist-get result :message))
          (find-file enkan-repl-center-file))
      (error "%s" (plist-get result :message)))))

;; Pure functions for magit project selection (used by enkan-repl-magit)
(defun enkan-repl--get-magit-project-list-pure (target-directories)
  "Get list of projects for magit selection from TARGET-DIRECTORIES.
Returns list of (project-name . project-path) pairs."
  (unless target-directories
    (error "Project directories is empty"))
  (mapcar (lambda (entry)
            (let ((project-name (car entry))
                  (project-path (if (stringp (cdr entry))
                                    (cdr entry)
                                  ;; Handle nested cons structure
                                  (if (consp (cdr entry))
                                      (cdr (cdr entry))
                                    (cdr entry)))))
              (cons project-name (expand-file-name project-path))))
          target-directories))

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
Returns list of strings for selection interface."
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

(defun enkan-repl-magit ()
  "Open magit for selected project from active sessions only.

Category: Center File Operations"
  (interactive)
  (let* ((valid-buffers (enkan-repl--get-available-buffers-pure (buffer-list))))
    (if (= (length valid-buffers) 0)
        (message "No active enkan sessions found for magit")
      (let* ((selected-buffer
              (if (= 1 (length valid-buffers))
                  ;; Auto-select single session
                  (car valid-buffers)
                ;; Interactive selection for multiple sessions
                (let* ((choices (enkan-repl--build-buffer-selection-choices-pure valid-buffers))
                       (selected-display (hmenu "Select project for magit:" choices)))
                  (cdr (assoc selected-display choices))))))
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


;;;###autoload
(defun enkan-repl-print-setup-to-buffer ()
  "Print current setup variables for debugging.
Displays enkan-repl-projects, enkan-repl-target-directories,
enkan-repl-project-aliases, and current session state.

Category: Debugging"
  (interactive)
  (let ((buffer-name "*ENKAN-REPL Setup Debug*"))
    (with-output-to-temp-buffer buffer-name
      (princ "=== ENKAN-REPL CENTER SETUP DEBUG ===\n\n")
      (princ "projects:\n")
      (if enkan-repl-projects
          (dolist (project enkan-repl-projects)
            (princ (format "  %s: %s\n" (car project) (cdr project))))
        (princ "  <empty>\n"))
      (princ "\nProject directories:\n")
      (if enkan-repl-target-directories
          (dolist (entry enkan-repl-target-directories)
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
      (princ (format "  Current project: %s\n" (or enkan-repl--current-project "<none>")))
      (princ (format "  Session list: %s\n" (or enkan-repl-session-list "<empty>")))
      (princ (format "  Session counter: %d\n" enkan-repl--session-counter))
      (princ "\n=== END DEBUG ===\n"))))

(provide 'enkan-repl)

;;; enkan-repl.el ends here

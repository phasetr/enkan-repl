;;; claudemacs-client.el --- Enhanced client utilities for claudemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1") (claudemacs "0.1.0"))
;; Keywords: claudemacs ai tools convenience
;; URL: https://github.com/phasetr/claudemacs-client
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

;; claudemacs-client provides enhanced utilities for working with claudemacs.
;;
;; Features:
;; - Send text from files, buffers, and regions to claudemacs
;; - Dedicated input file with org-mode support
;; - File-based project management with persistent storage
;; - Multi-project support (planned)
;; - Enhanced key bindings for efficient workflow
;;
;; Installation:
;; Add to your init.el:
;;   (require 'claudemacs-client)
;;
;; Usage:
;; M-x claudemacs-client-open-project-input
;;
;; Acknowledgments:
;; This project is built on top of claudemacs.  We thank the original
;; authors for their excellent work.

;;; Code:

(require 'cl-lib)

;; Compatibility check
(when (locate-library "claudemacs")
  (require 'claudemacs))

;; Declare external functions to avoid byte-compiler warnings
(declare-function eat--send-string "eat" (process string))
(declare-function claudemacs-client--encode-full-path-pure "claudemacs-client-test")
(declare-function claudemacs-client--decode-full-path-pure "claudemacs-client-test")
(defvar eat--process)
(defvar eat-mode)

;;;; Custom Variables

(defgroup claudemacs-client nil
  "Enhanced client utilities for claudemacs."
  :group 'tools
  :prefix "claudemacs-client-")

;;;; Constants

(defconst claudemacs-client-path-separator "--"
  "String to replace path separators (/) in filenames.
This is a fixed part of the claudemacs-client filename encoding scheme
and should not be changed to maintain compatibility and security.")

(defconst claudemacs-client-default-template-filename "default.org"
  "Default template filename for claudemacs-client input files.
This file should be located in the package directory and contains
the standard template structure for new project input files.")

(defconst claudemacs-client-file-prefix "cec"
  "Prefix for claudemacs-client encoded filenames.
This is a fixed part of the filename encoding scheme and should not be changed
to maintain compatibility with existing project files.")

;;;; Internal Variables

(defvar claudemacs-client-debug-mode nil
  "When non-nil, enable debug messages for send operations.")

(defvar claudemacs-client--package-directory
  (cond
   ;; Try load-file-name first (when loading with load command)
   (load-file-name (file-name-directory load-file-name))
   ;; Try buffer-file-name (when evaluating in buffer)
   (buffer-file-name (file-name-directory buffer-file-name))
   ;; Try to find claudemacs-client.el in load-path
   ((locate-library "claudemacs-client")
    (file-name-directory (locate-library "claudemacs-client")))
   ;; Search for default.org in load-path directories
   (t
    (or
     (cl-some
      (lambda (dir)
        (let ((expanded-dir (expand-file-name dir))
              (default-org-path (expand-file-name claudemacs-client-default-template-filename (expand-file-name dir))))
          (when (file-exists-p default-org-path)
            expanded-dir)))
      load-path)
     ;; Final fallback to current directory
     default-directory)))
  "Package directory determined at load time.")

(defcustom claudemacs-client-template-file nil
  "Template file path for claudemacs-client input files.
When nil, uses the default template (default.org).
When set to a file path, uses that file as the template."
  :type
  '(choice
    (const :tag "Use default template" nil)
    (file :tag "Custom template file"))
  :group
  'claudemacs-client)

;;;; Core Functions

;;;; File Naming and Path Management

(defun claudemacs-client--encode-full-path (path)
  "Encode PATH by replacing forward slashes with the configured separator.
Example: \\='/Users/phasetr/project1/\\=' -> \\='cec--Users--phasetr--project1\\='"
  (claudemacs-client--encode-full-path-pure
   path
   claudemacs-client-file-prefix
   claudemacs-client-path-separator))

(defun claudemacs-client--decode-full-path (encoded-name)
  "Decode ENCODED-NAME back to original path.
Example: \\='cec--Users--phasetr--project1\\=' -> \\='/Users/phasetr/project1/\\='"
  (claudemacs-client--decode-full-path-pure
   encoded-name
   claudemacs-client-file-prefix
   claudemacs-client-path-separator))

(defun claudemacs-client--get-project-file-path (&optional directory)
  "Get the full path for the project file based on DIRECTORY.
If DIRECTORY is nil, use the current `default-directory'."
  (let*
      ((target-dir (or directory default-directory))
       (encoded-name (claudemacs-client--encode-full-path target-dir))
       (filename (concat encoded-name ".org")))
    (expand-file-name filename target-dir)))

;;;; Template Loading Functions


(defun claudemacs-client--get-package-directory ()
  "Get package directory for template files.
Returns directory path where default templates are located."
  claudemacs-client--package-directory)

(defun claudemacs-client--load-template ()
  "Load template content based on claudemacs-client-template-file setting.
Returns template content as string, or nil if no template found."
  (let ((template-path
         (cond
          ;; If custom template file is specified, use it
          (claudemacs-client-template-file
           (expand-file-name claudemacs-client-template-file))
          ;; Otherwise, look for default template in package directory
          (t (expand-file-name claudemacs-client-default-template-filename
                               (claudemacs-client--get-package-directory))))))
    (when (and template-path (file-exists-p template-path))
      (with-temp-buffer
        (insert-file-contents template-path)
        (buffer-string)))))

;;;; Pure Functions for Directory/Buffer Detection
;; Pure functions have been moved to test file to avoid namespace pollution

;;;; Debug Functions

(defun claudemacs-client--debug-message (format-string &rest args)
  "Print debug message if debug mode is enabled.
FORMAT-STRING is the format string.  ARGS are the arguments."
  (when claudemacs-client-debug-mode
    (apply #'message (concat "[CLAUDEMACS-DEBUG] " format-string) args)))

(defun claudemacs-client--sanitize-content (content)
  "Sanitize CONTENT to ensure it can be safely sent to claudemacs.
This function handles edge cases with special characters and ensures
proper formatting for terminal input.  Also addresses Claude Code
interpretation issues."
  (when content
    (let ((sanitized content))
      (claudemacs-client--debug-message "Input content: %S" content)
      ;; Remove any remaining control characters that might interfere with terminal input
      (setq sanitized (replace-regexp-in-string "[[:cntrl:]]"
                                                (lambda (match)
                                                  (cond
                                                   ;; Keep normal newlines and tabs
                                                   ((or (string= match "\n") (string= match "\t")) match)
                                                   ;; Remove other control characters
                                                   (t "")))
                                                sanitized))
      ;; Ensure content doesn't end with problematic characters
      ;; This addresses potential issues with Mac's region selection
      (setq sanitized (replace-regexp-in-string "[\r\x0B\x0C\x0E-\x1F]+\\'" "" sanitized))

      (claudemacs-client--debug-message "After control char cleanup: %S" sanitized)
      (claudemacs-client--debug-message "File path pattern match: %s"
                                        (if (string-match-p "~/[^[:space:]]*\\.[a-zA-Z0-9]+\\'" sanitized) "YES" "NO"))
      (claudemacs-client--debug-message "Punctuation pattern match: %s"
                                        (if (string-match-p "[.!?。！？]\\'" sanitized) "YES" "NO"))

      ;; Address Claude Code interpretation issue: if content ends with a file path
      ;; without punctuation, Claude Code may interpret it as a file reference instead
      ;; of text content. Add an explanatory marker to ensure it's treated as text.
      (when (and (string-match-p "~/[^[:space:]]*\\.[a-zA-Z0-9]+\\'" sanitized)
                 (not (string-match-p "[.!?。！？]\\'" sanitized)))
        (claudemacs-client--debug-message "Adding end marker to prevent file path interpretation")
        (setq sanitized (concat sanitized "\n(This text is added by claudemacs-client as a workaround for Claude Code's special interpretation of file paths)")))

      (claudemacs-client--debug-message "Final sanitized content: %S" sanitized)
      ;; Final trim to ensure clean content
      (string-trim sanitized))))


;;;; Target Directory Detection Functions

(defun claudemacs-client--get-target-directory-for-buffer ()
  "Get the target directory for current buffer.
For persistent files, extract directory from filename.
For other buffers, use current `default-directory'."
  (if (and buffer-file-name
           (string-match-p "cec-.+\\.org$" (file-name-nondirectory buffer-file-name)))
      ;; This is a persistent file, extract directory from filename
      (claudemacs-client--decode-full-path
       (file-name-base (file-name-nondirectory buffer-file-name)))
    ;; Use current directory
    default-directory))

(defun claudemacs-client--get-buffer-for-directory (&optional directory)
  "Get the claudemacs buffer for DIRECTORY if it exists and is live.
If DIRECTORY is nil, use current `default-directory'."
  (let ((target-dir (or directory default-directory))
        (matching-buffer nil))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf))
            (default-dir (with-current-buffer buf
                           (when (boundp 'default-directory)
                             default-directory)))
            (eat-mode (with-current-buffer buf
                        (and (boundp 'eat-mode) eat-mode))))
        (when (and (buffer-live-p buf)
                   name       ; Ensure name is not nil
                   ;; Check for directory-specific claudemacs buffer
                   (or (and (string-match-p "^\\*claudemacs:" name)
                            (string-prefix-p (concat "*claudemacs:" target-dir) name))
                       ;; Fallback to generic buffers only if they match directory
                       (and (or (string= name "*claude*")
                                (string= name "*claudemacs*"))
                            (when default-dir
                              (string= (file-truename default-dir)
                                       (file-truename target-dir))))
                       ;; Check for eat-mode buffers with claude in name
                       (and eat-mode
                            (string-match-p "claude" name)
                            (when default-dir
                              (string= (file-truename default-dir)
                                       (file-truename target-dir))))))
          (setq matching-buffer buf)
          (cl-return))))
    matching-buffer))

(defun claudemacs-client--buffer-matches-directory (buffer target-dir)
  "Check if BUFFER's working directory matches TARGET-DIR."
  (with-current-buffer buffer
    (when (boundp 'default-directory)
      (string= (file-truename default-directory)
               (file-truename target-dir)))))

(defun claudemacs-client--can-send-text (&optional directory)
  "Check if text can actually be sent to claudemacs (strict check).
If DIRECTORY is provided, check for claudemacs in that directory.
Otherwise, use current `default-directory'."
  (let ((claude-buffer (claudemacs-client--get-buffer-for-directory directory)))
    (when claude-buffer
      (with-current-buffer claude-buffer
        (and (boundp 'eat--process)
             eat--process
             (process-live-p eat--process))))))

(defun claudemacs-client--send-text (text &optional directory)
  "Send TEXT to claudemacs buffer.
If DIRECTORY is provided, send to claudemacs in that directory.
Otherwise, use current `default-directory'."
  (let ((claude-buffer (claudemacs-client--get-buffer-for-directory directory)))
    (claudemacs-client--debug-message "send-text called with text length: %d, buffer: %s"
                                      (length text) (if claude-buffer (buffer-name claude-buffer) "nil"))
    (when (and claude-buffer
               (with-current-buffer claude-buffer
                 (and (boundp 'eat--process)
                      eat--process
                      (process-live-p eat--process))))
      (claudemacs-client--debug-message "Sending text to claude buffer: %S" (substring text 0 (min 50 (length text))))
      (with-current-buffer claude-buffer
        (eat--send-string eat--process text)
        (eat--send-string eat--process "\r")
        (claudemacs-client--debug-message "Text sent successfully")
        t))))

;;;; Public API

;;;###autoload
(defun claudemacs-client-send-region (start end)
  "Send the text in region from START to END to claudemacs."
  (interactive "r")
  (when (use-region-p)
    (let*
        ((raw-content (buffer-substring-no-properties start end))
         (content (claudemacs-client--sanitize-content (string-trim raw-content)))
         (target-dir (claudemacs-client--get-target-directory-for-buffer)))
      (claudemacs-client--debug-message "Raw content length: %d, trimmed: %d" (length raw-content) (length content))
      (claudemacs-client--debug-message "Content empty?: %s, target-dir: %s" (string-empty-p content) target-dir)
      (if
          (and content (not (string-empty-p content)))
          (progn
            (claudemacs-client--debug-message "Attempting to send content")
            (if
                (claudemacs-client--send-text content target-dir)
                (message "Region sent to Claude (%d characters)" (length content))
              (message "❌ Cannot send - no matching claudemacs buffer found for this directory")))
        (message "No content to send (empty or whitespace only)")))))

;;;###autoload
(defun claudemacs-client-send-buffer ()
  "Send the entire current buffer to claudemacs."
  (interactive)
  (let
      ((content (claudemacs-client--sanitize-content (buffer-substring-no-properties (point-min) (point-max))))
       (target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if
        (claudemacs-client--send-text content target-dir)
        (message "File sent to Claude: %s (%d characters)"
                 (buffer-name) (length content))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-send-rest-of-buffer ()
  "Send rest of buffer from cursor position to end to claudemacs."
  (interactive)
  (let*
      ((raw-content (buffer-substring-no-properties (point) (point-max)))
       (content (claudemacs-client--sanitize-content (string-trim raw-content)))
       (target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if
        (and content (not (string-empty-p content)))
        (if
            (claudemacs-client--send-text content target-dir)
            (message "Rest of buffer sent to Claude (%d characters)"
                     (length content))
          (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))
      (message "No content from cursor to end of file"))))

;;;###autoload
(defun claudemacs-client-send-line ()
  "Send the current line to claudemacs."
  (interactive)
  (let*
      ((line-start (line-beginning-position))
       (line-end (line-end-position))
       (raw-content (buffer-substring-no-properties line-start line-end))
       (content (claudemacs-client--sanitize-content (string-trim raw-content)))
       (target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if
        (and content (not (string-empty-p content)))
        (if
            (claudemacs-client--send-text content target-dir)
            (message "Line sent to Claude (%d characters)" (length content))
          (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))
      (message "No content to send (empty or whitespace only)"))))


(defun claudemacs-client--create-project-input-file (target-directory)
  "Create project input file for TARGET-DIRECTORY from template.
Returns the created file path."
  (let*
      ((file-path (claudemacs-client--get-project-file-path target-directory))
       (template-source (cond
                         ;; If custom template is specified and exists, use it
                         ((and claudemacs-client-template-file
                               (file-exists-p claudemacs-client-template-file))
                          claudemacs-client-template-file)
                         ;; Otherwise, use default.org
                         (t (expand-file-name claudemacs-client-default-template-filename
                                              (claudemacs-client--get-package-directory))))))
    ;; Ensure target directory exists
    (unless (file-exists-p (file-name-directory file-path))
      (make-directory (file-name-directory file-path) t))

    ;; Copy template file to target location
    (copy-file template-source file-path)
    file-path))

;;;###autoload
(defun claudemacs-client-open-project-input (&optional directory)
  "Open or create project input file for DIRECTORY.
If DIRECTORY is nil, use current `default-directory'.
If project input file exists, open it directly.
If not exists, create from template then open."
  (interactive)
  (let*
      ((target-dir (or directory default-directory))
       (file-path (claudemacs-client--get-project-file-path target-dir)))

    ;; Create file if it doesn't exist
    (unless (file-exists-p file-path)
      (claudemacs-client--create-project-input-file target-dir))

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
      (goto-char (point-max))
      (message "Project input file ready: %s" (file-name-nondirectory file-path)))))

;;;###autoload
(defun claudemacs-client-start-claudemacs ()
  "Start claudemacs and change to appropriate directory.
Determines directory from current buffer filename if it's a persistent file.
Checks for existing sessions to prevent double startup."
  (interactive)
  (let*
      ((target-dir (claudemacs-client--get-target-directory-for-buffer))
       (existing-buffer (claudemacs-client--get-buffer-for-directory target-dir))
       (can-send (claudemacs-client--can-send-text target-dir))
       (original-default-directory default-directory))
    ;; Check for existing session
    (cond
     (can-send
      (message
       "Claudemacs session already running in: %s (buffer: %s)"
       target-dir (buffer-name existing-buffer)))
     ;; Dead session exists - offer to restart
     (existing-buffer
      (when
          (y-or-n-p (format "Dead claudemacs session found in %s.  Restart? " target-dir))
        (kill-buffer existing-buffer)
        (claudemacs-client-start-claudemacs)))
     ;; No existing session - start new one
     (t
      (unwind-protect
          (progn
            (cd target-dir)
            (if
                (fboundp 'claudemacs-transient-menu)
                (progn
                  (claudemacs-transient-menu)
                  (message "Started claudemacs in: %s" target-dir))
              (error "Claudemacs-transient-menu not available.  Is claudemacs package loaded?")))
        ;; Restore original directory if startup failed
        (unless (claudemacs-client--can-send-text target-dir)
          (cd original-default-directory)))))))

;;;###autoload
(defun claudemacs-client-setup-window-layout ()
  "Set up window layout with org file on left and claudemacs on right.
This is the author's preference - customize as needed."
  (interactive)
  (let
      ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (let ((claudemacs-buf (claudemacs-client--get-buffer-for-directory target-dir)))
      (if claudemacs-buf
          (switch-to-buffer claudemacs-buf)
        (message "claudemacs buffer not found. Run (claudemacs-client-start-claudemacs) first.")))
    (other-window -1)
    (message "Window layout setup complete")))

;;;###autoload
(defun claudemacs-client-output-template ()
  "Output current template content to a new buffer for customization."
  (interactive)
  (let* ((template-content (claudemacs-client--load-template))
         (template-file (or claudemacs-client-template-file "default"))
         (buffer-name (format "*claudemacs-template-%s*"
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
(defun claudemacs-client-status ()
  "Show detailed diagnostic information for troubleshooting connection issues."
  (interactive)
  (let*
      ((target-dir (claudemacs-client--get-target-directory-for-buffer))
       (claude-buffer (claudemacs-client--get-buffer-for-directory target-dir))
       (can-send (claudemacs-client--can-send-text target-dir))
       (expected-session (concat "*claudemacs:" target-dir "*"))
       (all-buffers (buffer-list))
       (claudemacs-sessions
        (seq-filter
         (lambda (buf)
           (string-match-p "^\\*claudemacs:" (buffer-name buf)))
         all-buffers))
       (other-claude-buffers
        (seq-filter
         (lambda (buf)
           (let
               ((name (buffer-name buf)))
             (or (string= name "*claude*")
                 (string= name "*claudemacs*")
                 (and (with-current-buffer buf
                        (and (boundp 'eat-mode) eat-mode))
                      (string-match-p "claude" name)))))
         all-buffers)))
    (with-output-to-temp-buffer "*claudemacs-client-diagnostic*"
      (princ "═══ claudemacs-client Diagnostic Report ═══\n\n")
      ;; Basic information
      (princ (format "Target directory: %s\n" target-dir))
      (princ (format "Current buffer: %s\n\n" (buffer-name)))
      ;; Session status
      (if
          can-send
          (princ
           (format "[✅] Session status: CONNECTED\n  Active session: %s\n\n"
                   (buffer-name claude-buffer)))
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
           (string-match-p "cec-.+\\.org$" (file-name-nondirectory (buffer-file-name))))
          (let*
              ((encoded-name (file-name-base (file-name-nondirectory (buffer-file-name))))
               (decoded-path (claudemacs-client--decode-full-path encoded-name))
               (re-encoded (claudemacs-client--encode-full-path decoded-path)))
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
      (princ "Found claudemacs sessions:\n")
      (if claudemacs-sessions
          (dolist (buf claudemacs-sessions)
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
        (princ "  (no claudemacs sessions found)\n"))
      ;; Other Claude buffers
      (when other-claude-buffers
        (princ "\nOther Claude-related buffers:\n")
        (dolist (buf other-claude-buffers)
          (let*
              ((name (buffer-name buf))
               (dir
                (with-current-buffer buf
                  (when (boundp 'default-directory)
                    default-directory)))
               (process-status
                (with-current-buffer buf
                  (if
                      (and (boundp 'eat--process)
                           eat--process
                           (process-live-p eat--process))
                      "alive" "dead"))))
            (princ (format "  - %s in %s (process: %s)\n" name (or dir "unknown") process-status)))))
      (princ "\n")
      ;; Recommended actions
      (princ "Recommended actions:\n")
      (cond
       (can-send
        (princ "✓ Connection is working. You can send text to Claude.\n"))
       ((not claudemacs-sessions)
        (princ "1. Start claudemacs in target directory: (claudemacs-client-start-claudemacs)\n")
        (princ "2. Or run: M-x claudemacs-client-start-claudemacs\n"))
       (t
        (princ "1. Start claudemacs for target directory: (claudemacs-client-start-claudemacs)\n")
        (princ "2. Or switch to existing session directory:\n")
        (dolist (buf claudemacs-sessions)
          (let
              ((session-path
                (replace-regexp-in-string
                 "^\\*claudemacs:\\(.*\\)\\*$" "\\1"
                 (buffer-name buf))))
            (princ (format "   - %s\n" session-path))))))
      (princ "\nFor more help: M-x claudemacs-client-open-project-input\n"))))

;;;###autoload
(defun claudemacs-client-toggle-debug-mode ()
  "Toggle debug mode for claudemacs-client operations."
  (interactive)
  (setq claudemacs-client-debug-mode (not claudemacs-client-debug-mode))
  (message "claudemacs-client debug mode: %s"
           (if claudemacs-client-debug-mode "ENABLED" "DISABLED")))


;;;###autoload
(defun claudemacs-client-send-enter ()
  "Send enter key to claudemacs buffer."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (claudemacs-client--can-send-text target-dir)
        (progn
          (claudemacs-client--send-text "" target-dir)
          (message "Sent enter to Claude"))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-send-1 ()
  "Send \\='1\\=' to claudemacs buffer for numbered choice prompt."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (claudemacs-client--can-send-text target-dir)
        (progn
          (claudemacs-client--send-text "1" target-dir)
          (message "Sent '1' to Claude"))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-send-2 ()
  "Send \\='2\\=' to claudemacs buffer for numbered choice prompt."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (claudemacs-client--can-send-text target-dir)
        (progn
          (claudemacs-client--send-text "2" target-dir)
          (message "Sent '2' to Claude"))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-send-3 ()
  "Send \\='3\\=' to claudemacs buffer for numbered choice prompt."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (claudemacs-client--can-send-text target-dir)
        (progn
          (claudemacs-client--send-text "3" target-dir)
          (message "Sent '3' to Claude"))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-enable-debug-mode ()
  "Enable debug mode for claudemacs-client operations."
  (interactive)
  (setq claudemacs-client-debug-mode t)
  (message "claudemacs-client debug mode: ENABLED"))

;;;###autoload
(defun claudemacs-client-disable-debug-mode ()
  "Disable debug mode for claudemacs-client operations."
  (interactive)
  (setq claudemacs-client-debug-mode nil)
  (message "claudemacs-client debug mode: DISABLED"))

(provide 'claudemacs-client)

;;; claudemacs-client.el ends here

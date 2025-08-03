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
(defvar eat--process)
(defvar eat-mode)

;;;; Custom Variables

(defgroup claudemacs-client nil
  "Enhanced client utilities for claudemacs."
  :group 'tools
  :prefix "claudemacs-client-")

(defcustom claudemacs-client-path-separator "--"
  "String to replace path separators (/) in filenames."
  :type 'string
  :group 'claudemacs-client)

(defcustom claudemacs-client-template-language "en"
  "Template language for claudemacs-client input files.
Supported languages: \"en\" (English), \"ja\" (Japanese)"
  :type '(choice (const "en") (const "ja") (const "custom"))
  :group 'claudemacs-client)

(defcustom claudemacs-client-custom-template-path nil
  "Path to custom template file.  When set, overrides language setting completely."
  :type '(choice (const nil) (file :tag "Custom template file"))
  :group 'claudemacs-client)

;;;; Core Functions

;;;; File Naming and Path Management

(defun claudemacs-client--encode-full-path (path)
  "Encode PATH by replacing forward slashes with the configured separator.
Example: \\='/Users/phasetr/project1/\\=' -> \\='cec--Users--phasetr--project1\\='"
  (let
    ((cleaned-path
       (if (string-suffix-p "/" path)
         (substring path 0 -1)
         path)))
    (concat "cec" (replace-regexp-in-string "/" claudemacs-client-path-separator cleaned-path))))

(defun claudemacs-client--decode-full-path (encoded-name)
  "Decode ENCODED-NAME back to original path.
Example: \\='cec--Users--phasetr--project1\\=' -> \\='/Users/phasetr/project1/\\='"
  (when (string-prefix-p "cec" encoded-name)
    (let ((path-part (substring encoded-name 3)))  ; Remove "cec" prefix
      (concat (replace-regexp-in-string claudemacs-client-path-separator "/" path-part) "/"))))

(defun claudemacs-client--get-project-file-path (&optional directory)
  "Get the full path for the project file based on DIRECTORY.
If DIRECTORY is nil, use the current `default-directory'."
  (let*
    ((target-dir (or directory default-directory))
      (encoded-name (claudemacs-client--encode-full-path target-dir))
      (filename (concat encoded-name ".org")))
    (expand-file-name filename target-dir)))

;;;; Template Loading Functions

(defun claudemacs-client--get-template-path (language)
  "Get template file path for LANGUAGE.
Returns nil if template file doesn't exist."
  (let* ((package-dir
          (cond
           ;; Try load-file-name first (when loading with load command)
           (load-file-name (file-name-directory load-file-name))
           ;; Try buffer-file-name (when evaluating in buffer)
           (buffer-file-name (file-name-directory buffer-file-name))
           ;; Fallback to current directory
           (t default-directory)))
         (template-file (format "templates/%s.org" language))
         (template-path (expand-file-name template-file package-dir)))
    (when (file-exists-p template-path)
      template-path)))

(defun claudemacs-client--load-template (language)
  "Load template content for LANGUAGE with fallback mechanism.
Returns template content as string, or nil if no template found."
  (let ((template-path
         (cond
          ;; Custom template file overrides everything
          ((and claudemacs-client-custom-template-path
                (file-exists-p claudemacs-client-custom-template-path))
           claudemacs-client-custom-template-path)
          ;; Language-specific template
          ((claudemacs-client--get-template-path language)
           (claudemacs-client--get-template-path language))
          ;; Fallback to English for "custom" language
          ((and (string= language "custom")
                (claudemacs-client--get-template-path "en"))
           (claudemacs-client--get-template-path "en"))
          ;; Final fallback to English
          ((claudemacs-client--get-template-path "en")
           (claudemacs-client--get-template-path "en"))
          (t nil))))
    (when template-path
      (with-temp-buffer
        (insert-file-contents template-path)
        (buffer-string)))))

;;;; File Template and Content Management

(defun claudemacs-client--get-file-template-with-startup-code (project-path)
  "Generate file template with startup code for PROJECT-PATH.
Now loads template from external file based on language setting."
  (let ((template-content (claudemacs-client--load-template claudemacs-client-template-language)))
    (if template-content
        (format template-content project-path)
      ;; Fallback to basic template if external template loading fails
      (format "* Claude Input File
Project: %s

** Startup Code
(claudemacs-start)

** Layout Setup
(claudemacs-client-setup-window-layout)

** Essential Functions
- Send region: M-x claudemacs-client-send-region
- Send rest of buffer: M-x claudemacs-client-send-rest-of-buffer
- Send buffer: M-x claudemacs-client-send-buffer

** Thought/memo
" project-path))))

(defun claudemacs-client--initialize-project-file (file-path)
  "Initialize a new project file at FILE-PATH with template content."
  (let*
    ((directory (file-name-directory file-path))
      (project-path (claudemacs-client--decode-full-path
                      (file-name-base (file-name-nondirectory file-path)))))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (with-temp-file file-path
      (insert (claudemacs-client--get-file-template-with-startup-code project-path)))
    file-path))

(defun claudemacs-client--find-project-file (file-path)
  "Find or create project file at FILE-PATH and return file buffer."
  (if (file-exists-p file-path)
    (find-file file-path)
    (let ((buffer (find-file file-path)))
      (claudemacs-client--initialize-project-file file-path)
      (revert-buffer t t)
      buffer)))

;;;; Pure Functions for Directory/Buffer Detection

(defun claudemacs-client--extract-directory-pure (buffer-file-name-arg default-directory-arg)
  "Pure function: Extract target directory from BUFFER-FILE-NAME-ARG.
Uses DEFAULT-DIRECTORY-ARG if BUFFER-FILE-NAME-ARG doesn't match pattern.
If BUFFER-FILE-NAME-ARG matches persistent file pattern, decode directory."
  (if (and buffer-file-name-arg
        (string-match-p "cec-.+\\.org$" (file-name-nondirectory buffer-file-name-arg)))
    ;; This is a persistent file, extract directory from filename
    (claudemacs-client--decode-full-path
      (file-name-base (file-name-nondirectory buffer-file-name-arg)))
    ;; Use current directory
    default-directory-arg))

(defun claudemacs-client--buffer-matches-directory-pure (buffer-default-directory target-dir)
  "Pure function: Check if BUFFER-DEFAULT-DIRECTORY matches TARGET-DIR.
Returns t if directories match, nil otherwise."
  (when buffer-default-directory
    (string= (file-truename buffer-default-directory)
      (file-truename target-dir))))

(defun claudemacs-client--find-matching-buffer-pure (buffer-list target-directory)
  "Pure function: Find claudemacs buffer matching TARGET-DIRECTORY.
BUFFER-LIST should contain buffer objects.
Returns matching buffer or nil."
  (cl-find-if
    (lambda (buf-info)
      (let ((name (plist-get buf-info :name))
             (default-dir (plist-get buf-info :default-directory))
             (eat-mode (plist-get buf-info :eat-mode))
             (buffer (plist-get buf-info :buffer)))
        (and (buffer-live-p buffer)
          name       ; Ensure name is not nil
          ;; Check for directory-specific claudemacs buffer
          (or (and (string-match-p "^\\*claudemacs:" name)
                (string-prefix-p (concat "*claudemacs:" target-directory) name))
            ;; Fallback to generic buffers only if they match directory
            (and (or (string= name "*claude*")
                   (string= name "*claudemacs*"))
              (claudemacs-client--buffer-matches-directory-pure default-dir target-directory))
            ;; Check for eat-mode buffers with claude in name
            (and eat-mode
              (string-match-p "claude" name)
              (claudemacs-client--buffer-matches-directory-pure default-dir target-directory))))))
    buffer-list))

(defun claudemacs-client--can-send-text-pure (claude-buffer)
  "Pure function: Check if CLAUDE-BUFFER can receive text.
Returns t if buffer has live eat process, nil otherwise."
  (when claude-buffer
    (with-current-buffer claude-buffer
      (and (boundp 'eat--process)
        eat--process
        (process-live-p eat--process)))))

(defun claudemacs-client--send-text-pure (text claude-buffer)
  "Pure function: Send TEXT to CLAUDE-BUFFER.
Returns t if successful, nil if buffer cannot receive text.
Does not modify global state."
  (when (and claude-buffer (claudemacs-client--can-send-text-pure claude-buffer))
    (with-current-buffer claude-buffer
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\r")
      t)))

;;;; Target Directory Detection Wrapper Functions

(defun claudemacs-client--get-target-directory-for-buffer ()
  "Get the target directory for current buffer.
For persistent files, extract directory from filename.
For other buffers, use current `default-directory'.
Wrapper function that uses pure function internally."
  (claudemacs-client--extract-directory-pure buffer-file-name default-directory))

(defun claudemacs-client--get-buffer-for-directory (&optional directory)
  "Get the claudemacs buffer for DIRECTORY if it exists and is live.
If DIRECTORY is nil, use current `default-directory'.
Wrapper function that uses pure function internally."
  (let*
    ((target-dir (or directory default-directory))
      (buffer-info-list
        (mapcar
          (lambda (buf)
            (list
              :buffer buf
              :name (buffer-name buf)
              :default-directory
              (with-current-buffer buf
                (when (boundp 'default-directory)
                  default-directory))
              :eat-mode
              (with-current-buffer buf
                (and (boundp 'eat-mode) eat-mode))))
          (buffer-list)))
      (match-info (claudemacs-client--find-matching-buffer-pure buffer-info-list target-dir)))
    (when match-info
      (plist-get match-info :buffer))))

(defun claudemacs-client--buffer-matches-directory (buffer target-dir)
  "Check if BUFFER's working directory matches TARGET-DIR.
Wrapper function that uses pure function internally."
  (with-current-buffer buffer
    (when (boundp 'default-directory)
      (claudemacs-client--buffer-matches-directory-pure default-directory target-dir))))

(defun claudemacs-client--can-send-text (&optional directory)
  "Check if text can actually be sent to claudemacs (strict check).
If DIRECTORY is provided, check for claudemacs in that directory.
Otherwise, use current `default-directory'.
Wrapper function that uses pure function internally."
  (let ((claude-buffer (claudemacs-client--get-buffer-for-directory directory)))
    (claudemacs-client--can-send-text-pure claude-buffer)))

(defun claudemacs-client--send-text (text &optional directory)
  "Send TEXT to claudemacs buffer.
If DIRECTORY is provided, send to claudemacs in that directory.
Otherwise, use current `default-directory'.
Wrapper function that uses pure function internally."
  (let ((claude-buffer (claudemacs-client--get-buffer-for-directory directory)))
    (claudemacs-client--send-text-pure text claude-buffer)))

;;;; Public API

;;;###autoload
(defun claudemacs-client-send-region (start end)
  "Send the text in region from START to END to claudemacs."
  (interactive "r")
  (when (use-region-p)
    (let
      ((content (buffer-substring-no-properties start end))
        (target-dir (claudemacs-client--get-target-directory-for-buffer)))
      (if (claudemacs-client--send-text content target-dir)
        (message "Region sent to Claude (%d characters)" (length content))
        (message "❌ Cannot send - no matching claudemacs buffer found for this directory")))))

;;;###autoload
(defun claudemacs-client-send-buffer ()
  "Send the entire current buffer to claudemacs."
  (interactive)
  (let
    ((content (buffer-substring-no-properties (point-min) (point-max)))
      (target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (claudemacs-client--send-text content target-dir)
      (message "File sent to Claude: %s (%d characters)"
        (buffer-name) (length content))
      (message "❌ Cannot send - no matching claudemacs buffer found for this directory"))))

;;;###autoload
(defun claudemacs-client-send-rest-of-buffer ()
  "Send rest of buffer from cursor position to end to claudemacs."
  (interactive)
  (let
    ((content (buffer-substring-no-properties (point) (point-max)))
      (target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (if (or (not content) (string-match-p "\\`[[:space:]]*\\'" content))
      (message "No content from cursor to end of file")
      (if (claudemacs-client--send-text content target-dir)
        (message "Rest of buffer sent to Claude (%d characters)"
          (length content))
        (message "❌ Cannot send - no matching claudemacs buffer found for this directory")))))

;;;###autoload
(defun claudemacs-client-open-project-input (&optional directory)
  "Open or create project input file for DIRECTORY.
If DIRECTORY is nil, use current `default-directory'.
This function finds existing input file or creates new one if needed."
  (interactive)
  (let*
    ((target-dir (or directory default-directory))
      (file-path (claudemacs-client--get-project-file-path target-dir))
      (buffer (claudemacs-client--find-project-file file-path)))
    (with-current-buffer buffer
      ;; Set up org mode
      (when (fboundp 'org-mode)
        (unless (eq major-mode 'org-mode)
          (let ((org-mode-hook nil))
            (ignore org-mode-hook)  ; Suppress unused variable warning
            (org-mode))))
      ;; Set up key bindings
      (claudemacs-client--setup-input-file-keys))
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (message "Project input file ready: %s" (file-name-nondirectory file-path))))

(defun claudemacs-client-send-region-interactive ()
  "Interactive version of send region that handles no region case."
  (interactive)
  (if (use-region-p)
    (claudemacs-client-send-region (region-beginning) (region-end))
    (message "No region selected")))

(defun claudemacs-client--setup-input-file-keys ()
  "Set up key bindings for the Claude input file."
  ;; Set up key bindings using buffer-local keymap
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "C-c C-c") #'claudemacs-client-send-rest-of-buffer)
  (local-set-key (kbd "C-c C-r") #'claudemacs-client-send-region-interactive)
  (local-set-key (kbd "C-c C-b") #'claudemacs-client-send-buffer))

;;;###autoload
(defun claudemacs-client-start ()
  "Start claudemacs and change to appropriate directory.
Determines directory from current buffer filename if it's a persistent file."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (cd target-dir)
    (when (fboundp 'claudemacs-transient-menu)
      (claudemacs-transient-menu))
    (message "Started claudemacs in: %s" target-dir)))

;;;###autoload
(defun claudemacs-client-setup-window-layout ()
  "Set up window layout with org file on left and claudemacs on right.
This is the author's preference - customize as needed."
  (interactive)
  (let ((target-dir (claudemacs-client--get-target-directory-for-buffer)))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (let ((claudemacs-buf (claudemacs-client--get-buffer-for-directory target-dir)))
      (if claudemacs-buf
          (switch-to-buffer claudemacs-buf)
        (message "claudemacs buffer not found. Run (claudemacs-client-start) first.")))
    (other-window -1)
    (message "Window layout setup complete")))

;;;###autoload
(defun claudemacs-client-output-template (&optional language)
  "Output template content to a new buffer for customization.
If LANGUAGE is specified, use that language template.
Otherwise, use current language setting."
  (interactive)
  (let* ((target-language (or language claudemacs-client-template-language))
         (template-content (claudemacs-client--load-template target-language))
         (buffer-name (format "*claudemacs-template-%s*" target-language)))
    (if template-content
        (progn
          (with-output-to-temp-buffer buffer-name
            (princ template-content))
          (with-current-buffer buffer-name
            (when (fboundp 'org-mode)
              (org-mode)))
          (message "Template output to buffer: %s" buffer-name)
          t)
      (message "Template not found for language: %s" target-language)
      nil)))

;;; Debug and Utility Functions

;; These functions are primarily for debugging and troubleshooting.
;; They can be safely removed in production environments if needed.

;;;###autoload
(defun claudemacs-client-status ()
  "Show connection status and diagnostic information."
  (interactive)
  (let* ((target-dir (claudemacs-client--get-target-directory-for-buffer))
          (claude-buffer (claudemacs-client--get-buffer-for-directory target-dir))
          (can-send (claudemacs-client--can-send-text target-dir))
          (claude-buffers
            (seq-filter
              (lambda (buf)
                (string-match-p "\\*claudemacs:" (buffer-name buf)))
              (buffer-list))))
    (with-output-to-temp-buffer "*claudemacs-client-status*"
      (princ "═══ claudemacs-client Status ═══\n\n")
      (princ (format "Current buffer: %s\n" (buffer-name)))
      (princ (format "Target directory: %s\n" target-dir))
      (princ (format "Connection status: %s\n"
               (if claude-buffer
                 (format "✓ Connected (%s)" (buffer-name claude-buffer))
                 "✗ Not connected")))
      (princ (format "Can send text: %s\n\n" (if can-send "YES" "NO")))
      (princ "Available Claude buffers:\n")
      (if claude-buffers
        (dolist (buf claude-buffers)
          (princ (format "  - %s\n" (buffer-name buf))))
        (princ "  (none found)\n"))
      (princ "\n" )
      (princ "Troubleshooting:\n")
      (princ "- If not connected: Start claudemacs in target directory\n")
      (princ "- If can't send: Check that Claude buffer has active process\n"))))

(provide 'claudemacs-client)

;;; claudemacs-client.el ends here

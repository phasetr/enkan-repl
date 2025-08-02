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
;; - Dedicated input buffer with org-mode support
;; - History management for sent content
;; - Multi-project support (planned)
;; - Enhanced key bindings for efficient workflow
;;
;; Installation:
;; Add to your init.el:
;;   (require 'claudemacs-client)
;;
;; Usage:
;; M-x claudemacs-client-create-input-buffer
;; C-M-d (global binding)
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

(defcustom claudemacs-client-input-buffer-name "*Claude Input*"
  "Name of the dedicated input buffer."
  :type 'string
  :group 'claudemacs-client)

(defcustom claudemacs-client-max-history-size 50
  "Maximum number of items to keep in input history."
  :type 'integer
  :group 'claudemacs-client)

(defcustom claudemacs-client-use-persistent-files t
  "Whether to use persistent files instead of temporary buffers."
  :type 'boolean
  :group 'claudemacs-client)

(defcustom claudemacs-client-project-file-directory nil
  "Directory to store project files.  nil means use project root."
  :type '(choice (const :tag "Project Root" nil)
                 (string :tag "Custom Directory"))
  :group 'claudemacs-client)

(defcustom claudemacs-client-auto-save-interval 30
  "Auto-save interval in seconds."
  :type 'integer
  :group 'claudemacs-client)

(defcustom claudemacs-client-path-separator "--"
  "String to replace path separators (/) in filenames."
  :type 'string
  :group 'claudemacs-client)

(defvar claudemacs-client-input-history '()
  "History of texts sent to claudemacs.")

;;;; Core Functions

;;;; File Naming and Path Management

(defun claudemacs-client--encode-full-path (path)
  "Encode PATH by replacing forward slashes with the configured separator.
Example: \\='/Users/sekine/project1/\\=' -> \\='cec--Users--sekine--project1\\='"
  (let ((cleaned-path (if (string-suffix-p "/" path)
                          (substring path 0 -1)
                        path)))
    (concat "cec" (replace-regexp-in-string "/" claudemacs-client-path-separator cleaned-path))))

(defun claudemacs-client--decode-full-path (encoded-name)
  "Decode ENCODED-NAME back to original path.
Example: \\='cec--Users--sekine--project1\\=' -> \\='/Users/sekine/project1/\\='"
  (when (string-prefix-p "cec" encoded-name)
    (let ((path-part (substring encoded-name 3)))  ; Remove "cec" prefix
      (concat (replace-regexp-in-string claudemacs-client-path-separator "/" path-part) "/"))))

(defun claudemacs-client--get-project-file-path (&optional directory)
  "Get the full path for the project file based on DIRECTORY.
If DIRECTORY is nil, use the current `default-directory'.
If `claudemacs-client-project-file-directory' is set, use that as base."
  (let* ((target-dir (or directory default-directory))
         (encoded-name (claudemacs-client--encode-full-path target-dir))
         (filename (concat encoded-name ".org"))
         (base-dir (or claudemacs-client-project-file-directory target-dir)))
    (expand-file-name filename base-dir)))

;;;; File Template and Content Management

(defun claudemacs-client--get-file-template-with-startup-code (project-path)
  "Generate file template with startup code for PROJECT-PATH."
  (format "* Claude Input Buffer

Project: %s

** Startup Code (C-c C-e to evaluate)
#+begin_src elisp
(progn
  (cd \"%s\")
  (claudemacs-transient-menu))
#+end_src

** Layout Setup (C-c C-e to evaluate)
#+begin_src elisp
(progn
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (let ((claudemacs-buf (get-buffer \"*claudemacs:%s*\")))
    (if claudemacs-buf
        (switch-to-buffer claudemacs-buf)
      (message \"claudemacs buffer not found. Run startup code first.\")))
  (other-window -1))
#+end_src

** Usage
Write your text here and use the following keys:

- C-c C-c: Send from cursor to end
- C-c C-r: Send selected region
- C-c C-b: Send entire buffer
- C-c C-h: Insert from history

-----

" project-path project-path project-path))

(defun claudemacs-client--create-project-file (file-path)
  "Create a new project file at FILE-PATH with template content."
  (let* ((directory (file-name-directory file-path))
         (project-path (claudemacs-client--decode-full-path
                       (file-name-base (file-name-nondirectory file-path)))))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (with-temp-file file-path
      (insert (claudemacs-client--get-file-template-with-startup-code project-path)))
    file-path))

(defun claudemacs-client--load-project-file (file-path)
  "Load or create project file at FILE-PATH and return buffer."
  (if (file-exists-p file-path)
      (find-file file-path)
    (let ((buffer (find-file file-path)))
      (claudemacs-client--create-project-file file-path)
      (revert-buffer t t)
      buffer)))

(defun claudemacs-client--save-project-file ()
  "Save current buffer if it's a project file."
  (when (and buffer-file-name
             (string-match-p "cec-.+\\.org$" (file-name-nondirectory buffer-file-name)))
    (save-buffer)))

(defun claudemacs-client--setup-auto-save ()
  "Set up auto-save timer for current buffer."
  (when (and claudemacs-client-auto-save-interval
             (> claudemacs-client-auto-save-interval 0))
    (run-with-timer claudemacs-client-auto-save-interval
                    claudemacs-client-auto-save-interval
                    'claudemacs-client--save-project-file)))

;;;; Mode Line Status Display

(defvar claudemacs-client-connection-status nil
  "Current connection status for mode line display.")

(defun claudemacs-client--update-connection-status ()
  "Update connection status for mode line display."
  (setq claudemacs-client-connection-status
        (if (claudemacs-client--can-send-text) "[OK]Claude" "[NO]Claude"))
  (force-mode-line-update))

;; Define custom faces with strong colors to override UI themes
(defface claudemacs-client-connected-face
  '((t (:foreground "#00ff00" :weight bold :inherit mode-line)))
  "Face for claudemacs connected status."
  :group 'claudemacs-client)

(defface claudemacs-client-disconnected-face
  '((t (:foreground "#ff0000" :weight bold :inherit mode-line)))
  "Face for claudemacs disconnected status."
  :group 'claudemacs-client)

(defun claudemacs-client--mode-line-status ()
  "Return mode line status string with appropriate face."
  (when claudemacs-client-connection-status
    (if (string-prefix-p "[OK]" claudemacs-client-connection-status)
        (propertize claudemacs-client-connection-status 
                   'face 'claudemacs-client-connected-face)
      (propertize claudemacs-client-connection-status 
                 'face 'claudemacs-client-disconnected-face))))

(define-minor-mode claudemacs-client-mode
  "Minor mode for claudemacs-client with mode line status display."
  :lighter (:eval (claudemacs-client--mode-line-status))
  :global nil
  (if claudemacs-client-mode
      (progn
        (claudemacs-client--update-connection-status)
        ;; 定期的にステータスを更新（60秒間隔）
        (run-with-timer 60 60 'claudemacs-client--update-connection-status))
    (setq claudemacs-client-connection-status nil)))

(defun claudemacs-client--get-buffer ()
  "Get the claudemacs buffer if it exists and is live."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (buffer-live-p buf)
                       (let ((name (buffer-name buf)))
                         (or (string= name "*claude*")
                             (string= name "*claudemacs*")
                             (string-match-p "^\\*claudemacs:" name)
                             (and (boundp 'claudemacs-mode) claudemacs-mode)
                             (and (boundp 'eat-mode) eat-mode
                                  (string-match-p "claude" name)))))))
              (buffer-list)))

(defun claudemacs-client--can-send-text ()
  "Check if text can actually be sent to claudemacs (strict check)."
  (when-let ((claude-buffer (claudemacs-client--get-buffer)))
    (with-current-buffer claude-buffer
      (and (boundp 'eat--process) 
           eat--process
           (process-live-p eat--process)))))

(defun claudemacs-client--send-text (text)
  "Send TEXT to claudemacs buffer."
  (if (claudemacs-client--can-send-text)
      (when-let ((claude-buffer (claudemacs-client--get-buffer)))
        (with-current-buffer claude-buffer
          (eat--send-string eat--process text)
          (eat--send-string eat--process "\r")
          (claudemacs-client--save-to-history text)
          t))
    nil))

(defun claudemacs-client--save-to-history (text)
  "Save TEXT to input history, maintaining maximum size."
  (push text claudemacs-client-input-history)
  (when (> (length claudemacs-client-input-history)
           claudemacs-client-max-history-size)
    (setq claudemacs-client-input-history
          (butlast claudemacs-client-input-history))))

;;;; Public API

;;;###autoload
(defun claudemacs-client-send-region (start end)
  "Send the text in region from START to END to claudemacs."
  (interactive "r")
  (when (use-region-p)
    (let ((content (buffer-substring-no-properties start end)))
      (if (claudemacs-client--send-text content)
          (message "Region sent to Claude (%d characters)" (length content))
        (message "❌ Cannot send - claudemacs not connected or not running")))))

;;;###autoload
(defun claudemacs-client-send-buffer ()
  "Send the entire current buffer to claudemacs."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (if (claudemacs-client--send-text content)
        (message "Buffer sent to Claude: %s (%d characters)"
                 (buffer-name) (length content))
      (message "❌ Cannot send - claudemacs not connected or not running"))))

;;;###autoload
(defun claudemacs-client-send-from-cursor ()
  "Send text from cursor position to end of buffer to claudemacs."
  (interactive)
  (let ((content (buffer-substring-no-properties (point) (point-max))))
    (if (or (not content) (string-match-p "\\`[[:space:]]*\\'" content))
        (message "No content from cursor to end of buffer")
      (if (claudemacs-client--send-text content)
          (message "Text from cursor sent to Claude (%d characters)"
                   (length content))
        (message "❌ Cannot send - claudemacs not connected or not running")))))

;;;###autoload
(defun claudemacs-client-create-persistent-buffer (&optional directory)
  "Create or switch to a persistent project file for DIRECTORY.
If DIRECTORY is nil, use current `default-directory'.
This replaces the temporary buffer approach with persistent files."
  (interactive)
  (let* ((target-dir (or directory default-directory))
         (file-path (claudemacs-client--get-project-file-path target-dir))
         (buffer (claudemacs-client--load-project-file file-path)))
    (with-current-buffer buffer
      ;; Set up org mode
      (when (fboundp 'org-mode)
        (unless (eq major-mode 'org-mode)
          (let ((org-mode-hook nil))
            (ignore org-mode-hook)  ; Suppress unused variable warning
            (org-mode))))
      
      ;; Set up key bindings
      (claudemacs-client--setup-input-buffer-keys)
      
      ;; Enable minor mode for status display
      (claudemacs-client-mode 1)
      
      ;; Set up auto-save
      (when claudemacs-client-auto-save-interval
        (claudemacs-client--setup-auto-save)))
    
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (message "Persistent Claude buffer ready: %s" (file-name-nondirectory file-path))))

;;;###autoload
(defun claudemacs-client-create-input-buffer ()
  "Create or switch to input buffer.
Uses persistent files if `claudemacs-client-use-persistent-files' is t,
otherwise uses traditional temporary buffer."
  (interactive)
  (if claudemacs-client-use-persistent-files
      (claudemacs-client-create-persistent-buffer)
    (claudemacs-client--create-traditional-input-buffer)))

(defun claudemacs-client--create-traditional-input-buffer ()
  "Create or switch to a traditional dedicated input buffer.
This function creates a buffer for composing Claude text."
  (let ((buffer-name claudemacs-client-input-buffer-name))
    (let ((input-buffer (get-buffer-create buffer-name)))
      (with-current-buffer input-buffer
        ;; Set up org mode for better editing experience
        (when (fboundp 'org-mode)
          (unless (eq major-mode 'org-mode)
            ;; Temporarily disable org-mode-hook to avoid side effects
            (let ((org-mode-hook nil))
              (ignore org-mode-hook)  ; Suppress unused variable warning
              (org-mode))))

        ;; Set up key bindings
        (claudemacs-client--setup-input-buffer-keys)

        ;; Enable minor mode for status display
        (claudemacs-client-mode 1)

        ;; Add helpful header if buffer is empty
        (when (= (point-min) (point-max))
          (insert "* Claude Input Buffer\n\n")
          (insert "Write your text here and use the following keys:\n\n")
          (insert "- C-c C-c: Send from cursor to end\n")
          (insert "- C-c C-r: Send selected region\n")
          (insert "- C-c C-b: Send entire buffer\n")
          (insert "- C-c C-h: Insert from history\n\n")
          (insert "-----\n\n")))

      (switch-to-buffer input-buffer)
      (goto-char (point-max))
      (message "Claude input buffer ready. C-c C-c to send from cursor."))))


(defun claudemacs-client-send-region-interactive ()
  "Interactive version of send region that handles no region case."
  (interactive)
  (if (use-region-p)
      (claudemacs-client-send-region (region-beginning) (region-end))
    (message "No region selected")))

(defun claudemacs-client--setup-input-buffer-keys ()
  "Set up key bindings for the Claude input buffer."
  ;; Set up key bindings using buffer-local keymap
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "C-c C-c") #'claudemacs-client-send-from-cursor)
  (local-set-key (kbd "C-c C-r") #'claudemacs-client-send-region-interactive)
  (local-set-key (kbd "C-c C-b") #'claudemacs-client-send-buffer)
  (local-set-key (kbd "C-c C-h") #'claudemacs-client-insert-from-history))

;;;###autoload
(defun claudemacs-client-insert-from-history ()
  "Insert previously sent text from history."
  (interactive)
  (if claudemacs-client-input-history
      (let ((selected (completing-read "Insert from history: "
                                       claudemacs-client-input-history)))
        (insert selected))
    (message "No history available")))

;;;###autoload
(defun claudemacs-client-refresh-status ()
  "Manually refresh connection status and mode line display."
  (interactive)
  (claudemacs-client--update-connection-status)
  (message "Connection status refreshed: %s" 
           (if claudemacs-client-connection-status
               claudemacs-client-connection-status
             "No status")))

;;;###autoload
(defun claudemacs-client-show-status ()
  "Show status of claudemacs-client."
  (interactive)
  (let ((claude-buffer (claudemacs-client--get-buffer)))
    (message "claudemacs-client | History: %d items | Connection: %s"
             (length claudemacs-client-input-history)
             (if claude-buffer
                 (format "Connected (%s)" (buffer-name claude-buffer))
               "Not connected"))))

;;;; Setup

;; Global key binding
;;;###autoload
(define-key global-map (kbd "C-M-d") #'claudemacs-client-create-input-buffer)


;;;; Mode Line Integration

;;;###autoload
(defun claudemacs-client-setup-mode-line ()
  "Set up mode line display for claudemacs-client status."
  (interactive)
  (setq-default mode-line-misc-info
    (append mode-line-misc-info
            '((:eval (when (and (boundp 'claudemacs-client-mode)
                                claudemacs-client-mode
                                (boundp 'claudemacs-client-connection-status)
                                claudemacs-client-connection-status)
                       (concat " " (claudemacs-client--mode-line-status)))))))
  (force-mode-line-update t)
  (message "claudemacs-client mode line display enabled"))

;; Automatically set up mode line when package is loaded
(claudemacs-client-setup-mode-line)

;;;###autoload
(defun claudemacs-client-version ()
  "Show the version of claudemacs-client."
  (interactive)
  (message "claudemacs-client version 0.0.0"))

;;;; Debug and Utility Functions

;; These functions are primarily for debugging and troubleshooting.
;; They can be safely removed in production environments if needed.

;;;###autoload
(defun claudemacs-client-debug-mode-line ()
  "Debug mode line configuration and display issues."
  (interactive)
  (message "=== Mode Line Debug Information ===")
  (message "Current mode-line-format: %S" mode-line-format)
  (message "minor-mode-alist: %S" (assq 'claudemacs-client-mode minor-mode-alist))
  (message "claudemacs-client-mode: %S" claudemacs-client-mode)
  (message "claudemacs-client-connection-status: %S" claudemacs-client-connection-status)
  (message "mode-line-misc-info: %S" mode-line-misc-info)
  (message "Current buffer minor modes: %S" 
           (seq-filter (lambda (mode) (and (boundp (car mode)) (symbol-value (car mode))))
                      minor-mode-alist))
  (let ((test-status "[OK]Claude"))
    (message "Test status string: %S" test-status)
    (message "Test with face: %S" 
             (propertize test-status 'face 'claudemacs-client-connected-face)))
  (message "Face attributes - connected: %S" 
           (face-all-attributes 'claudemacs-client-connected-face))
  (message "Face attributes - disconnected: %S" 
           (face-all-attributes 'claudemacs-client-disconnected-face))
  ;; Check for problematic characters in mode line
  (message "Mode line string analysis:")
  (when claudemacs-client-connection-status
    (let ((status-str claudemacs-client-connection-status))
      (message "Status string length: %d" (length status-str))
      (message "Status string bytes: %S" (string-to-list status-str))
      (message "Status string with properties: %S" 
               (claudemacs-client--mode-line-status)))))

;;;###autoload
(defun claudemacs-client-toggle-connection-status ()
  "Toggle connection status for testing purposes."
  (interactive)
  (if (and claudemacs-client-connection-status
           (string-prefix-p "[OK]" claudemacs-client-connection-status))
      (progn
        (setq claudemacs-client-connection-status "[NO]Claude")
        (message "Connection status: DISCONNECTED (red)"))
    (progn
      (setq claudemacs-client-connection-status "[OK]Claude")
      (message "Connection status: CONNECTED (green)")))
  (force-mode-line-update))

;;;###autoload
(defun claudemacs-client-test-mode-line-colors ()
  "Test mode line color display for debugging."
  (interactive)
  (message "Testing mode line colors...")
  (setq claudemacs-client-connection-status "[OK]Claude")
  (force-mode-line-update)
  (message "Connected status (green): %s" (claudemacs-client--mode-line-status))
  (sit-for 3)
  (setq claudemacs-client-connection-status "[NO]Claude") 
  (force-mode-line-update)
  (message "Disconnected status (red): %s" (claudemacs-client--mode-line-status))
  (sit-for 3)
  (claudemacs-client--update-connection-status)
  (message "Mode line color test completed. Status restored to actual state."))

;;;###autoload
(defun claudemacs-client-force-face-colors ()
  "Force face colors to override strong UI themes."
  (interactive)
  (set-face-attribute 'claudemacs-client-connected-face nil
                      :foreground "#00ff00" 
                      :weight 'bold)
  (set-face-attribute 'claudemacs-client-disconnected-face nil
                      :foreground "#ff0000"
                      :weight 'bold)
  (force-mode-line-update)
  (message "Face colors forced to override strong themes"))

(provide 'claudemacs-client)

;;; claudemacs-client.el ends here

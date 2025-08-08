;;; enkan-repl-backend-claudemacs.el --- Claudemacs backend for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the claudemacs backend implementation for enkan-repl.
;; It maintains compatibility with the existing claudemacs-repl functionality.

;;; Code:

(require 'claudemacs nil t)

;;; Claudemacs Backend Implementation

(defun enkan-repl-backend-claudemacs-create (directory session-id)
  "Create a new claudemacs session for DIRECTORY with SESSION-ID."
  (let* ((buffer-name (format "*enkan-%s*" session-id))
         (default-directory (expand-file-name directory)))
    ;; Use claudemacs-start to create session
    (claudemacs-start)
    ;; Find the created buffer (might have different name)
    (let ((created-buffer (or (get-buffer "*claudemacs*")
                              (get-buffer (format "*claudemacs:%s*" directory))
                              (get-buffer "*claude*"))))
      (when created-buffer
        ;; Rename to our standard naming
        (with-current-buffer created-buffer
          (rename-buffer buffer-name t))
        ;; Set up bell handler for notifications
        (require 'enkan-repl-backend)
        (enkan-repl-backend-setup-bell-handler created-buffer))
      (get-buffer buffer-name))))

(defun enkan-repl-backend-claudemacs-send (text buffer)
  "Send TEXT to claudemacs BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'eat--process)
               eat--process
               (process-live-p eat--process))
      ;; Send text without newline
      (eat--send-string eat--process text)
      ;; Send C-m (Return key) to submit the input
      (eat--send-string eat--process "\C-m")
      ;; Try to keep cursor at bottom - eat-mode needs special handling
      (when (get-buffer-window buffer)
        (with-selected-window (get-buffer-window buffer)
          (goto-char (point-max))
          (end-of-buffer))))))

(defun enkan-repl-backend-claudemacs-alive-p (buffer)
  "Check if claudemacs BUFFER is alive."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'eat--process)
              eat--process
              (process-live-p eat--process)))))

(defun enkan-repl-backend-claudemacs-finish (buffer)
  "Finish claudemacs BUFFER session and clean up."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Send exit command to the shell
      (when (and (boundp 'eat--process)
                 eat--process
                 (process-live-p eat--process))
        ;; Send exit command
        (eat--send-string eat--process "exit\n")
        ;; Give it a moment to exit gracefully
        (sit-for 0.1)
        ;; Force kill if still alive
        (when (process-live-p eat--process)
          (kill-process eat--process))))))

(defun enkan-repl-backend-claudemacs-get-content (buffer)
  "Get content from claudemacs BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

(provide 'enkan-repl-backend-claudemacs)
;;; enkan-repl-backend-claudemacs.el ends here

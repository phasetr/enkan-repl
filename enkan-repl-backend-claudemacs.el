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
          (rename-buffer buffer-name t)))
      (get-buffer buffer-name))))

(defun enkan-repl-backend-claudemacs-send (text buffer)
  "Send TEXT to claudemacs BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'eat--process)
               eat--process
               (process-live-p eat--process))
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\n")
      ;; Try to keep cursor at bottom
      (goto-char (point-max))
      (recenter -1))))

(defun enkan-repl-backend-claudemacs-alive-p (buffer)
  "Check if claudemacs BUFFER is alive."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'eat--process)
              eat--process
              (process-live-p eat--process)))))

(defun enkan-repl-backend-claudemacs-get-content (buffer)
  "Get content from claudemacs BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

(provide 'enkan-repl-backend-claudemacs)
;;; enkan-repl-backend-claudemacs.el ends here

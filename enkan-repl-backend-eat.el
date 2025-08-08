;;; enkan-repl-backend-eat.el --- Eat backend for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the eat backend implementation for enkan-repl.
;; It uses the eat terminal emulator to run arbitrary commands.

;;; Code:

(require 'eat nil t)

;;; Eat Backend Implementation

(defun enkan-repl-backend-eat-create (directory session-id)
  "Create a new eat session for DIRECTORY with SESSION-ID."
  (let* ((buffer-name (format "*enkan-%s*" session-id))
         (default-directory (expand-file-name directory)))
    ;; Create new eat buffer
    (save-window-excursion
      (let ((eat-buffer-name buffer-name))
        (eat)))
    (get-buffer buffer-name)))

(defun enkan-repl-backend-eat-send (text buffer)
  "Send TEXT to eat BUFFER."
  (with-current-buffer buffer
    (when (and (boundp 'eat--process)
               eat--process
               (process-live-p eat--process))
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\n")
      ;; Try to keep cursor at bottom
      (goto-char (point-max))
      (recenter -1))))

(defun enkan-repl-backend-eat-alive-p (buffer)
  "Check if eat BUFFER is alive."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'eat--process)
              eat--process
              (process-live-p eat--process)))))

(defun enkan-repl-backend-eat-get-content (buffer)
  "Get content from eat BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

(provide 'enkan-repl-backend-eat)
;;; enkan-repl-backend-eat.el ends here

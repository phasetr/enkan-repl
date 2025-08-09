;;; enkan-repl-backend-eat.el --- Eat backend for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the eat backend implementation for enkan-repl.
;; It uses the eat terminal emulator to run arbitrary commands.

;;; Code:

(require 'eat nil t)

(defvar-local enkan-repl-backend-eat-idle-timer nil
  "Timer for detecting idle state in eat buffer.")

(defcustom enkan-repl-backend-eat-idle-seconds 3
  "Seconds of inactivity before triggering notification."
  :type 'integer
  :group 'enkan-repl-backend)

(defvar-local enkan-repl-backend-eat-last-output-time nil
  "Time of last output in eat buffer.")

(defun enkan-repl-backend-eat-reset-idle-timer (buffer)
  "Reset idle timer for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq enkan-repl-backend-eat-last-output-time (current-time))
      (when enkan-repl-backend-eat-idle-timer
        (cancel-timer enkan-repl-backend-eat-idle-timer))
      (setq enkan-repl-backend-eat-idle-timer
            (run-with-timer enkan-repl-backend-eat-idle-seconds nil
                           #'enkan-repl-backend-eat-check-idle buffer)))))

(defun enkan-repl-backend-eat-check-idle (buffer)
  "Check if BUFFER has been idle and trigger notification."
  (when (and enkan-repl-notify-on-completion
             (buffer-live-p buffer))
    (require 'enkan-repl-backend)
    (enkan-repl-backend--system-notification
     "Claude Code is ready for input"
     "Task Completed")))

;;; Eat Backend Implementation

(defun enkan-repl-backend-eat-create (directory session-id)
  "Create a new eat session for DIRECTORY with SESSION-ID."
  (let* ((buffer-name (format "*enkan-%s*" session-id))
         (default-directory (expand-file-name directory)))
    ;; Create new eat buffer
    (save-window-excursion
      (let ((eat-buffer-name buffer-name))
        (eat)))
    (let ((buffer (get-buffer buffer-name)))
      ;; Set up bell handler for notifications
      (when buffer
        (require 'enkan-repl-backend)
        (enkan-repl-backend-setup-bell-handler buffer)
        (with-current-buffer buffer
          (add-hook 'after-change-functions
                    (lambda (&rest _) (enkan-repl-backend-eat-reset-idle-timer buffer))
                    nil t))
        (enkan-repl-backend-eat-reset-idle-timer buffer))
      buffer)))

(defun enkan-repl-backend-eat-send (text buffer)
  "Send TEXT to eat BUFFER."
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

(defun enkan-repl-backend-eat-alive-p (buffer)
  "Check if eat BUFFER is alive."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'eat--process)
              eat--process
              (process-live-p eat--process)))))

(defun enkan-repl-backend-eat-finish (buffer)
  "Finish eat BUFFER session and clean up."
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

(defun enkan-repl-backend-eat-get-content (buffer)
  "Get content from eat BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun enkan-repl-backend-eat-test-notification ()
  "Test function to manually trigger notification."
  (interactive)
  (message "Testing notification...")
  (require 'enkan-repl-backend)
  (enkan-repl-backend--system-notification
   "Test: Claude Code is ready"
   "Test Notification"))

(provide 'enkan-repl-backend-eat)
;;; enkan-repl-backend-eat.el ends here

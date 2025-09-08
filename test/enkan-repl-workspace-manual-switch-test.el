;;; enkan-repl-workspace-manual-switch-test.el --- Test manual workspace switch -*- lexical-binding: t -*-

;;; Commentary:
;; Test what happens when user manually switches workspace without using command

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-manual-workspace-switch-loses-sessions ()
  "Test that manually switching workspace ID loses session info."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (default-directory "/path/to/er/"))
    
    ;; Create WS 01 with session
    (enkan-repl--setup-create-workspace-with-project t nil)
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    (enkan-repl--save-workspace-state)
    (message "WS 01 created with sessions: %S" enkan-repl-session-list)
    
    ;; Create WS 02 with session
    (enkan-repl--setup-create-workspace-with-project t nil)
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    ;; NOTE: User might forget to save here!
    (message "WS 02 sessions in memory: %S" enkan-repl-session-list)
    
    ;; User manually switches to another workspace WITHOUT saving current
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Now switch back to WS 02
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; WS 02 sessions would be lost if not saved
    (let ((ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (message "WS 02 state after manual switch: %S" ws02-state)
      ;; If session wasn't saved, it would be 0
      (let ((session-count (length (plist-get ws02-state :session-list))))
        (message "Session count: %d" session-count)
        ;; This would be 0 if the session wasn't saved!
        (should (= 0 session-count))))))

(ert-deftest test-workspace-not-saved-after-eat-start ()
  "Test that workspace might not be saved after eat starts."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (default-directory "/path/to/er/")
        (mock-eat-buffer nil))
    
    ;; Create WS 02
    (enkan-repl--setup-create-workspace-with-project t nil)
    (should (string= "01" enkan-repl--current-workspace))
    
    ;; Mock eat start WITHOUT automatic save
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
               (lambda ()
                 (setq mock-eat-buffer (generate-new-buffer "*eat*"))
                 mock-eat-buffer))
              ((symbol-function 'require)
               (lambda (_) t))
              ;; Override save to do nothing
              ((symbol-function 'enkan-repl--save-workspace-state)
               (lambda (&optional _)
                 (message "Save called but doing nothing!")
                 nil)))
      
      ;; Start eat (but save is mocked to do nothing)
      (enkan-repl-start-eat)
      
      ;; Session is registered in memory
      (should (equal '((1 . "er")) enkan-repl-session-list))
      
      ;; But NOT saved to workspace
      (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces))))
        (message "Workspace state: %S" ws-state)
        ;; Session list would be nil because save was mocked
        (should (null (plist-get ws-state :session-list)))))
    
    ;; Clean up
    (when (buffer-live-p mock-eat-buffer)
      (kill-buffer mock-eat-buffer))))

(provide 'enkan-repl-workspace-manual-switch-test)
;;; enkan-repl-workspace-manual-switch-test.el ends here
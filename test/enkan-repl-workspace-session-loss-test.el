;;; enkan-repl-workspace-session-loss-test.el --- Test for session loss bug -*- lexical-binding: t -*-

;;; Commentary:
;; Test that reproduces the session loss when switching workspaces

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-session-loss-on-switch ()
  "Test that sessions are NOT lost when switching workspaces."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory "/path/to/er/"))
    
    (message "\n=== Creating WS 01 with er project ===")
    
    ;; Create workspace 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl--current-project "er")
    
    ;; Simulate starting eat session (as would happen in real use)
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    (enkan-repl--save-workspace-state "01")
    
    (message "WS 01 after setup:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Saved state: %S" (cdr (assoc "01" enkan-repl--workspaces)))
    
    ;; Verify WS 01 has session
    (should (equal '((1 . "er")) enkan-repl-session-list))
    
    (message "\n=== Creating WS 02 with er project ===")
    
    ;; Create workspace 02
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl--current-project "er")
    
    ;; Reset sessions for new workspace
    (setq enkan-repl-session-list nil)
    (setq enkan-repl--session-counter 0)
    
    ;; Simulate starting eat session in WS 02
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    (enkan-repl--save-workspace-state "02")
    
    (message "WS 02 after setup:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Saved state: %S" (cdr (assoc "02" enkan-repl--workspaces)))
    
    ;; Verify WS 02 has session
    (should (equal '((1 . "er")) enkan-repl-session-list))
    
    (message "\n=== Switching from WS 02 to WS 01 ===")
    
    ;; Simulate enkan-repl-workspace-switch from 02 to 01
    (enkan-repl--save-workspace-state) ; Save current (02)
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    (message "After switch to WS 01:")
    (message "  Sessions: %S" enkan-repl-session-list)
    
    ;; WS 01 should have its session
    (should (equal '((1 . "er")) enkan-repl-session-list))
    
    (message "\n=== Switching back to WS 02 ===")
    
    ;; Switch back to WS 02
    (enkan-repl--save-workspace-state) ; Save current (01)
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    (message "After switch to WS 02:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Saved state in workspaces: %S" enkan-repl--workspaces)
    
    ;; WS 02 should STILL have its session
    (should (equal '((1 . "er")) enkan-repl-session-list))
    
    ;; Check both workspaces have sessions in saved state
    (let ((ws01-state (cdr (assoc "01" enkan-repl--workspaces)))
          (ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (message "Final WS 01 state: %S" ws01-state)
      (message "Final WS 02 state: %S" ws02-state)
      (should (equal '((1 . "er")) (plist-get ws01-state :session-list)))
      (should (equal '((1 . "er")) (plist-get ws02-state :session-list))))))

(ert-deftest test-workspace-list-shows-zero-sessions ()
  "Test the exact scenario where workspace-list shows 0 sessions."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil))
    
    ;; Create WS 01 with session
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (enkan-repl--save-workspace-state "01")
    
    ;; Create WS 02 with session
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    ;; MISSING: Not saving WS 02 state!
    ;; (enkan-repl--save-workspace-state "02")
    
    ;; Now check what workspace-list would show
    (let ((ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (message "WS 02 state (should have default empty state): %S" ws02-state)
      ;; State should have empty defaults from add-workspace
      (should (equal 0 (plist-get ws02-state :session-counter)))
      (should (null (plist-get ws02-state :session-list))))))

(ert-deftest test-workspace-switch-without-save ()
  "Test switching workspace without saving current state first."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0))
    
    ;; Create WS 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (enkan-repl--save-workspace-state "01")
    
    ;; Create WS 02 but DON'T save
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "project-b")))
    ;; NOT SAVING!
    
    ;; Switch to WS 01 without saving WS 02
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; WS 02 state should have empty defaults
    (let ((ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (should (equal 0 (plist-get ws02-state :session-counter)))
      (should (null (plist-get ws02-state :session-list))))
    
    ;; If we switch back to WS 02, sessions would be lost
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; Sessions would be nil because state was never saved
    (should (null enkan-repl-session-list))))

(provide 'enkan-repl-workspace-session-loss-test)
;;; enkan-repl-workspace-session-loss-test.el ends here
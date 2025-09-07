;;; enkan-repl-workspace-actual-save-test.el --- Test actual save behavior -*- lexical-binding: t -*-

;;; Commentary:
;; Test the actual save behavior when switching workspaces

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-switch-saves-current-state ()
  "Test that enkan-repl-workspace-switch saves current state."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil))
    
    ;; Create WS 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (enkan-repl--save-workspace-state "01")
    
    ;; Create WS 02
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "project-b") (2 . "project-b")))
    (setq enkan-repl--session-counter 2)
    (setq enkan-repl--current-project "project-b")
    ;; Don't save yet - simulate state only in memory
    
    (message "Before switch - WS 02 in memory:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Counter: %d" enkan-repl--session-counter)
    (message "  Saved state: %S" (cdr (assoc "02" enkan-repl--workspaces)))
    
    ;; Now simulate the switch that should save WS 02
    (enkan-repl--save-workspace-state) ; This should save current (02)
    
    (message "After save - WS 02 saved state:")
    (message "  %S" (cdr (assoc "02" enkan-repl--workspaces)))
    
    ;; Check WS 02 was saved
    (let ((ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (should ws02-state)
      (should (equal '((1 . "project-b") (2 . "project-b")) 
                     (plist-get ws02-state :session-list)))
      (should (equal 2 (plist-get ws02-state :session-counter))))))

(ert-deftest test-setup-create-workspace-saves-state ()
  "Test that setup-create-workspace properly saves initial state."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory "/path/to/er/"))
    
    ;; Create workspace with setup function
    (let ((ws-id (enkan-repl--setup-create-workspace-with-project t nil)))
      (message "Created workspace: %s" ws-id)
      (message "Current project: %s" enkan-repl--current-project)
      
      ;; Check that the workspace was created and saved
      (let ((ws-state (cdr (assoc ws-id enkan-repl--workspaces))))
        (message "Workspace state: %S" ws-state)
        (should ws-state)
        (should (equal "er" (plist-get ws-state :current-project)))
        ;; Note: No sessions yet since eat hasn't been started
        (should (equal 0 (plist-get ws-state :session-counter)))))))

(ert-deftest test-actual-usage-pattern ()
  "Test the actual usage pattern that causes the issue."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory "/path/to/er/"))
    
    ;; Step 1: First enkan-repl-setup creates WS 01
    (let ((ws01 (enkan-repl--setup-create-workspace-with-project t nil)))
      (message "\n=== After creating WS 01 ===")
      (message "Workspace ID: %s" ws01)
      (message "Current project: %s" enkan-repl--current-project)
      (message "Sessions (before eat): %S" enkan-repl-session-list)
      
      ;; Simulate eat session start
      (setq enkan-repl--session-counter 1)
      (enkan-repl--register-session 1 "er")
      (enkan-repl--save-workspace-state)
      
      (message "Sessions (after eat): %S" enkan-repl-session-list))
    
    ;; Step 2: Second enkan-repl-setup creates WS 02
    (let ((ws02 (enkan-repl--setup-create-workspace-with-project t nil)))
      (message "\n=== After creating WS 02 ===")
      (message "Workspace ID: %s" ws02)
      (message "Current project: %s" enkan-repl--current-project)
      (message "Sessions (before eat): %S" enkan-repl-session-list)
      
      ;; Simulate eat session start
      (setq enkan-repl--session-counter 1)
      (enkan-repl--register-session 1 "er")
      (enkan-repl--save-workspace-state)
      
      (message "Sessions (after eat): %S" enkan-repl-session-list))
    
    ;; Step 3: Check what enkan-repl-workspace-list would show
    (message "\n=== Workspace states for list ===")
    (dolist (ws-id '("01" "02"))
      (let ((state (cdr (assoc ws-id enkan-repl--workspaces))))
        (message "WS %s: sessions=%d, project=%s" 
                 ws-id
                 (length (plist-get state :session-list))
                 (plist-get state :current-project))
        ;; Both should have 1 session
        (should (= 1 (length (plist-get state :session-list))))))
    
    ;; Step 4: Switch to WS 01
    (message "\n=== Switching to WS 01 ===")
    (enkan-repl--save-workspace-state) ; Save current (02)
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    (message "After switch:")
    (message "  Current WS: %s" enkan-repl--current-workspace)
    (message "  Sessions: %S" enkan-repl-session-list)
    
    ;; Both workspaces should still have their sessions
    (dolist (ws-id '("01" "02"))
      (let ((state (cdr (assoc ws-id enkan-repl--workspaces))))
        (should (= 1 (length (plist-get state :session-list))))))))

(provide 'enkan-repl-workspace-actual-save-test)
;;; enkan-repl-workspace-actual-save-test.el ends here
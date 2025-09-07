;;; enkan-repl-workspace-complete-scenario-test.el --- Complete scenario test -*- lexical-binding: t -*-

;;; Commentary:
;; Test complete scenario: setup creates workspace, switch preserves sessions

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-complete-workspace-scenario ()
  "Test complete scenario from setup to switch."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-target-directories nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-center-file "/path/to/center.md")
        (default-directory "/path/to/er/"))
    
    (message "\n=== Step 1: First enkan-repl-setup creates WS 01 ===")
    
    ;; Simulate first enkan-repl-setup creating workspace
    (let ((new-id (enkan-repl--setup-create-workspace-with-project t nil)))
      (should (string= "01" new-id))
      (should (string= "01" enkan-repl--current-workspace)))
    
    ;; Simulate starting eat session
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (enkan-repl--save-workspace-state "01")
    
    (message "WS 01 state after setup:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Project: %s" enkan-repl--current-project)
    
    (message "\n=== Step 2: Second enkan-repl-setup creates WS 02 ===")
    
    ;; Simulate second enkan-repl-setup creating another workspace
    (let ((new-id (enkan-repl--setup-create-workspace-with-project t nil)))
      (should (string= "02" new-id))
      (should (string= "02" enkan-repl--current-workspace)))
    
    ;; Simulate starting eat session in WS 02
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (enkan-repl--save-workspace-state "02")
    
    (message "WS 02 state after setup:")
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Project: %s" enkan-repl--current-project)
    
    (message "\n=== Step 3: Switch from WS 02 to WS 01 ===")
    
    ;; Simulate enkan-repl-workspace-switch
    (enkan-repl--save-workspace-state) ; Save current (WS 02)
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    (message "After switching to WS 01:")
    (message "  Current workspace: %s" enkan-repl--current-workspace)
    (message "  Sessions: %S" enkan-repl-session-list)
    (message "  Project: %s" enkan-repl--current-project)
    
    ;; Verify WS 01 state is restored
    (should (string= "01" enkan-repl--current-workspace))
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (string= "er" enkan-repl--current-project))
    
    (message "\n=== Step 4: enkan-repl-workspace-list should show both ===")
    
    ;; Check workspace list shows correct session counts
    (dolist (ws-id '("01" "02"))
      (let ((state (enkan-repl--get-workspace-state enkan-repl--workspaces ws-id)))
        (message "WS %s: Sessions=%d, Project=%s"
                 ws-id
                 (length (plist-get state :session-list))
                 (plist-get state :current-project))
        (should (= 1 (length (plist-get state :session-list))))))))

(ert-deftest test-workspace-empty-session-bug ()
  "Test that workspace doesn't lose sessions on switch."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil))
    
    ;; Create WS 01 with sessions
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er") (2 . "er")))
    (setq enkan-repl--session-counter 2)
    (setq enkan-repl--current-project "er")
    (enkan-repl--save-workspace-state "01")
    
    ;; Create WS 02 with session
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (enkan-repl--save-workspace-state "02")
    
    ;; Clear current session list (simulate potential bug)
    (setq enkan-repl-session-list nil)
    
    ;; Switch to WS 01
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Sessions should be restored even if they were nil before
    (message "Sessions after switch: %S" enkan-repl-session-list)
    (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))
    
    ;; Switch to WS 02
    (enkan-repl--save-workspace-state) ; Save WS 01
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; WS 02 should have its session
    (should (equal '((1 . "er")) enkan-repl-session-list))
    
    ;; Switch back to WS 01
    (enkan-repl--save-workspace-state) ; Save WS 02
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; WS 01 should still have its sessions
    (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))))

(provide 'enkan-repl-workspace-complete-scenario-test)
;;; enkan-repl-workspace-complete-scenario-test.el ends here
;;; enkan-repl-workspace-zero-sessions-bug-test.el --- Test for zero sessions bug -*- lexical-binding: t -*-

;;; Commentary:
;; Test that reproduces the bug where workspace shows 0 sessions after switch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-shows-zero-sessions-after-switch ()
  "Test the exact bug: ws:02 shows 0 sessions after switch."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory "/path/to/er/"))
    
    (message "\n=== Step 1: Create WS 01 with er project ===")
    ;; Simulate first enkan-repl-setup
    (let ((ws01 (enkan-repl--setup-create-workspace-with-project t nil)))
      (message "Created WS: %s" ws01)
      (message "Current project: %s" enkan-repl--current-project)
      
      ;; Simulate eat session start (as would happen with C-M-s)
      (setq enkan-repl--session-counter 1)
      (enkan-repl--register-session 1 "er")
      (enkan-repl--save-workspace-state)
      
      (message "WS 01 sessions after eat: %S" enkan-repl-session-list))
    
    (message "\n=== Step 2: Create WS 02 with er project ===")
    ;; Simulate second enkan-repl-setup
    (let ((ws02 (enkan-repl--setup-create-workspace-with-project t nil)))
      (message "Created WS: %s" ws02)
      (message "Current project: %s" enkan-repl--current-project)
      
      ;; Simulate eat session start
      (setq enkan-repl--session-counter 1)
      (enkan-repl--register-session 1 "er")
      (enkan-repl--save-workspace-state)
      
      (message "WS 02 sessions after eat: %S" enkan-repl-session-list))
    
    (message "\n=== Step 3: Switch to WS 02 (simulating user action) ===")
    ;; This simulates what happens when user switches to ws:02
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    (message "After switch to WS 02:")
    (message "  Current workspace: %s" enkan-repl--current-workspace)
    (message "  Session list: %S" enkan-repl-session-list)
    (message "  Session counter: %d" enkan-repl--session-counter)
    
    ;; This is what enkan-repl-workspace-list would show
    (let ((ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (message "WS 02 saved state: %S" ws02-state)
      (let ((session-count (length (plist-get ws02-state :session-list))))
        (message "Session count that workspace-list would show: %d" session-count)
        ;; This should NOT be 0!
        (should (> session-count 0))
        (should (= 1 session-count))))))

(ert-deftest test-workspace-state-after-multiple-switches ()
  "Test workspace state preservation after multiple switches."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (default-directory "/path/to/er/"))
    
    ;; Create WS 01
    (enkan-repl--setup-create-workspace-with-project t nil)
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    (enkan-repl--save-workspace-state)
    
    ;; Create WS 02
    (enkan-repl--setup-create-workspace-with-project t nil)
    (setq enkan-repl--session-counter 1)
    (enkan-repl--register-session 1 "er")
    (enkan-repl--save-workspace-state)
    
    ;; Switch to WS 01
    (enkan-repl--save-workspace-state) ; Save current (02)
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Switch back to WS 02
    (enkan-repl--save-workspace-state) ; Save current (01)
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; Check both workspaces have their sessions
    (let ((ws01-state (cdr (assoc "01" enkan-repl--workspaces)))
          (ws02-state (cdr (assoc "02" enkan-repl--workspaces))))
      (message "Final WS 01: %S" ws01-state)
      (message "Final WS 02: %S" ws02-state)
      (should (= 1 (length (plist-get ws01-state :session-list))))
      (should (= 1 (length (plist-get ws02-state :session-list)))))))

(provide 'enkan-repl-workspace-zero-sessions-bug-test)
;;; enkan-repl-workspace-zero-sessions-bug-test.el ends here
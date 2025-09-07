;;; enkan-repl-workspace-layout-problem-test.el --- Test for workspace layout problem -*- lexical-binding: t -*-

;;; Commentary:
;; Test the actual problem where enkan-repl-setup-current-project-layout 
;; opens center file instead of eat buffers after workspace switch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-workspace-layout-session-preservation ()
  "Test that sessions are available for layout after workspace switch."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-target-directories nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-center-file "/path/to/center.md"))
    
    ;; Create workspace 01 with er project
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (setq enkan-repl-target-directories '(("er-alias" . ("er" . "/path/to/er"))))
    
    ;; Save workspace 01 state
    (enkan-repl--save-workspace-state "01")
    
    ;; Verify saved state
    (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
      (message "WS 01 saved state: %S" saved-state)
      (should (equal '((1 . "er")) (plist-get saved-state :session-list))))
    
    ;; Create workspace 02 with er project
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (setq enkan-repl-target-directories '(("er-alias" . ("er" . "/path/to/er"))))
    
    ;; Save workspace 02 state
    (enkan-repl--save-workspace-state "02")
    
    ;; Switch back to workspace 01 (simulating user action)
    (enkan-repl--save-workspace-state) ; Save current (02) state
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Debug: Check state after loading
    (message "After loading WS 01:")
    (message "  Current workspace: %s" enkan-repl--current-workspace)
    (message "  Session list: %S" enkan-repl-session-list)
    (message "  Current project: %s" enkan-repl--current-project)
    (message "  Target dirs: %S" enkan-repl-target-directories)
    
    ;; Verify session list is restored
    (should (equal "01" enkan-repl--current-workspace))
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (equal "er" enkan-repl--current-project))
    (should enkan-repl-target-directories)
    
    ;; Test enkan-repl--ws-session-list function
    (should (equal '((1 . "er")) (enkan-repl--ws-session-list)))
    
    ;; Test setup-window-eat-buffer-pure
    (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1
                     (enkan-repl--ws-session-list)
                     enkan-repl-target-directories)))
        (message "setup-window-eat-buffer-pure result: %S" result)
        (should result)
        (should (consp result))
        (should (string-match-p "\\*ws:01 enkan:/path/to/er/\\*" (cdr result)))))))

(ert-deftest test-workspace-layout-problem-reproduction ()
  "Reproduce the exact problem reported by user."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-target-directories nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-center-file "/path/to/center.md"))
    
    ;; Setup workspace 01 with er project
    (message "\n=== Setting up WS 01 ===")
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (setq enkan-repl-target-directories '(("er-alias" . ("er" . "/path/to/er"))))
    (enkan-repl--save-workspace-state "01")
    (message "WS 01 saved with sessions: %S" enkan-repl-session-list)
    
    ;; Setup workspace 02 with er project
    (message "\n=== Setting up WS 02 ===")
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (setq enkan-repl-target-directories '(("er-alias" . ("er" . "/path/to/er"))))
    (enkan-repl--save-workspace-state "02")
    (message "WS 02 saved with sessions: %S" enkan-repl-session-list)
    
    ;; Switch to WS 01
    (message "\n=== Switching to WS 01 ===")
    (enkan-repl--save-workspace-state) ; Save current WS 02
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    (message "After switch to WS 01:")
    (message "  Current workspace: %s" enkan-repl--current-workspace)
    (message "  Session list: %S" enkan-repl-session-list)
    (message "  enkan-repl--ws-session-list: %S" (enkan-repl--ws-session-list))
    
    ;; The problem: session list should be restored but might be nil
    (should enkan-repl-session-list)
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (equal '((1 . "er")) (enkan-repl--ws-session-list)))
    
    ;; This is what would fail if sessions aren't restored
    (when (fboundp 'enkan-repl--setup-session-eat-buffer)
      ;; Mock window for testing
      (let ((test-window 'mock-window))
        ;; This should find the eat buffer, not open center file
        (let ((result (enkan-repl--setup-window-eat-buffer-pure
                       test-window 1
                       (enkan-repl--ws-session-list)
                       enkan-repl-target-directories)))
          (if result
              (message "✅ Found eat buffer setup: %S" result)
            (message "❌ No eat buffer setup found - would open center file!")))))))

(provide 'enkan-repl-workspace-layout-problem-test)
;;; enkan-repl-workspace-layout-problem-test.el ends here
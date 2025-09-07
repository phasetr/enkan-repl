;;; enkan-repl-workspace-session-preservation-test.el --- Test for session list preservation -*- lexical-binding: t -*-

;;; Commentary:
;; Test that session lists are properly preserved when switching between workspaces

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-session-list-preservation-on-switch ()
  "Test that session lists are preserved when switching between workspaces."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil))
    
    ;; Create workspace 01 with er project and 2 sessions
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er") (2 . "er")))
    (setq enkan-repl--session-counter 2)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    
    ;; Save workspace 01 state
    (enkan-repl--save-workspace-state "01")
    
    ;; Verify saved state
    (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
      (should (equal '((1 . "er") (2 . "er")) (plist-get saved-state :session-list)))
      (should (equal 2 (plist-get saved-state :session-counter))))
    
    ;; Create workspace 02 with er project and 1 session
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    
    ;; Save workspace 02 state
    (enkan-repl--save-workspace-state "02")
    
    ;; Verify saved state
    (let ((saved-state (cdr (assoc "02" enkan-repl--workspaces))))
      (should (equal '((1 . "er")) (plist-get saved-state :session-list)))
      (should (equal 1 (plist-get saved-state :session-counter))))
    
    ;; Switch back to workspace 01
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Verify restored state for workspace 01
    (should (equal "01" enkan-repl--current-workspace))
    (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))
    (should (equal 2 enkan-repl--session-counter))
    (should (equal "er" enkan-repl--current-project))
    
    ;; Switch to workspace 02
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; Verify restored state for workspace 02
    (should (equal "02" enkan-repl--current-workspace))
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (equal 1 enkan-repl--session-counter))
    (should (equal "er" enkan-repl--current-project))
    
    ;; Switch back to workspace 01 again
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Verify restored state is still correct
    (should (equal "01" enkan-repl--current-workspace))
    (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))
    (should (equal 2 enkan-repl--session-counter))))

(ert-deftest test-workspace-switch-command-preserves-sessions ()
  "Test that enkan-repl-switch-workspace command preserves session lists."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil))
    
    ;; Create workspace 01 with sessions
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er") (2 . "er")))
    (setq enkan-repl--session-counter 2)
    (setq enkan-repl--current-project "er")
    (enkan-repl--save-workspace-state "01")
    
    ;; Create workspace 02 with different sessions
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "project-b")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "project-b")
    (enkan-repl--save-workspace-state "02")
    
    ;; Simulate switching with enkan-repl-switch-workspace logic
    ;; Switch from 02 to 01
    (enkan-repl--save-workspace-state) ; Save current (02) state
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Verify workspace 01 sessions are restored
    (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))
    (should (equal 2 enkan-repl--session-counter))
    
    ;; Switch back to 02
    (enkan-repl--save-workspace-state) ; Save current (01) state
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; Verify workspace 02 sessions are restored
    (should (equal '((1 . "project-b")) enkan-repl-session-list))
    (should (equal 1 enkan-repl--session-counter))))

(provide 'enkan-repl-workspace-session-preservation-test)
;;; enkan-repl-workspace-session-preservation-test.el ends here
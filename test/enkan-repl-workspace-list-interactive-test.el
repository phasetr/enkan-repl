;;; enkan-repl-workspace-list-interactive-test.el --- Interactive test for workspace list -*- lexical-binding: t -*-

;;; Commentary:

;; Interactive test to verify delete functionality works

;;; Code:

(require 'enkan-repl)
(require 'enkan-repl-workspace)
(require 'enkan-repl-workspace-list)

;; Setup test data
(setq enkan-repl--workspaces '(("01" . (:current-project nil
                                         :project-aliases nil
                                         :session-list nil
                                         :session-counter 0))
                               ("02" . (:current-project nil
                                         :project-aliases nil
                                         :session-list nil
                                         :session-counter 0))
                               ("03" . (:current-project nil
                                         :project-aliases nil
                                         :session-list nil
                                         :session-counter 0))))

(setq enkan-repl--current-workspace "01")
(setq enkan-repl-target-directories '(("test" . "/test/path")))
(setq enkan-repl-center-file "test.md")
(setq enkan-repl-projects '(("test-project" . "Project Description")))

;; Display workspace list
(enkan-repl-workspace-list)

;; Show the current state
(message "Initial workspaces: %s" (mapcar #'car enkan-repl--workspaces))
(message "Current workspace: %s" enkan-repl--current-workspace)
(message "Buffer mode: %s" major-mode)
(message "Key 'd' is bound to: %s" (lookup-key enkan-repl-workspace-list-mode-map "d"))

;; Instructions
(message "")
(message "=== INTERACTIVE TEST INSTRUCTIONS ===")
(message "1. Move cursor to a workspace line")
(message "2. Press 'd' to delete the workspace")
(message "3. Check if workspace is deleted")
(message "")
(message "Current keybindings in workspace list buffer:")
(message "  d - Delete workspace")
(message "  RET - Switch to workspace")
(message "  g - Refresh list")
(message "  q - Quit")

(provide 'enkan-repl-workspace-list-interactive-test)
;;; enkan-repl-workspace-list-interactive-test.el ends here
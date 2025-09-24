;;; manual-test-instructions.el --- Manual test instructions -*- lexical-binding: t -*-

;;; Commentary:

;; Manual testing instructions to verify the functionality

;;; Code:

;; === MANUAL TEST INSTRUCTIONS ===
;;
;; 1. Start fresh Emacs session
;; 2. Evaluate the following:

;; Step 1: Load the required files
(add-to-list 'load-path "/Users/sekine/dev/self/enkan-repl")
(require 'enkan-repl-workspace)
(require 'enkan-repl)
(require 'enkan-repl-workspace-list)

;; Step 2: Setup test data
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

;; Step 3: Open workspace list
(enkan-repl-workspace-list)

;; Step 4: Test the following:
;; - Press 'd' on any workspace line - should prompt for deletion
;; - Press 'g' - should refresh the list
;; - Press 'q' - should quit the buffer

;; Step 5: Verify key bindings
(with-current-buffer "*Enkan Workspace List*"
  (message "Current major mode: %s" major-mode)
  (message "Key 'd' is bound to: %s" (lookup-key enkan-repl-workspace-list-mode-map "d"))
  (message "Key 'g' is bound to: %s" (lookup-key enkan-repl-workspace-list-mode-map "g"))
  (message "Key 'q' is bound to: %s" (lookup-key enkan-repl-workspace-list-mode-map "q")))

;; Step 6: Test enkan-repl-teardown
;; Create a single workspace scenario
(setq enkan-repl--workspaces '(("01" . (:current-project nil
                                         :project-aliases nil
                                         :session-list nil
                                         :session-counter 0))))
(setq enkan-repl--current-workspace "01")

;; Now test teardown - should delete the workspace and create a new one
;; (enkan-repl--teardown-delete-current-workspace-pure)
;; After this, check enkan-repl--workspaces and enkan-repl--current-workspace

(provide 'manual-test-instructions)
;;; manual-test-instructions.el ends here
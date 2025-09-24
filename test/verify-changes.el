;;; verify-changes.el --- Verify that our changes are working -*- lexical-binding: t -*-

;;; Commentary:

;; This file verifies the changes made to enkan-repl

;;; Code:

;; Load the modules explicitly
(add-to-list 'load-path "/Users/sekine/dev/self/enkan-repl")
(require 'enkan-repl-workspace)
(require 'enkan-repl)
(require 'enkan-repl-workspace-list)

;; Test 1: Check if delete function exists
(if (fboundp 'enkan-repl-workspace-list-delete-workspace)
    (message "✓ enkan-repl-workspace-list-delete-workspace function exists")
  (error "✗ enkan-repl-workspace-list-delete-workspace function NOT found"))

;; Test 2: Check if 'd' key is bound in the mode map
(let ((binding (lookup-key enkan-repl-workspace-list-mode-map "d")))
  (if (eq binding 'enkan-repl-workspace-list-delete-workspace)
      (message "✓ 'd' key is correctly bound to delete function")
    (if binding
        (error "✗ 'd' key is bound to wrong function: %s" binding)
      (error "✗ 'd' key is not bound"))))

;; Test 3: Check if teardown changes are present
(with-temp-buffer
  (insert-file-contents "/Users/sekine/dev/self/enkan-repl/enkan-repl.el")
  (goto-char (point-min))
  (if (search-forward "If no workspaces remain, create a new default workspace" nil t)
      (message "✓ enkan-repl-teardown changes are present")
    (error "✗ enkan-repl-teardown changes NOT found")))

;; Test 4: Create test workspaces and verify functionality
(let ((enkan-repl--workspaces '(("01" . (:current-project nil
                                          :project-aliases nil
                                          :session-list nil
                                          :session-counter 0))
                                ("02" . (:current-project nil
                                          :project-aliases nil
                                          :session-list nil
                                          :session-counter 0))))
      (enkan-repl--current-workspace "01")
      (enkan-repl-target-directories '(("test" . "/test/path")))
      (enkan-repl-center-file "test.md")
      (enkan-repl-projects '(("test-project" . "Project Description"))))

  ;; Test deletion of a workspace
  (let ((result (enkan-repl--delete-workspace enkan-repl--workspaces "02")))
    (if (and (= 1 (length result))
             (assoc "01" result)
             (not (assoc "02" result)))
        (message "✓ Workspace deletion logic works")
      (error "✗ Workspace deletion logic failed"))))

(message "\n=== All tests passed successfully ===")

(provide 'verify-changes)
;;; verify-changes.el ends here
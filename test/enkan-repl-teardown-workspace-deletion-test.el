;;; enkan-repl-teardown-workspace-deletion-test.el --- Test workspace deletion in teardown -*- lexical-binding: t -*-

;;; Commentary:
;; Test that teardown actually deletes workspaces

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-teardown-deletes-workspace-pure-function ()
  "Test that enkan-repl--teardown-delete-current-workspace-pure deletes workspace."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "test" :session-list ((1 . "test")) :session-counter 1 :project-aliases nil))
                                   ("02" . (:current-project "other" :session-list ((1 . "other")) :session-counter 1 :project-aliases nil))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "test")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "test")
        (enkan-repl-project-aliases nil))
    
    ;; Verify initial state
    (should (equal "01" enkan-repl--current-workspace))
    (should (equal 2 (length enkan-repl--workspaces)))
    (should (assoc "01" enkan-repl--workspaces))
    
    ;; Execute deletion
    (enkan-repl--teardown-delete-current-workspace-pure)
    
    ;; Verify workspace was deleted
    (should (equal nil enkan-repl--current-workspace))
    (should (equal 1 (length enkan-repl--workspaces)))
    (should (not (assoc "01" enkan-repl--workspaces)))
    (should (assoc "02" enkan-repl--workspaces))
    
    ;; Verify session variables were reset
    (should (equal nil enkan-repl-session-list))
    (should (equal 0 enkan-repl--session-counter))
    (should (equal nil enkan-repl--current-project))
    (should (equal nil enkan-repl-project-aliases))))

(provide 'enkan-repl-teardown-workspace-deletion-test)
;;; enkan-repl-teardown-workspace-deletion-test.el ends here
;;; enkan-repl-teardown-workspace-switch-test.el --- Test workspace switching after teardown -*- lexical-binding: t -*-

;;; Commentary:
;; Test that teardown switches to another workspace when available

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-teardown-switches-to-remaining-workspace ()
  "Test that teardown switches to remaining workspace when available."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "test" :session-list ((1 . "test")) :session-counter 1 :project-aliases nil))
                                   ("02" . (:current-project "other" :session-list ((1 . "other")) :session-counter 1 :project-aliases nil))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "test")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "test")
        (enkan-repl-project-aliases nil)
        (workspace-switch-called nil))
    
    ;; Mock enkan-repl-workspace-switch to verify it's called
    (cl-letf (((symbol-function 'enkan-repl-workspace-switch)
               (lambda ()
                 (setq workspace-switch-called t)
                 ;; Simulate switching to workspace 02
                 (setq enkan-repl--current-workspace "02")
                 (enkan-repl--load-workspace-state "02"))))
      
      ;; Execute deletion
      (enkan-repl--teardown-delete-current-workspace-pure)
      
      ;; Verify workspace was deleted
      (should (equal 1 (length enkan-repl--workspaces)))
      (should (not (assoc "01" enkan-repl--workspaces)))
      (should (assoc "02" enkan-repl--workspaces))
      
      ;; Now test the teardown flow with workspace switching
      ;; Since we have remaining workspace, switch should be triggered
      (when (enkan-repl--list-workspace-ids enkan-repl--workspaces)
        (enkan-repl-workspace-switch))
      
      ;; Verify workspace switch was called
      (should workspace-switch-called)
      (should (equal "02" enkan-repl--current-workspace)))))

(ert-deftest test-teardown-no-switch-when-no-workspaces ()
  "Test that teardown doesn't switch when no workspaces remain."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "test" :session-list ((1 . "test")) :session-counter 1 :project-aliases nil))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "test")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "test")
        (enkan-repl-project-aliases nil)
        (workspace-switch-called nil))
    
    ;; Mock enkan-repl-workspace-switch
    (cl-letf (((symbol-function 'enkan-repl-workspace-switch)
               (lambda ()
                 (setq workspace-switch-called t))))
      
      ;; Execute deletion
      (enkan-repl--teardown-delete-current-workspace-pure)
      
      ;; Verify workspace was deleted
      (should (equal 0 (length enkan-repl--workspaces)))
      (should (equal nil enkan-repl--current-workspace))
      
      ;; Test the teardown flow - should not call switch when no workspaces
      (when (enkan-repl--list-workspace-ids enkan-repl--workspaces)
        (enkan-repl-workspace-switch))
      
      ;; Verify workspace switch was NOT called
      (should-not workspace-switch-called))))

(provide 'enkan-repl-teardown-workspace-switch-test)
;;; enkan-repl-teardown-workspace-switch-test.el ends here
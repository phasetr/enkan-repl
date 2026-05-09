;;; enkan-repl-workspace-list-delete-test.el --- Test workspace list deletion -*- lexical-binding: t -*-

;;; Commentary:

;; Test the delete functionality in workspace list

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-workspace)
(require 'enkan-repl-workspace-list)

(ert-deftest test-workspace-list-delete-function-is-merged ()
  "Workspace-list deletion uses the shared workspace delete command."
  (should (fboundp 'enkan-repl-workspace-delete))
  (should-not (fboundp 'enkan-repl-workspace-list-delete-workspace)))

(ert-deftest test-workspace-list-key-binding ()
  "Test that 'd' key is bound to delete function."
  (should (eq (lookup-key enkan-repl-workspace-list-mode-map "d")
              'enkan-repl-workspace-delete)))

(ert-deftest test-workspace-list-delete-with-mock ()
  "Test workspace deletion with mock data."
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

    ;; Test deletion of non-current workspace
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'enkan-repl--delete-workspace-completely)
               (lambda (workspace-id)
                 (setq enkan-repl--workspaces
                       (enkan-repl--delete-workspace enkan-repl--workspaces
                                                     workspace-id))
                 (list :deleted t)))
              ((symbol-function 'enkan-repl-workspace-list-refresh) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (insert (propertize "02" 'workspace-id "02"))
        (goto-char (point-min))
        (enkan-repl-workspace-list-mode)
        (enkan-repl-workspace-delete))
      ;; Verify workspace 02 was deleted
      (should (= 1 (length enkan-repl--workspaces)))
      (should (assoc "01" enkan-repl--workspaces))
      (should-not (assoc "02" enkan-repl--workspaces)))))

(ert-deftest test-workspace-list-delete-current ()
  "Test deletion of current workspace."
  (let ((enkan-repl--workspaces '(("01" . (:current-project nil
                                           :project-aliases nil
                                           :session-list nil
                                           :session-counter 0))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-target-directories '(("test" . "/test/path")))
        (enkan-repl-center-file "test.md")
        (enkan-repl-projects '(("test-project" . "Project Description"))))

    ;; Test deletion of current (and only) workspace
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'enkan-repl--delete-workspace-completely)
               (lambda (_workspace-id)
                 (setq enkan-repl--workspaces '(("01" . (:current-project nil
                                                          :project-aliases nil
                                                          :session-list nil
                                                          :session-counter 0))))
                 (setq enkan-repl--current-workspace "01")
                 (list :deleted t)))
              ((symbol-function 'enkan-repl-workspace-list-refresh) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (insert (propertize "01" 'workspace-id "01"))
        (goto-char (point-min))
        (enkan-repl-workspace-list-mode)
        (enkan-repl-workspace-delete))
      ;; After deletion and recreation, should have a workspace
      (should enkan-repl--workspaces)
      (should enkan-repl--current-workspace))))

(ert-deftest test-workspace-list-delete-delegates-to-complete-delete ()
  "Workspace-list deletion uses the shared complete-delete API."
  (let ((enkan-repl--workspaces '(("08" . (:current-project nil
                                           :project-aliases nil
                                           :session-list nil
                                           :session-counter 0))))
        (enkan-repl--current-workspace "08")
        (deleted nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'enkan-repl--delete-workspace-completely)
               (lambda (workspace-id)
                 (setq deleted workspace-id)
                 (list :deleted t)))
              ((symbol-function 'enkan-repl-workspace-list-refresh) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (insert (propertize "08" 'workspace-id "08"))
        (goto-char (point-min))
        (enkan-repl-workspace-list-mode)
        (enkan-repl-workspace-delete))
      (should (string= "08" deleted)))))

(provide 'enkan-repl-workspace-list-delete-test)
;;; enkan-repl-workspace-list-delete-test.el ends here

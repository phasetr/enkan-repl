;;; enkan-repl-workspace-list-delete-test.el --- Test workspace list deletion -*- lexical-binding: t -*-

;;; Commentary:

;; Test the delete functionality in workspace list

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-workspace)
(require 'enkan-repl-workspace-list)

(ert-deftest test-workspace-list-delete-function-exists ()
  "Test that the delete function exists."
  (should (fboundp 'enkan-repl-workspace-list-delete-workspace)))

(ert-deftest test-workspace-list-key-binding ()
  "Test that 'd' key is bound to delete function."
  (should (eq (lookup-key enkan-repl-workspace-list-mode-map "d")
              'enkan-repl-workspace-list-delete-workspace)))

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
              ((symbol-function 'enkan-repl-workspace-list-refresh) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Mock the point to be on workspace 02
      (cl-letf (((symbol-function 'enkan-repl-workspace-list--get-workspace-at-point)
                 (lambda () "02")))
        (enkan-repl-workspace-list-delete-workspace))
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
              ((symbol-function 'enkan-repl--save-workspace-state) (lambda () nil))
              ((symbol-function 'enkan-repl--load-workspace-state) (lambda (_) nil))
              ((symbol-function 'enkan-repl-workspace-list-refresh) (lambda () nil))
              ((symbol-function 'enkan-repl-setup)
               (lambda ()
                 (setq enkan-repl--workspaces '(("01" . (:current-project nil
                                                          :project-aliases nil
                                                          :session-list nil
                                                          :session-counter 0))))
                 (setq enkan-repl--current-workspace "01")))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; Mock the point to be on workspace 01
      (cl-letf (((symbol-function 'enkan-repl-workspace-list--get-workspace-at-point)
                 (lambda () "01")))
        (enkan-repl-workspace-list-delete-workspace))
      ;; After deletion and recreation, should have a workspace
      (should enkan-repl--workspaces)
      (should enkan-repl--current-workspace))))

(provide 'enkan-repl-workspace-list-delete-test)
;;; enkan-repl-workspace-list-delete-test.el ends here
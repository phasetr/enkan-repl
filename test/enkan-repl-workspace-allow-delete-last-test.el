;;; enkan-repl-workspace-allow-delete-last-test.el --- Test allowing deletion of last workspace -*- lexical-binding: t -*-

;;; Commentary:

;; Test that last workspace can be deleted and a new one is created

;;; Code:

(require 'ert)
(require 'enkan-repl-workspace)

(ert-deftest test-last-workspace-deletion ()
  "Test that last workspace can be deleted."
  ;; Create initial workspace
  (let ((workspaces '(("01" . (:current-project nil
                                :project-aliases nil
                                :session-list nil
                                :session-counter 0)))))
    ;; Delete the only workspace
    (let ((result (enkan-repl--delete-workspace workspaces "01")))
      ;; Should return nil or empty list when last workspace is deleted
      (should (null result)))))

(provide 'enkan-repl-workspace-allow-delete-last-test)
;;; enkan-repl-workspace-allow-delete-last-test.el ends here
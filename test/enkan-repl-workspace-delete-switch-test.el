;;; enkan-repl-workspace-delete-switch-test.el --- Test workspace switching after deletion -*- lexical-binding: t -*-

;;; Commentary:
;; Test that workspace deletion switches to another workspace when available.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-delete-loads-remaining-workspace ()
  "Test that complete deletion loads a remaining workspace when available."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "test" :session-list ((1 . "test")) :session-counter 1 :project-aliases nil))
                                   ("02" . (:current-project "other" :session-list ((1 . "other")) :session-counter 1 :project-aliases nil))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "test")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "test")
        (enkan-repl-project-aliases nil))

    (cl-letf (((symbol-function 'enkan-repl--stop-workspace-terminals)
               (lambda (_workspace-id)
                 (list :buffers-killed 0 :tmux-killed nil))))
      (enkan-repl--delete-workspace-completely "01")

      ;; Verify workspace was deleted
      (should (equal 1 (length enkan-repl--workspaces)))
      (should (not (assoc "01" enkan-repl--workspaces)))
      (should (assoc "02" enkan-repl--workspaces))

      ;; Verify remaining workspace was loaded directly
      (should (equal "02" enkan-repl--current-workspace)))))

(ert-deftest test-workspace-delete-creates-workspace-when-none-remain ()
  "Test that complete deletion creates a workspace when none remain."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "test" :session-list ((1 . "test")) :session-counter 1 :project-aliases nil))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "test")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "test")
        (enkan-repl-project-aliases nil))

    (cl-letf (((symbol-function 'enkan-repl--stop-workspace-terminals)
               (lambda (_workspace-id)
                 (list :buffers-killed 0 :tmux-killed nil)))
              ((symbol-function 'enkan-repl-setup)
               (lambda ()
                 (setq enkan-repl--workspaces
                       '(("01" . (:current-project nil
                                  :session-list nil
                                  :session-counter 0
                                  :project-aliases nil))))
                 (setq enkan-repl--current-workspace "01"))))
      (enkan-repl--delete-workspace-completely "01")

      (should (equal 1 (length enkan-repl--workspaces)))
      (should (equal "01" enkan-repl--current-workspace)))))

(provide 'enkan-repl-workspace-delete-switch-test)
;;; enkan-repl-workspace-delete-switch-test.el ends here

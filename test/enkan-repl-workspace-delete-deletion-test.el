;;; enkan-repl-workspace-delete-deletion-test.el --- Test workspace deletion -*- lexical-binding: t -*-

;;; Commentary:
;; Test that complete workspace deletion removes workspace state.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-delete-deletes-workspace-completely ()
  "Test that complete workspace deletion removes the current workspace."
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
    
    (cl-letf (((symbol-function 'enkan-repl--stop-workspace-terminals)
               (lambda (_workspace-id)
                 (list :buffers-killed 0 :tmux-killed nil))))
      ;; Execute deletion
      (enkan-repl--delete-workspace-completely "01")

      ;; Verify workspace was deleted and the remaining workspace was loaded
      (should (equal "02" enkan-repl--current-workspace))
      (should (equal 1 (length enkan-repl--workspaces)))
      (should (not (assoc "01" enkan-repl--workspaces)))
      (should (assoc "02" enkan-repl--workspaces))

      ;; Verify session variables now reflect the remaining workspace
      (should (equal '((1 . "other")) enkan-repl-session-list))
      (should (equal 1 enkan-repl--session-counter))
      (should (equal "other" enkan-repl--current-project)))))

(ert-deftest test-workspace-delete-preserves-target-registry-for-new-setup ()
  "Deleting a workspace must not make unrelated project aliases unusable."
  (let ((enkan-repl--workspaces
         '(("01" . (:current-project "junk"
                    :session-list ((1 . "junk"))
                    :session-counter 1
                    :project-aliases (("junk" . "junk"))
                    :target-directories
                    (("junk" . ("junk" . "/repo/junk")))))
           ("02" . (:current-project "er"
                    :session-list ((1 . "enkan-repl"))
                    :session-counter 1
                    :project-aliases (("er" . "enkan-repl"))
                    :target-directories
                    (("er" . ("enkan-repl" . "/repo/enkan-repl")))))))
        (enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "junk")))
        (enkan-repl--session-counter 1)
        (enkan-repl--current-project "junk")
        (enkan-repl-project-aliases '(("junk" . "junk")))
        (enkan-repl-target-directories
         '(("junk" . ("junk" . "/repo/junk"))
           ("er" . ("enkan-repl" . "/repo/enkan-repl")))))
    (cl-letf (((symbol-function 'enkan-repl--stop-workspace-terminals)
               (lambda (_workspace-id)
                 (list :buffers-killed 0 :tmux-killed nil))))
      (enkan-repl--delete-workspace-completely "01")
      (should (equal "02" enkan-repl--current-workspace))
      (should (equal '(("er" . ("enkan-repl" . "/repo/enkan-repl"))
                       ("junk" . ("junk" . "/repo/junk")))
                     enkan-repl-target-directories))
      (should (equal '("junk" . "/repo/junk")
                     (enkan-repl--setup-project-session "junk"))))))

(provide 'enkan-repl-workspace-delete-deletion-test)
;;; enkan-repl-workspace-delete-deletion-test.el ends here

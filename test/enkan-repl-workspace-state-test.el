;;; enkan-repl-workspace-state-test.el --- Tests for workspace state functions -*- lexical-binding: t -*-

;;; Commentary:
;; Test workspace state save/load functionality

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-enkan-repl--ws-state->plist ()
  "Test collecting workspace state into plist."
  (let ((enkan-repl--current-project "test-project")
        (enkan-repl-session-list '((1 . "proj1") (2 . "proj2")))
        (enkan-repl--session-counter 2)
        (enkan-repl-project-aliases '(("p1" . "proj1") ("p2" . "proj2"))))
    (let ((plist (enkan-repl--ws-state->plist)))
      (should (equal (plist-get plist :current-project) "test-project"))
      (should (equal (plist-get plist :session-list) '((1 . "proj1") (2 . "proj2"))))
      (should (equal (plist-get plist :session-counter) 2))
      (should (equal (plist-get plist :project-aliases) '(("p1" . "proj1") ("p2" . "proj2")))))))

(ert-deftest test-enkan-repl--plist->ws-state ()
  "Test restoring workspace state from plist."
  (let ((enkan-repl--current-project nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-project-aliases nil))
    (let ((state '(:current-project "restored-project"
                   :session-list ((3 . "proj3") (4 . "proj4"))
                   :session-counter 2
                   :project-aliases (("p3" . "proj3") ("p4" . "proj4")))))
      (enkan-repl--plist->ws-state state)
      (should (equal enkan-repl--current-project "restored-project"))
      (should (equal enkan-repl-session-list '((3 . "proj3") (4 . "proj4"))))
      (should (equal enkan-repl--session-counter 2))
      (should (equal enkan-repl-project-aliases '(("p3" . "proj3") ("p4" . "proj4")))))))

(ert-deftest test-enkan-repl--save-workspace-state ()
  "Test saving workspace state."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project "save-test")
        (enkan-repl-session-list '((1 . "sp1")))
        (enkan-repl--session-counter 1)
        (enkan-repl-project-aliases '(("s1" . "sp1"))))
    ;; Save current state
    (let ((saved-plist (enkan-repl--save-workspace-state)))
      ;; Check returned plist
      (should (equal (plist-get saved-plist :current-project) "save-test"))
      (should (equal (plist-get saved-plist :session-list) '((1 . "sp1"))))
      (should (equal (plist-get saved-plist :session-counter) 1))
      (should (equal (plist-get saved-plist :project-aliases) '(("s1" . "sp1"))))
      ;; Check stored in workspaces
      (let ((stored (assoc "01" enkan-repl--workspaces #'string=)))
        (should stored)
        (let ((stored-plist (cdr stored)))
          (should (equal (plist-get stored-plist :current-project) "save-test"))
          (should (equal (plist-get stored-plist :session-list) '((1 . "sp1"))))
          (should (equal (plist-get stored-plist :session-counter) 1))
          (should (equal (plist-get stored-plist :project-aliases) '(("s1" . "sp1")))))))))

(ert-deftest test-enkan-repl--save-workspace-state-with-id ()
  "Test saving workspace state with specific workspace ID."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project "ws02-test")
        (enkan-repl-session-list '((5 . "wp5")))
        (enkan-repl--session-counter 1)
        (enkan-repl-project-aliases '(("w5" . "wp5"))))
    ;; Save to workspace "02"
    (enkan-repl--save-workspace-state "02")
    ;; Check stored in workspaces under "02"
    (let ((stored (assoc "02" enkan-repl--workspaces #'string=)))
      (should stored)
      (let ((stored-plist (cdr stored)))
        (should (equal (plist-get stored-plist :current-project) "ws02-test"))
        (should (equal (plist-get stored-plist :session-list) '((5 . "wp5"))))))))

(ert-deftest test-enkan-repl--load-workspace-state ()
  "Test loading workspace state."
  (let ((enkan-repl--workspaces `((,(intern "01") . (:current-project "loaded-proj"
                                          :session-list ((7 . "lp7") (8 . "lp8"))
                                          :session-counter 2
                                          :project-aliases (("l7" . "lp7") ("l8" . "lp8"))))))
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-project-aliases nil))
    ;; Load state
    (let ((loaded-plist (enkan-repl--load-workspace-state)))
      ;; Check returned plist
      (should loaded-plist)
      (should (equal (plist-get loaded-plist :current-project) "loaded-proj"))
      ;; Check globals were updated
      (should (equal enkan-repl--current-project "loaded-proj"))
      (should (equal enkan-repl-session-list '((7 . "lp7") (8 . "lp8"))))
      (should (equal enkan-repl--session-counter 2))
      (should (equal enkan-repl-project-aliases '(("l7" . "lp7") ("l8" . "lp8")))))))

(ert-deftest test-enkan-repl--load-workspace-state-not-found ()
  "Test loading workspace state when no state exists."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project "unchanged")
        (enkan-repl-session-list '((9 . "unchanged")))
        (enkan-repl--session-counter 99)
        (enkan-repl-project-aliases '(("u" . "unchanged"))))
    ;; Try to load non-existent state
    (let ((loaded-plist (enkan-repl--load-workspace-state)))
      ;; Should return nil
      (should-not loaded-plist)
      ;; Globals should remain unchanged
      (should (equal enkan-repl--current-project "unchanged"))
      (should (equal enkan-repl-session-list '((9 . "unchanged"))))
      (should (equal enkan-repl--session-counter 99))
      (should (equal enkan-repl-project-aliases '(("u" . "unchanged")))))))

(ert-deftest test-enkan-repl--save-load-roundtrip ()
  "Test save and load roundtrip preserves state."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace "01")
        ;; Initial state
        (enkan-repl--current-project "roundtrip-test")
        (enkan-repl-session-list '((10 . "rt10") (11 . "rt11") (12 . "rt12")))
        (enkan-repl--session-counter 3)
        (enkan-repl-project-aliases '(("r10" . "rt10") ("r11" . "rt11") ("r12" . "rt12"))))
    ;; Save state
    (enkan-repl--save-workspace-state)
    ;; Modify globals
    (setq enkan-repl--current-project "modified")
    (setq enkan-repl-session-list nil)
    (setq enkan-repl--session-counter 0)
    (setq enkan-repl-project-aliases nil)
    ;; Load state back
    (enkan-repl--load-workspace-state)
    ;; Check state was restored
    (should (equal enkan-repl--current-project "roundtrip-test"))
    (should (equal enkan-repl-session-list '((10 . "rt10") (11 . "rt11") (12 . "rt12"))))
    (should (equal enkan-repl--session-counter 3))
    (should (equal enkan-repl-project-aliases '(("r10" . "rt10") ("r11" . "rt11") ("r12" . "rt12"))))))

(ert-deftest test-enkan-repl--save-workspace-state-overwrite ()
  "Test that saving workspace state overwrites existing state."
  (let ((enkan-repl--workspaces `((,(intern "01") . (:current-project "old"
                                          :session-list ((1 . "old1"))
                                          :session-counter 1
                                          :project-aliases (("o1" . "old1"))))))
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project "new")
        (enkan-repl-session-list '((2 . "new2")))
        (enkan-repl--session-counter 2)
        (enkan-repl-project-aliases '(("n2" . "new2"))))
    ;; Save new state
    (enkan-repl--save-workspace-state)
    ;; Check old state was replaced
    (let ((stored (assoc "01" enkan-repl--workspaces #'string=)))
      (should stored)
      (let ((stored-plist (cdr stored)))
        (should (equal (plist-get stored-plist :current-project) "new"))
        (should (equal (plist-get stored-plist :session-list) '((2 . "new2"))))
        (should (equal (plist-get stored-plist :session-counter) 2))
        (should (equal (plist-get stored-plist :project-aliases) '(("n2" . "new2"))))))))

(provide 'enkan-repl-workspace-state-test)
;;; enkan-repl-workspace-state-test.el ends here
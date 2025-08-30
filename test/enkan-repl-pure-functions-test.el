;;; enkan-repl-pure-functions-test.el --- Tests for pure functions -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for pure functions in enkan-repl

;;; Code:

(require 'ert)
(require 'enkan-repl)

;; Tests for enkan-repl--get-project-info-from-directories
(ert-deftest test-enkan-repl--get-project-info-from-directories ()
  "Test getting project info from directories."
  ;; Test with sample target directories
  (let ((target-directories '(("proj1" "Project One" . "/home/user/project1")
                              ("proj2" "Project Two" . "/home/user/project2")
                              ("test" "Test Project" . "/opt/test"))))
    ;; Test existing alias
    (should (equal (enkan-repl--get-project-info-from-directories "proj1" target-directories)
                   '("Project One" . "/home/user/project1")))
    (should (equal (enkan-repl--get-project-info-from-directories "proj2" target-directories)
                   '("Project Two" . "/home/user/project2")))
    (should (equal (enkan-repl--get-project-info-from-directories "test" target-directories)
                   '("Test Project" . "/opt/test")))
    ;; Test non-existing alias
    (should (null (enkan-repl--get-project-info-from-directories "nonexistent" target-directories)))
    ;; Test with empty directories
    (should (null (enkan-repl--get-project-info-from-directories "proj1" '())))
    ;; Test with nil directories
    (should (null (enkan-repl--get-project-info-from-directories "proj1" nil)))))

;; Tests for enkan-repl--get-current-session-state-info
(ert-deftest test-enkan-repl--get-current-session-state-info ()
  "Test getting current session state information."
  ;; Test with sample state values
  (let ((current-project '("alias1" "alias2"))
        (session-list '((1 . "project1") (2 . "project2")))
        (session-counter 3)
        (project-aliases '(("alias1" . "Project One") ("alias2" . "Project Two"))))
    ;; Call the pure function with parameters
    (let ((state-info (enkan-repl--get-current-session-state-info
                       current-project session-list session-counter project-aliases)))
      ;; Verify the returned alist structure
      (should (equal (cdr (assoc 'current-project state-info)) current-project))
      (should (equal (cdr (assoc 'session-list state-info)) session-list))
      (should (equal (cdr (assoc 'session-counter state-info)) session-counter))
      (should (equal (cdr (assoc 'project-aliases state-info)) project-aliases))))
  
  ;; Test with nil values
  (let ((state-info (enkan-repl--get-current-session-state-info nil nil nil nil)))
    (should (null (cdr (assoc 'current-project state-info))))
    (should (null (cdr (assoc 'session-list state-info))))
    (should (null (cdr (assoc 'session-counter state-info))))
    (should (null (cdr (assoc 'project-aliases state-info)))))
  
  ;; Test with empty lists
  (let ((state-info (enkan-repl--get-current-session-state-info '() '() 0 '())))
    (should (equal (cdr (assoc 'current-project state-info)) '()))
    (should (equal (cdr (assoc 'session-list state-info)) '()))
    (should (equal (cdr (assoc 'session-counter state-info)) 0))
    (should (equal (cdr (assoc 'project-aliases state-info)) '()))))

(provide 'enkan-repl-pure-functions-test)
;;; enkan-repl-pure-functions-test.el ends here
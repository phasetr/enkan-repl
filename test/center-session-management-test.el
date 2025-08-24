;;; center-session-management-test.el --- Tests for session management functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>  
;; Keywords: test

;;; Commentary:

;; Tests for session management functions that were previously untested.

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;;; Session clearing tests

(ert-deftest test-center-clear-sessions ()
  "Test clearing all registered sessions."
  (let ((enkan-repl-session-list '((1 . "test-project")))
        (enkan-repl-center-project-registry '(("test" . ("test-project" . "/path")))))
    (enkan-repl-center-clear-sessions)
    (should (null enkan-repl-session-list))
    (should (null enkan-repl-center-project-registry))))

;;;; Session listing tests

(ert-deftest test-center-list-sessions-with-sessions ()
  "Test listing sessions when sessions exist."
  (let ((enkan-repl-session-list '((1 . "project1") (2 . "project2")))
        (output-buffer nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer) (setq output-buffer buffer))))
      (enkan-repl-center-list-sessions)
      (should output-buffer))))

(ert-deftest test-center-list-sessions-empty ()
  "Test listing sessions when no sessions exist."
  (let ((enkan-repl-session-list '())
        (message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (setq message-called t))))
      (enkan-repl-center-list-sessions)
      (should message-called))))

;;;; Auto-setup tests

(ert-deftest test-center-auto-setup-function-exists ()
  "Test that auto-setup function exists."
  (should (fboundp 'enkan-repl-center-auto-setup)))

;;;; Help function tests

(ert-deftest test-center-file-help-function-exists ()
  "Test that help function exists and can be called."
  (should (fboundp 'enkan-repl-center-file-help))
  (let ((help-buffer nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buffer) (setq help-buffer buffer))))
      (enkan-repl-center-file-help)
      (should help-buffer))))

(provide 'center-session-management-test)

;;; center-session-management-test.el ends here
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

;;;; Buffer killing tests

(ert-deftest test-center-finish-all-sessions-uses-correct-buffer-format ()
  "Test that enkan-repl-center-finish-all-sessions uses *enkan:* buffer format like standard version."
  ;; Setup test environment with mock registry - registry uses (alias . (project-name . project-path))
  (let ((enkan-repl-center-project-registry
         '(("alias1" . ("Project One" . "/tmp/project1"))
           ("alias2" . ("Project Two" . "/tmp/project2"))))
        ;; Session list uses (session-number . project-name)
        (enkan-repl-session-list '((1 . "Project One") (2 . "Project Two")))
        (killed-buffers '())
        (get-buffer-calls '())
        (enkan-buffer-found nil))
    
    ;; Mock enkan-repl--get-buffer-for-directory to simulate finding *enkan:* buffers
    (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (directory)
                 (push directory get-buffer-calls)
                 ;; Simulate finding a buffer with *enkan:* format
                 (when (or (string= directory "/tmp/project1")
                           (string= directory "/tmp/project2"))
                   (setq enkan-buffer-found t)
                   ;; Return a mock buffer object
                   (get-buffer-create (format "*enkan:%s*" directory)))))
              ((symbol-function 'kill-buffer)
               (lambda (buffer)
                 (when buffer
                   (push (buffer-name buffer) killed-buffers))
                 t))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t)))
      
      ;; Execute the function
      (enkan-repl-center-finish-all-sessions)
      
      ;; Verify that the correct directories were checked
      (should (member "/tmp/project1" get-buffer-calls))
      (should (member "/tmp/project2" get-buffer-calls))
      
      ;; Verify that buffers with *enkan:* format were killed
      (should (member "*enkan:/tmp/project1*" killed-buffers))
      (should (member "*enkan:/tmp/project2*" killed-buffers))
      
      ;; Verify that enkan-repl--get-buffer-for-directory was called
      (should enkan-buffer-found))))

(ert-deftest test-center-finish-uses-same-buffer-discovery-as-standard ()
  "Test that center version uses same buffer discovery method as standard enkan-repl-finish-eat."
  ;; Setup test with a specific project - correct registry format
  (let ((enkan-repl-center-project-registry
         '(("testalias" . ("Test Project" . "/home/test/project"))))
        ;; Session uses project name, not alias
        (enkan-repl-session-list '((1 . "Test Project")))
        (buffer-discovery-method-called nil))
    
    ;; Mock enkan-repl--get-buffer-for-directory to verify it's being called
    (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (directory)
                 (setq buffer-discovery-method-called directory)
                 ;; Return nil to simulate no buffer found
                 nil))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t)))
      
      ;; Execute the function
      (enkan-repl-center-finish-all-sessions)
      
      ;; Verify that the same buffer discovery method was used as standard version
      (should (string= buffer-discovery-method-called "/home/test/project")))))

(provide 'center-session-management-test)

;;; center-session-management-test.el ends here
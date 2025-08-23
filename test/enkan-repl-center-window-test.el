;;; enkan-repl-center-window-test.el --- Tests for center window navigation -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for pure functions in center-window-navigation.el

;;; Code:

(require 'ert)
(load-file "enkan-repl.el")
(load-file "examples/center-window-navigation.el")

;;;; Pure function tests

(ert-deftest enkan-repl-test--get-session-project-path-pure ()
  "Test pure function for getting session project path."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (session-list '((4 . "pt-tools") (5 . "enkan-repl") (6 . "claude-code")))
         (project-registry `(("pt" . ("pt-tools" . ,(concat temp-dir "pt-tools")))
                             ("er" . ("enkan-repl" . ,(concat temp-dir "enkan-repl")))
                             ("cc" . ("claude-code" . ,(concat temp-dir "claude-code"))))))
    ;; Valid session numbers - should find project by project name
    (should (string= (concat temp-dir "pt-tools")
                     (enkan-repl--get-session-project-path-pure 4 session-list project-registry)))
    (should (string= (concat temp-dir "enkan-repl")
                     (enkan-repl--get-session-project-path-pure 5 session-list project-registry)))
    (should (string= (concat temp-dir "claude-code")
                     (enkan-repl--get-session-project-path-pure 6 session-list project-registry)))
    
    ;; Invalid session number
    (should (null (enkan-repl--get-session-project-path-pure 99 session-list project-registry)))
    
    ;; Empty lists
    (should (null (enkan-repl--get-session-project-path-pure 4 '() project-registry)))
    (should (null (enkan-repl--get-session-project-path-pure 4 session-list '())))
    
    ;; Project not in registry
    (let ((missing-project-session-list '((4 . "missing-project"))))
      (should (null (enkan-repl--get-session-project-path-pure 4 missing-project-session-list project-registry))))))

(ert-deftest enkan-repl-test--setup-window-dired-pure ()
  "Test pure function for window dired setup."
  (let* ((temp-dir (file-name-as-directory temporary-file-directory))
         (session-list '((4 . "pt-tools")))
         (project-registry `(("pt" . ("pt-tools" . ,temp-dir))))
         (mock-window 'test-window))
    
    ;; Create temp directory for test
    (make-directory temp-dir t)
    
    ;; Valid setup
    (let ((result (enkan-repl--setup-window-dired-pure 
                   mock-window 4 session-list project-registry)))
      (should (equal (car result) mock-window))
      (should (string= (cdr result) temp-dir)))
    
    ;; Invalid session number
    (should (null (enkan-repl--setup-window-dired-pure 
                   mock-window 99 session-list project-registry)))
    
    ;; Non-existent directory
    (let ((bad-registry `(("pt" . ("pt-tools" . "/non-existent-directory")))))
      (should (null (enkan-repl--setup-window-dired-pure 
                     mock-window 4 session-list bad-registry))))))

;;;; Error handling tests

(ert-deftest enkan-repl-test--2session-layout-error-handling ()
  "Test error handling in 2-session layout setup."
  (cl-letf (((symbol-function 'delete-other-windows) (lambda ()))
            ((symbol-function 'split-window-right) (lambda (&optional size) 'window-2))
            ((symbol-function 'split-window-below) (lambda (&optional size) 'window-3))
            ((symbol-function 'other-window) (lambda (n)))
            ((symbol-function 'balance-windows) (lambda ()))
            ((symbol-function 'window-list) (lambda () '(window-1 window-2 window-3 window-4 window-5)))
            ((symbol-function 'window-width) (lambda () 100))
            ((symbol-function 'window-height) (lambda () 30))
            ((symbol-function 'floor) (lambda (n) (truncate n)))
            ((symbol-function 'select-window) (lambda (window)))
            ((symbol-function 'dired) (lambda (directory))))
    
    ;; Test error when variables are not bound
    (should-error
     (let ((enkan-repl-session-list)
           (enkan-repl-center-project-registry))
       (makunbound 'enkan-repl-session-list)
       (makunbound 'enkan-repl-center-project-registry)
       (enkan-repl-setup-2session-layout))
     :type 'error)
    
    ;; Test error when session list is nil
    (should-error
     (let ((enkan-repl-session-list nil)
           (enkan-repl-center-project-registry '(("pt" . ("pt-tools" . "/tmp")))))
       (enkan-repl-setup-2session-layout))
     :type 'error)
    
    ;; Test error when project registry is nil
    (should-error
     (let ((enkan-repl-session-list '((4 . "pt-tools")))
           (enkan-repl-center-project-registry nil))
       (enkan-repl-setup-2session-layout))
     :type 'error)))

(provide 'enkan-repl-center-window-test)

;;; enkan-repl-center-window-test.el ends here
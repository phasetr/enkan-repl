;;; claudemacs-repl-session-controller-test.el --- Tests for Session Controller functions -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive tests for claudemacs-repl-start-claudemacs and claudemacs-repl-finish-claudemacs
;; Tests cover helper functions, main functions, error conditions, and edge cases.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the main file - try multiple approaches for robustness
(unless (featurep 'claudemacs-repl)
  (or (ignore-errors (require 'claudemacs-repl))
      (let ((main-file (expand-file-name "../claudemacs-repl.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
      (error "Could not load claudemacs-repl.el")))

;;;; Helper Function Tests

(ert-deftest test-claudemacs-repl--get-session-info-structure ()
  "Test that get-session-info returns correct structure."
  (let ((info (claudemacs-repl--get-session-info)))
    (should (listp info))
    (should (= 3 (length info)))
    (should (stringp (nth 0 info)))  ; target-dir should be string
    ;; Note: existing-buffer and can-send may be nil, which is valid
    ))

(ert-deftest test-claudemacs-repl--get-session-info-consistency ()
  "Test that get-session-info returns consistent results across multiple calls."
  (let ((info1 (claudemacs-repl--get-session-info))
        (info2 (claudemacs-repl--get-session-info)))
    (should (equal (nth 0 info1) (nth 0 info2)))  ; Same target directory
    (should (equal (nth 1 info1) (nth 1 info2)))  ; Same existing buffer
    (should (equal (nth 2 info1) (nth 2 info2))))) ; Same can-send status

(ert-deftest test-claudemacs-repl--execute-claudemacs-command-error-handling ()
  "Test error handling in execute-claudemacs-command."
  ;; Mock require to succeed but fboundp to fail for specific function
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
            ((symbol-function 'fboundp) (lambda (sym) (not (eq sym 'non-existent-function))))
            ((symbol-function 'cd) (lambda (&rest _) nil)))
    (let ((error-thrown nil)
          (error-message nil))
      (condition-case err
          (claudemacs-repl--execute-claudemacs-command 
           'non-existent-function
           "Should not reach this message")
        (error 
         (setq error-thrown t)
         (setq error-message (error-message-string err))))
      ;; Verify error was thrown with specific message about the function
      (should error-thrown)
      (should (string-match-p "non-existent-function not available after loading claudemacs package" 
                              error-message)))))

(ert-deftest test-claudemacs-repl--handle-dead-session-no-restart ()
  "Test handle-dead-session without restart function."
  ;; Create a temporary buffer to simulate dead session
  (let ((test-buffer (generate-new-buffer "*test-dead-session*"))
        (test-dir "/tmp/test")
        (called-restart nil))
    (unwind-protect
        (progn
          ;; Mock y-or-n-p to return t
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (claudemacs-repl--handle-dead-session 
             test-buffer test-dir 
             "Test prompt for %s?"
             nil)
            ;; Buffer should be killed
            (should-not (buffer-live-p test-buffer))))
      ;; Cleanup: kill buffer if it still exists
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest test-claudemacs-repl--handle-dead-session-with-restart ()
  "Test handle-dead-session with restart function."
  (let ((test-buffer (generate-new-buffer "*test-dead-session-restart*"))
        (test-dir "/tmp/test")
        (restart-called nil))
    (unwind-protect
        (progn
          ;; Mock functions
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (claudemacs-repl--handle-dead-session 
             test-buffer test-dir 
             "Test restart prompt for %s?"
             (lambda () (setq restart-called t)))
            ;; Buffer should be killed and restart should be called
            (should-not (buffer-live-p test-buffer))
            (should restart-called)))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

;;;; Main Function Tests

(ert-deftest test-claudemacs-repl-start-claudemacs-exists ()
  "Test that start-claudemacs function exists and is interactive."
  (should (fboundp 'claudemacs-repl-start-claudemacs))
  (should (commandp 'claudemacs-repl-start-claudemacs)))

(ert-deftest test-claudemacs-repl-finish-claudemacs-exists ()
  "Test that finish-claudemacs function exists and is interactive."
  (should (fboundp 'claudemacs-repl-finish-claudemacs))
  (should (commandp 'claudemacs-repl-finish-claudemacs)))

(ert-deftest test-start-claudemacs-no-existing-session-error ()
  "Test start-claudemacs behavior when claudemacs package is not available."
  ;; Mock session info to return no existing session
  (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
             (lambda () (list "/tmp/test" nil nil)))
            ((symbol-function 'claudemacs-repl--execute-claudemacs-command)
             (lambda (&rest _) (error "Claudemacs package not found"))))
    (should-error (claudemacs-repl-start-claudemacs) :type 'error)))

(ert-deftest test-start-claudemacs-existing-active-session ()
  "Test start-claudemacs with existing active session."
  (let ((message-called nil)
        (test-buffer (generate-new-buffer "*test-active-session*")))
    (unwind-protect
        (progn
          ;; Mock session info to return active session
          (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
                     (lambda () (list "/tmp/test" test-buffer t)))
                    ((symbol-function 'message)
                     (lambda (&rest args) (setq message-called args))))
            (claudemacs-repl-start-claudemacs)
            ;; Should have called message about existing session
            (should message-called)
            (should (string-match-p "already running" (car message-called)))))
      (kill-buffer test-buffer))))

(ert-deftest test-finish-claudemacs-no-session ()
  "Test finish-claudemacs with no existing session."
  (let ((message-called nil))
    ;; Mock session info to return no session
    (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
               (lambda () (list "/tmp/test" nil nil)))
              ((symbol-function 'message)
               (lambda (&rest args) (setq message-called args))))
      (claudemacs-repl-finish-claudemacs)
      ;; Should have called message about no session found
      (should message-called)
      (should (string-match-p "No claudemacs session found" (car message-called))))))

(ert-deftest test-finish-claudemacs-active-session-cancel ()
  "Test finish-claudemacs with active session but user cancels."
  (let ((test-buffer (generate-new-buffer "*test-active-finish*"))
        (execute-called nil))
    (unwind-protect
        (progn
          ;; Mock session info and user input
          (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
                     (lambda () (list "/tmp/test" test-buffer t)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))  ; User cancels
                    ((symbol-function 'claudemacs-repl--execute-claudemacs-command)
                     (lambda (&rest _) (setq execute-called t))))
            (claudemacs-repl-finish-claudemacs)
            ;; Execute should not be called since user cancelled
            (should-not execute-called)))
      (kill-buffer test-buffer))))

;;;; Documentation and Structure Tests

(ert-deftest test-both-functions-have-proper-category ()
  "Test that both functions have proper Category documentation."
  (let ((start-doc (documentation 'claudemacs-repl-start-claudemacs))
        (finish-doc (documentation 'claudemacs-repl-finish-claudemacs)))
    (should (string-match-p "Category: Session Controller" start-doc))
    (should (string-match-p "Category: Session Controller" finish-doc))))

(ert-deftest test-both-functions-mention-filename-detection ()
  "Test that both functions mention filename-based directory detection."
  (let ((start-doc (documentation 'claudemacs-repl-start-claudemacs))
        (finish-doc (documentation 'claudemacs-repl-finish-claudemacs)))
    (should (string-match-p "filename" start-doc))
    (should (string-match-p "filename" finish-doc))))

(ert-deftest test-function-symmetry ()
  "Test that start and finish functions have symmetric structure."
  ;; Both should use the same helper functions
  (should (fboundp 'claudemacs-repl--get-session-info))
  (should (fboundp 'claudemacs-repl--execute-claudemacs-command))
  (should (fboundp 'claudemacs-repl--handle-dead-session))
  
  ;; Both should be interactive commands
  (should (commandp 'claudemacs-repl-start-claudemacs))
  (should (commandp 'claudemacs-repl-finish-claudemacs))
  
  ;; Both should have similar documentation structure
  (let ((start-doc (documentation 'claudemacs-repl-start-claudemacs))
        (finish-doc (documentation 'claudemacs-repl-finish-claudemacs)))
    (should (stringp start-doc))
    (should (stringp finish-doc))))

;;;; Integration Tests

(ert-deftest test-start-finish-integration-no-claudemacs ()
  "Integration test: start and finish with no claudemacs available."
  ;; Mock session info to simulate no existing sessions
  (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
             (lambda () (list "/tmp/test" nil nil)))
            ((symbol-function 'require)
             (lambda (feature &optional filename noerror)
               (if (eq feature 'claudemacs)
                   nil  ; Simulate claudemacs not available
                 (funcall (symbol-function 'require) feature filename noerror)))))
    ;; Start should fail
    (should-error (claudemacs-repl-start-claudemacs) :type 'error)
    ;; Finish should report no session
    (let ((message-content nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (setq message-content (car args)))))
        (claudemacs-repl-finish-claudemacs)
        (should (string-match-p "No claudemacs session found" message-content))))))

;;;; Edge Case Tests  

(ert-deftest test-helper-functions-with-nil-values ()
  "Test helper functions handle nil values gracefully."
  ;; Test get-session-info with mocked functions returning nil
  (cl-letf (((symbol-function 'claudemacs-repl--get-target-directory-for-buffer)
             (lambda () nil))
            ((symbol-function 'claudemacs-repl--get-buffer-for-directory)
             (lambda (&rest _) nil))
            ((symbol-function 'claudemacs-repl--can-send-text)
             (lambda (&rest _) nil)))
    (let ((info (claudemacs-repl--get-session-info)))
      (should (listp info))
      (should (= 3 (length info))))))

(ert-deftest test-execute-command-with-directory-change-failure ()
  "Test execute-claudemacs-command when directory change fails."
  ;; Mock cd to fail
  (cl-letf (((symbol-function 'cd)
             (lambda (&rest _) (error "Cannot change directory"))))
    (should-error 
     (claudemacs-repl--execute-claudemacs-command 
      'some-command "Success message")
     :type 'error)))

;;;; Performance and Resource Tests

(ert-deftest test-no-resource-leaks ()
  "Test that functions don't create resource leaks."
  (let ((initial-buffer-count (length (buffer-list))))
    ;; Run both functions with mocked inputs
    (cl-letf (((symbol-function 'claudemacs-repl--get-session-info)
               (lambda () (list "/tmp/test" nil nil)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      ;; These should not create permanent buffers or processes
      (ignore-errors (claudemacs-repl-start-claudemacs))
      (ignore-errors (claudemacs-repl-finish-claudemacs))
      ;; Buffer count should not have increased significantly
      (should (<= (length (buffer-list)) (+ initial-buffer-count 2))))))

(provide 'claudemacs-repl-session-controller-test)

;;; claudemacs-repl-session-controller-test.el ends here
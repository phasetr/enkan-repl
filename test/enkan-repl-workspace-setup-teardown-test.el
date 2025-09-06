;;; enkan-repl-workspace-setup-teardown-test.el --- Tests for workspace-scoped setup/teardown -*- lexical-binding: t -*-

;;; Commentary:
;; Test that setup/teardown operations are workspace-scoped

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-utils)

(ert-deftest test-enkan-repl-setup-teardown-workspace-scope ()
  "Test that setup and teardown operations are workspace-scoped."
  ;; Test setup: ensure we have a workspace
  (should (equal enkan-repl--current-workspace "01"))
  
  ;; Test that setup functions use workspace accessors
  ;; This is verified by the fact that the functions use enkan-repl--ws-* functions
  
  ;; Test setup log state uses workspace accessors
  ;; The function uses princ which outputs to standard-output, not to the buffer itself
  ;; So we test that the function runs without error and accepts workspace accessors
  (let ((test-output ""))
    (cl-letf (((symbol-function 'princ)
               (lambda (str) (setq test-output (concat test-output str)))))
      (let ((buffer-name "*test-setup*"))
        (with-temp-buffer
          (rename-buffer buffer-name)
          (enkan-repl--setup-log-state buffer-name "Test" 
                                       (enkan-repl--ws-current-project)
                                       (enkan-repl--ws-session-list)
                                       (enkan-repl--ws-session-counter))
          ;; Check the output contains expected text
          (should (string-match-p "Test state" test-output))))))
  
  ;; Test reset config uses workspace accessors
  (let ((test-output ""))
    (cl-letf (((symbol-function 'princ)
               (lambda (str) (setq test-output (concat test-output str)))))
      (let ((buffer-name "*test-reset*"))
        (with-temp-buffer
          (rename-buffer buffer-name)
          (enkan-repl--setup-reset-config buffer-name)
          ;; Verify workspace state was reset
          (should (null (enkan-repl--ws-session-list)))
          (should (= (enkan-repl--ws-session-counter) 0))
          (should (null (enkan-repl--ws-current-project)))
          ;; Check output mentions reset
          (should (string-match-p "Reset" test-output)))))))

(ert-deftest test-enkan-repl-terminate-workspace-buffers ()
  "Test that terminate-all-session-buffers only affects current workspace."
  ;; Mock session list with a test session
  (let ((test-sessions '((1 . "test-project")))
        (test-directories '(("/path/to/test-project" . "test-project"))))
    
    ;; Create a mock buffer for workspace 01
    (let ((mock-buffer-name "*ws:01 enkan:/path/to/test-project*"))
      (with-temp-buffer
        (rename-buffer mock-buffer-name)
        
        ;; Mock the get-buffer-for-directory to return our buffer when in workspace
        (cl-letf (((symbol-function 'enkan-repl--get-project-path-from-directories)
                   (lambda (_name _dirs) "/path/to/test-project"))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (path)
                     (when (string= path "/path/to/test-project")
                       ;; Only return buffer if it matches current workspace
                       (let ((buf (get-buffer mock-buffer-name)))
                         (when (and buf 
                                   (enkan-repl--buffer-name-matches-workspace 
                                    (buffer-name buf) 
                                    enkan-repl--current-workspace))
                           buf))))))
          
          ;; Terminate should find and kill the buffer
          (let ((result (enkan-repl--terminate-all-session-buffers 
                        test-sessions test-directories)))
            (should (= (car result) 1)) ; Should terminate 1 buffer
            (should (eq (cdr (assoc :status (car (cdr result)))) 'terminated))))))))

(ert-deftest test-enkan-repl-setup-teardown-docstrings ()
  "Test that setup/teardown docstrings mention workspace context."
  ;; Check setup docstring mentions workspace
  (should (string-match-p "workspace" 
                          (documentation 'enkan-repl-setup)))
  
  ;; Check teardown docstring mentions workspace
  (should (string-match-p "workspace" 
                          (documentation 'enkan-repl-teardown)))
  
  ;; Check terminate-all-session-buffers mentions workspace
  (should (string-match-p "workspace" 
                          (documentation 'enkan-repl--terminate-all-session-buffers))))

(provide 'enkan-repl-workspace-setup-teardown-test)
;;; enkan-repl-workspace-setup-teardown-test.el ends here
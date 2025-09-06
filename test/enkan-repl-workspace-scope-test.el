;;; enkan-repl-workspace-scope-test.el --- Tests for workspace scope filtering -*- lexical-binding: t -*-

;;; Commentary:
;; Test cases for workspace-scoped buffer filtering

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-utils)

(ert-deftest test-enkan-repl--buffer-name-matches-workspace ()
  "Test workspace matching in buffer names."
  ;; Test matching workspace
  (should (enkan-repl--buffer-name-matches-workspace
           "*ws:01 enkan:/home/user/project*" "01"))
  
  ;; Test non-matching workspace
  (should-not (enkan-repl--buffer-name-matches-workspace
               "*ws:02 enkan:/home/user/project*" "01"))
  
  ;; Test invalid buffer name
  (should-not (enkan-repl--buffer-name-matches-workspace
               "*enkan:/home/user/project*" "01"))
  
  ;; Test nil inputs
  (should-not (enkan-repl--buffer-name-matches-workspace nil "01"))
  (should-not (enkan-repl--buffer-name-matches-workspace "*ws:01 enkan:/path*" nil)))

(ert-deftest test-enkan-repl--extract-workspace-id ()
  "Test extraction of workspace ID from buffer names."
  ;; Test valid workspace buffer
  (should (equal (enkan-repl--extract-workspace-id
                  "*ws:01 enkan:/home/user/project*") "01"))
  
  ;; Test different workspace ID
  (should (equal (enkan-repl--extract-workspace-id
                  "*ws:99 enkan:/home/user/project*") "99"))
  
  ;; Test invalid format
  (should-not (enkan-repl--extract-workspace-id
               "*enkan:/home/user/project*"))
  
  ;; Test nil input
  (should-not (enkan-repl--extract-workspace-id nil)))

(ert-deftest test-enkan-repl--get-available-buffers-with-workspace ()
  "Test that get-available-buffers filters by workspace."
  ;; Save current workspace
  (let ((original-ws enkan-repl--current-workspace))
    (unwind-protect
        (progn
          ;; Set current workspace to 01
          (setq enkan-repl--current-workspace "01")
          
          ;; Create mock buffers with different workspace IDs
          (let* ((buffer-ws01-1 (get-buffer-create "*ws:01 enkan:/project1*"))
                 (buffer-ws01-2 (get-buffer-create "*ws:01 enkan:/project2*"))
                 (buffer-ws02 (get-buffer-create "*ws:02 enkan:/project3*"))
                 (buffer-list (list buffer-ws01-1 buffer-ws01-2 buffer-ws02)))
            
            ;; Mock eat--process for testing
            (dolist (buf (list buffer-ws01-1 buffer-ws01-2 buffer-ws02))
              (with-current-buffer buf
                (setq-local eat--process 'mock-process)))
            
            ;; Mock process-live-p
            (cl-letf (((symbol-function 'process-live-p) (lambda (_) t)))
              ;; Test filtering - should only return ws:01 buffers
              (let ((result (enkan-repl--get-available-buffers buffer-list)))
                (should (= (length result) 2))
                (should (member buffer-ws01-1 result))
                (should (member buffer-ws01-2 result))
                (should-not (member buffer-ws02 result))))
            
            ;; Clean up buffers
            (dolist (buf buffer-list)
              (kill-buffer buf))))
      ;; Restore original workspace
      (setq enkan-repl--current-workspace original-ws))))

(ert-deftest test-enkan-repl--get-buffer-for-directory-with-workspace ()
  "Test that get-buffer-for-directory filters by workspace."
  ;; Save current workspace
  (let ((original-ws enkan-repl--current-workspace))
    (unwind-protect
        (progn
          ;; Set current workspace to 01
          (setq enkan-repl--current-workspace "01")
          
          ;; Create mock buffers
          (let* ((buffer-ws01 (get-buffer-create "*ws:01 enkan:/home/test*"))
                 (buffer-ws02 (get-buffer-create "*ws:02 enkan:/home/test*")))
            
            ;; Mock eat-mode for both buffers
            (dolist (buf (list buffer-ws01 buffer-ws02))
              (with-current-buffer buf
                (setq-local eat-mode t)))
            
            ;; Mock enkan-repl--buffer-matches-directory to return true
            (cl-letf (((symbol-function 'enkan-repl--buffer-matches-directory)
                       (lambda (name dir) t)))
              ;; Should return ws:01 buffer, not ws:02
              (let ((result (enkan-repl--get-buffer-for-directory "/home/test")))
                (should (eq result buffer-ws01))))
            
            ;; Clean up buffers
            (kill-buffer buffer-ws01)
            (kill-buffer buffer-ws02)))
      ;; Restore original workspace
      (setq enkan-repl--current-workspace original-ws))))

(provide 'enkan-repl-workspace-scope-test)

;;; enkan-repl-workspace-scope-test.el ends here
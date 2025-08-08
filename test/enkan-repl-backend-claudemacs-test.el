;;; enkan-repl-backend-claudemacs-test.el --- Tests for claudemacs backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the claudemacs backend implementation.
;; This ensures compatibility with existing claudemacs functionality.

;;; Code:

(require 'ert)
(require 'enkan-repl-backend)
(require 'enkan-repl-backend-claudemacs)

(ert-deftest enkan-repl-backend-claudemacs-test-session-creation ()
  "Test creating a new claudemacs session."
  ;; claudemacs は必須のためskip-unlessを削除
  
  (let ((enkan-repl-backend-type 'claudemacs)
        (enkan-repl-sessions-alist nil)
        (test-dir "/tmp/test-claudemacs"))
    ;; Create directory if it doesn't exist
    (make-directory test-dir t)
    
    ;; Mock claudemacs-start to avoid actual process creation
    (cl-letf (((symbol-function 'claudemacs-start)
               (lambda () (get-buffer-create "*claudemacs*"))))
      
      ;; Create session
      (let ((buffer (enkan-repl-backend-create-session test-dir)))
        (should (bufferp buffer))
        (should (buffer-live-p buffer))
        (should (string-match "\\*enkan-claudemacs:" (buffer-name buffer)))
        
        ;; Check session is registered
        (should (= 1 (length enkan-repl-sessions-alist)))
        (let ((session (car enkan-repl-sessions-alist)))
          (should (eq (plist-get (cdr session) :backend) 'claudemacs))
          (should (string= (plist-get (cdr session) :directory) test-dir)))
        
        ;; Clean up
        (kill-buffer buffer)))))

(ert-deftest enkan-repl-backend-claudemacs-test-buffer-renaming ()
  "Test that claudemacs buffers are renamed to standard format."
  ;; claudemacs は必須のためskip-unlessを削除
  
  (let ((enkan-repl-backend-type 'claudemacs)
        (enkan-repl-sessions-alist nil)
        (test-dir "/tmp/test-rename"))
    (make-directory test-dir t)
    
    ;; Test different possible claudemacs buffer names
    (dolist (original-name '("*claudemacs*" "*claude*" "*claudemacs:/tmp/test-rename*"))
      (cl-letf (((symbol-function 'claudemacs-start)
                 (lambda () (get-buffer-create original-name))))
        
        (let ((buffer (enkan-repl-backend-create-session test-dir)))
          (should (bufferp buffer))
          ;; Buffer should be renamed to standard format
          (should (string-match "\\*enkan-claudemacs:" (buffer-name buffer)))
          ;; Original buffer should not exist
          (should-not (get-buffer original-name))
          
          ;; Clean up
          (kill-buffer buffer)
          (setq enkan-repl-sessions-alist nil))))))

(ert-deftest enkan-repl-backend-claudemacs-test-compatibility ()
  "Test compatibility with existing claudemacs features."
  ;; claudemacs は必須のためskip-unlessを削除
  
  (let ((enkan-repl-backend-type 'claudemacs)
        (test-buffer (get-buffer-create "*test-claudemacs-buffer*")))
    
    ;; Mock eat--process for testing
    (with-current-buffer test-buffer
      (setq-local eat--process 'mock-process))
    
    ;; Test alive check
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (proc) (eq proc 'mock-process))))
      (should (enkan-repl-backend-claudemacs-alive-p test-buffer)))
    
    ;; Test content retrieval
    (with-current-buffer test-buffer
      (insert "Test content"))
    (should (string= "Test content" 
                     (enkan-repl-backend-claudemacs-get-content test-buffer)))
    
    ;; Clean up
    (kill-buffer test-buffer)))

(ert-deftest enkan-repl-backend-claudemacs-test-send-text ()
  "Test sending text to claudemacs buffer."
  ;; claudemacs は必須のためskip-unlessを削除
  
  (let ((test-buffer (get-buffer-create "*test-send*"))
        (sent-text nil))
    
    ;; Mock eat--send-string
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (proc text) (setq sent-text (concat sent-text text))))
              ((symbol-function 'process-live-p)
               (lambda (proc) t))
              ((symbol-function 'recenter)
               (lambda (&optional arg) nil)))
      
      (with-current-buffer test-buffer
        (setq-local eat--process 'mock-process)
        
        ;; Send text
        (enkan-repl-backend-claudemacs-send "Test message" test-buffer)
        
        ;; Check that text and newline were sent
        (should (string= sent-text "Test message\n"))))
    
    ;; Clean up
    (kill-buffer test-buffer)))

(ert-deftest enkan-repl-backend-claudemacs-test-multiple-sessions ()
  "Test that multiple claudemacs sessions can coexist."
  ;; claudemacs は必須のためskip-unlessを削除
  
  (let ((enkan-repl-backend-type 'claudemacs)
        (enkan-repl-sessions-alist nil)
        (test-dir1 "/tmp/claudemacs-1")
        (test-dir2 "/tmp/claudemacs-2"))
    
    (make-directory test-dir1 t)
    (make-directory test-dir2 t)
    
    (cl-letf (((symbol-function 'claudemacs-start)
               (lambda () (get-buffer-create (format "*claudemacs-%s*" (random 1000))))))
      
      ;; Create first session
      (let ((buffer1 (enkan-repl-backend-create-session test-dir1)))
        (should (bufferp buffer1))
        
        ;; Create second session with new-session-p
        (let ((buffer2 (enkan-repl-backend-create-session test-dir2)))
          (should (bufferp buffer2))
          (should-not (eq buffer1 buffer2))
          
          ;; Check both sessions are registered
          (should (= 2 (length enkan-repl-sessions-alist)))
          
          ;; Clean up
          (kill-buffer buffer1)
          (kill-buffer buffer2))))))

(provide 'enkan-repl-backend-claudemacs-test)
;;; enkan-repl-backend-claudemacs-test.el ends here
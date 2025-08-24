;;; center-text-sending-test.el --- Tests for center file text sending functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for center file specific text sending functions.
;; These tests verify the pure function behavior for center file text sending.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-enkan-repl--center-send-text-to-buffer-with-valid-buffer ()
  "Test center text sending with valid buffer and process."
  (let ((test-buffer (generate-new-buffer "*test-enkan*"))
        (text-sent nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Mock eat--process - use start-process instead of make-network-process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process)
      
      ;; Mock eat--send-string to capture what's sent
      (cl-letf (((symbol-function 'eat--send-string)
                 (lambda (proc text)
                   (setq text-sent (cons text text-sent))))
                ((symbol-function 'process-live-p) 
                 (lambda (proc) t))
                ((symbol-function 'run-at-time)
                 (lambda (time repeat func &rest args)
                   ;; Execute immediately for testing
                   (apply func args))))
        
        ;; Test the function
        (let ((result (enkan-repl--center-send-text-to-buffer "test123" test-buffer)))
          (should (eq result t))
          (should (equal text-sent '("\r" "test123"))))))
    
    ;; Clean up
    (when (process-live-p process-obj)
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-enkan-repl--center-send-text-to-buffer-with-invalid-buffer ()
  "Test center text sending with invalid buffer."
  (let ((result (enkan-repl--center-send-text-to-buffer "test" nil)))
    (should (null result))))

(ert-deftest test-enkan-repl--center-send-text-to-buffer-with-dead-process ()
  "Test center text sending with dead process."
  (let ((test-buffer (generate-new-buffer "*test-enkan*")))
    (with-current-buffer test-buffer
      ;; Mock eat--process as dead - use start-process and then kill it
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (delete-process eat--process)
      
      ;; Test the function
      (let ((result (enkan-repl--center-send-text-to-buffer "test123" test-buffer)))
        (should (null result))))
    
    (kill-buffer test-buffer)))

(ert-deftest test-enkan-repl--center-send-text-to-buffer-cursor-positioning ()
  "Test center text sending cursor positioning behavior."
  (let ((test-buffer (generate-new-buffer "*test-enkan*"))
        (cursor-moved nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Set up buffer content and cursor
      (insert "line1\nline2\nline3")
      (goto-char (point-min))
      
      ;; Mock eat--process - use start-process instead of make-network-process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process)
      
      ;; Mock functions
      (cl-letf (((symbol-function 'eat--send-string)
                 (lambda (proc text) nil))
                ((symbol-function 'process-live-p) 
                 (lambda (proc) t))
                ((symbol-function 'run-at-time)
                 (lambda (time repeat func &rest args)
                   ;; Execute cursor positioning immediately
                   (apply func args)
                   (setq cursor-moved (= (point) (point-max))))))
        
        ;; Test the function
        (enkan-repl--center-send-text-to-buffer "test" test-buffer)
        (should cursor-moved)))
    
    ;; Clean up
    (when (process-live-p process-obj)
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(provide 'center-text-sending-test)

;;; center-text-sending-test.el ends here
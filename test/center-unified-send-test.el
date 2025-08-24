;;; center-unified-send-test.el --- Tests for unified center file send command -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unified tests for center send functionality including:
;; - Send input parsing and dispatching
;; - Text sending to buffers  
;; - Center send command integration

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-center-parse-send-input-literal-text ()
  "Test parsing of literal text input."
  (let ((result (enkan-repl-center--parse-send-input nil "hello world")))
    (should (equal (alist-get :target-buffer result) nil))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) "hello world"))))

(ert-deftest test-center-parse-send-input-numeric-prefix ()
  "Test parsing with numeric prefix argument."
  (let ((result (enkan-repl-center--parse-send-input 3 "test message")))
    (should (equal (alist-get :target-buffer result) 3))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) "test message"))))

(ert-deftest test-center-parse-send-input-er-prefix-with-text ()
  "Test parsing of :er prefix with buffer name and text."
  (let ((result (enkan-repl-center--parse-send-input nil ":er mybuffer hello world")))
    (should (equal (alist-get :target-buffer result) "mybuffer"))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) "hello world"))))

(ert-deftest test-center-parse-send-input-er-prefix-only-buffer ()
  "Test parsing of :er prefix with only buffer name."
  (let ((result (enkan-repl-center--parse-send-input nil ":er mybuffer")))
    (should (equal (alist-get :target-buffer result) "mybuffer"))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) ""))))

(ert-deftest test-center-parse-send-input-special-commands ()
  "Test parsing of special command strings."
  (let ((result-esc (enkan-repl-center--parse-send-input nil ":esc"))
        (result-ret (enkan-repl-center--parse-send-input nil ":ret")))
    ;; Test :esc command
    (should (equal (alist-get :target-buffer result-esc) nil))
    (should (equal (car (alist-get :action result-esc)) :key))
    (should (equal (cdr (alist-get :action result-esc)) 'escape))
    ;; Test :ret command
    (should (equal (alist-get :target-buffer result-ret) nil))
    (should (equal (car (alist-get :action result-ret)) :key))
    (should (equal (cdr (alist-get :action result-ret)) 'return))))

(ert-deftest test-center-parse-send-input-er-with-special-commands ()
  "Test parsing of :er prefix combined with special commands."
  (let ((result (enkan-repl-center--parse-send-input nil ":er mybuffer :esc")))
    (should (equal (alist-get :target-buffer result) "mybuffer"))
    (should (equal (car (alist-get :action result)) :key))
    (should (equal (cdr (alist-get :action result)) 'escape))))

(ert-deftest test-center-parse-send-input-empty-string ()
  "Test parsing of empty string input."
  (let ((result (enkan-repl-center--parse-send-input nil "")))
    (should (equal (alist-get :target-buffer result) nil))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) ""))))

(ert-deftest test-center-parse-send-input-numeric-prefix-overrides-er ()
  "Test that numeric prefix takes precedence when :er is not used."
  (let ((result (enkan-repl-center--parse-send-input 5 "normal text")))
    (should (equal (alist-get :target-buffer result) 5))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) "normal text"))))

(ert-deftest test-center-parse-send-input-complex-buffer-names ()
  "Test parsing with complex buffer names containing special characters."
  (let ((result (enkan-repl-center--parse-send-input nil ":er my-complex_buffer123 test message")))
    (should (equal (alist-get :target-buffer result) "my-complex_buffer123"))
    (should (equal (car (alist-get :action result)) :string))
    (should (equal (cdr (alist-get :action result)) "test message"))))

;;; Tests for buffer resolution function

(ert-deftest test-center-resolve-target-buffer-numeric ()
  "Test buffer resolution with numeric specification."
  (let* ((buffer1 (generate-new-buffer "*test1*"))
         (buffer2 (generate-new-buffer "*test2*"))
         (buffers (list buffer1 buffer2)))
    (unwind-protect
        (progn
          (should (eq (enkan-repl-center--resolve-target-buffer-pure 1 buffers) buffer1))
          (should (eq (enkan-repl-center--resolve-target-buffer-pure 2 buffers) buffer2))
          (should (null (enkan-repl-center--resolve-target-buffer-pure 3 buffers))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-center-resolve-target-buffer-string-match ()
  "Test buffer resolution with string specification."
  (let* ((buffer1 (generate-new-buffer "*enkan:test1*"))
         (buffer2 (generate-new-buffer "*enkan:test2*"))
         (buffer3 (generate-new-buffer "*other*"))
         (buffers (list buffer1 buffer2 buffer3)))
    (unwind-protect
        (progn
          ;; Test partial match
          (should (eq (enkan-repl-center--resolve-target-buffer-pure "test1" buffers) buffer1))
          (should (eq (enkan-repl-center--resolve-target-buffer-pure "test2" buffers) buffer2))
          ;; Test no match
          (should (null (enkan-repl-center--resolve-target-buffer-pure "nonexistent" buffers)))
          ;; Test multiple matches (should return first)
          (should (eq (enkan-repl-center--resolve-target-buffer-pure "enkan" buffers) buffer1)))
      (kill-buffer buffer1)
      (kill-buffer buffer2)
      (kill-buffer buffer3))))

(ert-deftest test-center-resolve-target-buffer-nil-spec ()
  "Test buffer resolution with nil specification."
  (let* ((buffer1 (generate-new-buffer "*test*"))
         (buffers (list buffer1)))
    (unwind-protect
        (should (null (enkan-repl-center--resolve-target-buffer-pure nil buffers)))
      (kill-buffer buffer1))))

;;; Tests for send action validation function

(ert-deftest test-center-perform-send-action-pure-string ()
  "Test send action validation for string actions."
  (let ((buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (let ((result (enkan-repl-center--perform-send-action-pure buffer '(:string . "test"))))
          (should (plist-get result :valid))
          (should (eq (plist-get result :action-type) :string))
          (should (equal (plist-get result :action-value) "test")))
      (kill-buffer buffer))))

(ert-deftest test-center-perform-send-action-pure-key ()
  "Test send action validation for key actions."
  (let ((buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (progn
          ;; Test valid key
          (let ((result (enkan-repl-center--perform-send-action-pure buffer '(:key . escape))))
            (should (plist-get result :valid))
            (should (eq (plist-get result :action-type) :key))
            (should (eq (plist-get result :action-value) 'escape)))
          ;; Test invalid key
          (let ((result (enkan-repl-center--perform-send-action-pure buffer '(:key . invalid))))
            (should-not (plist-get result :valid))))
      (kill-buffer buffer))))

(ert-deftest test-center-perform-send-action-pure-invalid-buffer ()
  "Test send action validation with invalid buffer."
  (let ((result (enkan-repl-center--perform-send-action-pure nil '(:string . "test"))))
    (should-not (plist-get result :valid))
    (should (string-match-p "Invalid buffer" (plist-get result :message)))))

;;;; Text Sending Tests

(ert-deftest test-center-send-text-to-buffer-with-valid-buffer ()
  "Test center text sending with valid buffer and process."
  (let ((test-buffer (generate-new-buffer "*test-enkan*"))
        (text-sent nil)
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Mock eat--process
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
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-text-to-buffer-with-invalid-buffer ()
  "Test center text sending with invalid buffer."
  (let ((invalid-buffer (generate-new-buffer "*invalid*")))
    (unwind-protect
        ;; No eat--process set up in buffer
        (let ((result (enkan-repl--center-send-text-to-buffer "test" invalid-buffer)))
          (should (null result)))
      (kill-buffer invalid-buffer))))

(ert-deftest test-center-send-text-to-buffer-with-dead-process ()
  "Test center text sending with dead process."
  (let ((test-buffer (generate-new-buffer "*dead-process*"))
        (dead-process nil))
    (with-current-buffer test-buffer
      ;; Create and kill a process
      (setq dead-process (start-process "dead-test" nil "echo" "test"))
      (delete-process dead-process)
      (setq-local eat--process dead-process)
      
      ;; Test with dead process
      (let ((result (enkan-repl--center-send-text-to-buffer "test" test-buffer)))
        (should (null result))))
    
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-text-to-buffer-integration ()
  "Test integration of center text sending functionality."
  (let ((test-buffer (generate-new-buffer "*integration-test*"))
        (sent-texts '())
        (process-obj nil))
    (with-current-buffer test-buffer
      (setq-local eat--process (start-process "integration-test" nil "echo" "test"))
      (setq process-obj eat--process)
      
      ;; Mock to capture all sent text
      (cl-letf (((symbol-function 'eat--send-string)
                 (lambda (proc text)
                   (push text sent-texts)))
                ((symbol-function 'process-live-p) 
                 (lambda (proc) t))
                ((symbol-function 'run-at-time)
                 (lambda (time repeat func &rest args)
                   (apply func args))))
        
        ;; Send multiple texts
        (enkan-repl--center-send-text-to-buffer "first" test-buffer)
        (enkan-repl--center-send-text-to-buffer "second" test-buffer)
        
        ;; Verify all texts were sent
        (should (member "first" sent-texts))
        (should (member "second" sent-texts))
        (should (member "\r" sent-texts))))
    
    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(provide 'center-unified-send-test)

;;; center-unified-send-test.el ends here
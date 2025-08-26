;;; simple-send-test.el --- Minimal test to reproduce multiline issue -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Minimal reproduction test for multiline sending issue

;;; Code:

(require 'ert)

;; Load the main package
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

(ert-deftest test-actual-multiline-sending ()
  "Test actual multiline text sending behavior that user is experiencing."
  (let ((test-buffer (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (multiline-text "line1\nline2\nline3")
        (sent-strings '())
        (call-count 0))
    
    ;; Mock eat--send-string to capture what actually gets sent
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (process string)
                 (setq call-count (1+ call-count))
                 (push string sent-strings)
                 ;; Simulate what eat might do with multiline strings
                 (message "eat--send-string received: %S (length: %d)" 
                         string (length string))
                 ;; Check if string contains newlines
                 (when (string-match-p "\n" string)
                   (message "WARNING: String contains newlines - this might cause issues in terminal"))))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'run-at-time) (lambda (&rest args) nil)))
      
      ;; Setup buffer
      (with-current-buffer test-buffer
        (setq-local eat--process (make-process :name "test" :command '("echo" "test"))))
      
      ;; Test the function
      (let ((result (enkan-repl--center-send-text-to-buffer multiline-text test-buffer)))
        (should result)
        (should (= 2 call-count))
        
        ;; Check what was actually sent
        (let ((first-call (nth 1 sent-strings))   ; LIFO order
              (second-call (nth 0 sent-strings)))
          (should (string= first-call multiline-text))
          (should (string= second-call "\r"))
          
          ;; This is the key check - does the first call contain all lines?
          (should (string-match-p "line1" first-call))
          (should (string-match-p "line2" first-call))  
          (should (string-match-p "line3" first-call))
          
          ;; Verify the structure
          (should (string= first-call "line1\nline2\nline3")))))
    
    ;; Clean up
    (kill-buffer test-buffer)))

(ert-deftest test-multiline-line-by-line-sending ()
  "Test if sending line by line would work better."
  (let ((test-buffer (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (multiline-text "line1\nline2\nline3")
        (sent-strings '())
        (call-count 0))
    
    ;; Mock eat--send-string
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (process string)
                 (setq call-count (1+ call-count))
                 (push string sent-strings)
                 (message "Line-by-line send %d: %S" call-count string)))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'run-at-time) (lambda (&rest args) nil)))
      
      ;; Setup buffer
      (with-current-buffer test-buffer
        (setq-local eat--process (make-process :name "test" :command '("echo" "test"))))
      
      ;; Send line by line
      (with-current-buffer test-buffer
        (dolist (line (split-string multiline-text "\n"))
          (when (not (string-empty-p line))
            (eat--send-string eat--process line)
            (eat--send-string eat--process "\r"))))
      
      ;; Verify line-by-line sending
      (should (= 6 call-count))  ; 3 lines + 3 returns
      (let ((all-calls (reverse sent-strings)))
        (should (string= (nth 0 all-calls) "line1"))
        (should (string= (nth 1 all-calls) "\r"))
        (should (string= (nth 2 all-calls) "line2"))
        (should (string= (nth 3 all-calls) "\r"))
        (should (string= (nth 4 all-calls) "line3"))
        (should (string= (nth 5 all-calls) "\r"))))
    
    ;; Clean up
    (kill-buffer test-buffer)))

(ert-deftest test-enkan-repl-send-region-multiline-fix ()
  "Test that enkan-repl-send-region works with multiline text after fix."
  (let ((test-buffer (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (multiline-text "line1\nline2\nline3")
        (sent-strings '())
        (call-count 0))
    
    ;; Mock eat--send-string to capture what gets sent
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (process string)
                 (setq call-count (1+ call-count))
                 (push string sent-strings)
                 (message "enkan-repl-send-region Call %d: %S (len: %d)" 
                         call-count string (length string))))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'run-at-time) (lambda (&rest args) nil))
              ((symbol-function 'use-region-p) (lambda () t)))
      
      ;; Setup buffer
      (with-current-buffer test-buffer
        (setq-local eat--process (make-process :name "test" :command '("echo" "test"))))
      
      ;; Test the function
      (with-temp-buffer
        (insert multiline-text)
        (let ((enkan-repl-session-list '((1 . "enkan-repl")))
              (default-directory "/Users/sekine/dev/self/enkan-repl/"))
          (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
                     (lambda (dir) test-buffer)))
            ;; Call without arg - should work after fix
            (enkan-repl-send-region (point-min) (point-max)))))
      
      ;; Verify multiline text was sent
      (should (> call-count 0))
      (should (member multiline-text sent-strings)))
    
    ;; Clean up
    (kill-buffer test-buffer)))

(provide 'simple-send-test)
;;; simple-send-test.el ends here
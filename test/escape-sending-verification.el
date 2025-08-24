;;; escape-sending-verification.el --- Verification test for actual ESC sending -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Direct verification that ESC characters are actually sent, not just ":esc" strings.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-actual-escape-character-sending ()
  "Verify that actual ESC character (\\e) is sent, not string \":esc\"."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (sent-strings '())
        (process-obj nil))
    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))
    
    ;; Mock eat--send-string to capture what's actually sent
    (cl-letf (((symbol-function 'buffer-list)
               (lambda () (list test-buffer)))
              ((symbol-function 'eat--send-string)
               (lambda (proc text)
                 (setq sent-strings (append sent-strings (list text)))))
              ((symbol-function 'process-live-p) 
               (lambda (proc) t)))
      
      ;; Test ESC sending through the new implementation
      (let ((result (enkan-repl--send-escape-to-buffer test-buffer)))
        (should (plist-get result :success))
        ;; Verify that actual ESC character (\e) was sent, not ":esc" string
        (should (member "\e" sent-strings))
        (should-not (member ":esc" sent-strings))))
    
    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(provide 'escape-sending-verification)

;;; escape-sending-verification.el ends here
;;; center-send-line-test.el --- Tests for center send line functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unified tests for center send line functionality.
;; Tests the core send line behavior with prefix arguments and selection UI.

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;;; Pure function tests for center send line behavior

(ert-deftest test-center-send-line-prefix-args ()
  "Test that prefix arguments 1-4 send to specific sessions."
  (let ((mock-calls '()))
    (cl-letf (((symbol-function 'enkan-repl--send-region-with-prefix)
               (lambda (start end session)
                 (push (list start end session) mock-calls))))
      ;; Test with prefix arg 1
      (let ((current-prefix-arg 1))
        (save-excursion
          (with-temp-buffer
            (insert "test line")
            (goto-char (point-min))
            (enkan-repl-center-send-line 1))))
      (should (equal (car mock-calls) '(1 10 1))))))

(ert-deftest test-center-send-line-no-arg-shows-selection ()
  "Test that no prefix argument shows buffer selection UI."
  (let ((selection-shown nil)
        (valid-buffers '()))
    (cl-letf (((symbol-function 'enkan-repl--collect-enkan-buffers-pure)
               (lambda (buffers) valid-buffers))
              ((symbol-function 'enkan-repl--send-buffer-content)
               (lambda (start end desc) (setq selection-shown t))))
      (with-temp-buffer
        (insert "test line")
        (goto-char (point-min))
        (enkan-repl-center-send-line))
      (should selection-shown))))

(ert-deftest test-center-send-line-invalid-prefix ()
  "Test that invalid prefix arguments show selection UI."
  (let ((selection-shown nil))
    (cl-letf (((symbol-function 'enkan-repl--collect-enkan-buffers-pure)
               (lambda (buffers) '()))
              ((symbol-function 'enkan-repl--send-buffer-content) 
               (lambda (start end desc) (setq selection-shown t))))
      (with-temp-buffer
        (insert "test line")
        (goto-char (point-min))
        (enkan-repl-center-send-line 99))
      (should selection-shown))))

;;;; ER prefix parsing tests

(ert-deftest test-center-send-line-er-prefix-detection ()
  "Test detection of ER prefix in line content."
  (should (string-match-p "^:er " ":er test"))
  (should (string-match-p "^:er " ":er alias :esc"))
  (should-not (string-match-p "^:er " "regular line"))
  (should-not (string-match-p "^:er " "test :er middle")))

(ert-deftest test-center-send-line-er-prefix-parsing ()
  "Test parsing of ER prefix commands."
  (let ((result (enkan-repl-center--parse-alias-command-pure ":er test")))
    (should (plist-get result :valid))
    (should (string= (plist-get result :alias) "test"))
    (should (eq (plist-get result :command) :send))))

(ert-deftest test-center-send-line-er-prefix-with-commands ()
  "Test ER prefix with ESC and RET commands."
  (let ((esc-result (enkan-repl-center--parse-alias-command-pure ":er test :esc"))
        (ret-result (enkan-repl-center--parse-alias-command-pure ":er test :ret")))
    (should (eq (plist-get esc-result :command) :esc))
    (should (eq (plist-get ret-result :command) :ret))))

;;;; Integration tests

(ert-deftest test-center-send-line-integration-prefix-priority ()
  "Test that numeric prefix takes priority over ER prefix."
  (let ((prefix-used nil)
        (er-parsed nil))
    (cl-letf (((symbol-function 'enkan-repl--send-region-with-prefix)
               (lambda (start end session) (setq prefix-used session)))
              ((symbol-function 'enkan-repl-center--parse-alias-command-pure)
               (lambda (text) (setq er-parsed t) '(:valid nil))))
      (with-temp-buffer
        (insert ":er test")
        (goto-char (point-min))
        (enkan-repl-center-send-line 2))
      (should (eq prefix-used 2))
      (should-not er-parsed))))

(provide 'center-send-line-test)

;;; center-send-line-test.el ends here
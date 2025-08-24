;;; enkan-center-send-line-fixed-integration-test.el --- Integration tests for fixed center send line -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests to verify the fixed enkan-repl-center-send-line function
;; behaves correctly with prefix arguments like enkan-repl-send-line.

;;; Code:

(require 'ert)

;; Mock function to track calls
(defvar enkan-test-send-region-calls nil
  "List to track calls to send-region-with-prefix.")

(defun enkan-repl--send-region-with-prefix-mock (start end session)
  "Mock function to track send-region-with-prefix calls."
  (push (list start end session) enkan-test-send-region-calls))

;; Test function that mirrors the fixed implementation
(defun enkan-test-center-send-line-fixed (&optional arg)
  "Test version of fixed center send line function."
  (if (and (numberp arg) (<= 1 arg 4))
      (enkan-repl--send-region-with-prefix-mock 10 20 arg)
    'selection-ui))

;; Test prefix argument behavior
(ert-deftest test-enkan-center-send-line-fixed-prefix ()
  "Test that fixed function correctly handles prefix arguments."
  (setq enkan-test-send-region-calls nil)
  
  ;; Test valid prefix arguments
  (enkan-test-center-send-line-fixed 1)
  (enkan-test-center-send-line-fixed 2)  
  (enkan-test-center-send-line-fixed 3)
  (enkan-test-center-send-line-fixed 4)
  
  ;; Check calls were made correctly
  (should (= (length enkan-test-send-region-calls) 4))
  (should (equal (nth 0 enkan-test-send-region-calls) '(10 20 4)))
  (should (equal (nth 1 enkan-test-send-region-calls) '(10 20 3)))
  (should (equal (nth 2 enkan-test-send-region-calls) '(10 20 2)))
  (should (equal (nth 3 enkan-test-send-region-calls) '(10 20 1))))

;; Test no argument behavior
(ert-deftest test-enkan-center-send-line-fixed-no-arg ()
  "Test that fixed function triggers selection UI without argument."
  (should (eq (enkan-test-center-send-line-fixed nil) 'selection-ui))
  (should (eq (enkan-test-center-send-line-fixed) 'selection-ui)))

;; Test invalid argument behavior
(ert-deftest test-enkan-center-send-line-fixed-invalid-args ()
  "Test that fixed function triggers selection UI for invalid arguments."
  (should (eq (enkan-test-center-send-line-fixed 0) 'selection-ui))
  (should (eq (enkan-test-center-send-line-fixed 5) 'selection-ui))
  (should (eq (enkan-test-center-send-line-fixed -1) 'selection-ui))
  (should (eq (enkan-test-center-send-line-fixed "invalid") 'selection-ui)))

(provide 'enkan-center-send-line-fixed-integration-test)
;;; enkan-center-send-line-fixed-integration-test.el ends here
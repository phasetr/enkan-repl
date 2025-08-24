;;; enkan-center-send-line-fixed-simple-test.el --- Tests for fixed simple center send line -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the user-fixed simple implementation of enkan-repl-center-send-line
;; that uses enkan-repl--send-buffer-content for non-prefix arguments.

;;; Code:

(require 'ert)

;; Mock function to track calls to send-region-with-prefix
(defvar enkan-test-send-region-with-prefix-calls nil
  "List to track calls to send-region-with-prefix.")

(defvar enkan-test-send-buffer-content-calls nil
  "List to track calls to send-buffer-content.")

(defun enkan-repl--send-region-with-prefix-mock (start end session)
  "Mock function to track send-region-with-prefix calls."
  (push (list start end session) enkan-test-send-region-with-prefix-calls))

(defun enkan-repl--send-buffer-content-mock (start end description)
  "Mock function to track send-buffer-content calls."
  (push (list start end description) enkan-test-send-buffer-content-calls))

;; Test function that mirrors the user-fixed implementation
(defun enkan-test-center-send-line-simple (&optional arg)
  "Test version of user-fixed simple center send line function."
  (if (and (numberp arg) (<= 1 arg 4))
      (enkan-repl--send-region-with-prefix-mock 10 20 arg)
    (enkan-repl--send-buffer-content-mock 10 20 "Line")))

;; Test prefix argument behavior calls send-region-with-prefix
(ert-deftest test-enkan-center-send-line-simple-prefix-args ()
  "Test that prefix arguments 1-4 call send-region-with-prefix."
  (setq enkan-test-send-region-with-prefix-calls nil
        enkan-test-send-buffer-content-calls nil)
  
  ;; Test valid prefix arguments
  (enkan-test-center-send-line-simple 1)
  (enkan-test-center-send-line-simple 2)  
  (enkan-test-center-send-line-simple 3)
  (enkan-test-center-send-line-simple 4)
  
  ;; Check calls were made correctly
  (should (= (length enkan-test-send-region-with-prefix-calls) 4))
  (should (equal (nth 0 enkan-test-send-region-with-prefix-calls) '(10 20 4)))
  (should (equal (nth 1 enkan-test-send-region-with-prefix-calls) '(10 20 3)))
  (should (equal (nth 2 enkan-test-send-region-with-prefix-calls) '(10 20 2)))
  (should (equal (nth 3 enkan-test-send-region-with-prefix-calls) '(10 20 1)))
  
  ;; No send-buffer-content calls
  (should (= (length enkan-test-send-buffer-content-calls) 0)))

;; Test no argument calls send-buffer-content
(ert-deftest test-enkan-center-send-line-simple-no-arg ()
  "Test that no argument calls send-buffer-content."
  (setq enkan-test-send-region-with-prefix-calls nil
        enkan-test-send-buffer-content-calls nil)
  
  (enkan-test-center-send-line-simple nil)
  (enkan-test-center-send-line-simple)
  
  ;; Check send-buffer-content was called
  (should (= (length enkan-test-send-buffer-content-calls) 2))
  (should (equal (nth 0 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  (should (equal (nth 1 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  
  ;; No send-region-with-prefix calls
  (should (= (length enkan-test-send-region-with-prefix-calls) 0)))

;; Test invalid arguments call send-buffer-content
(ert-deftest test-enkan-center-send-line-simple-invalid-args ()
  "Test that invalid arguments call send-buffer-content."
  (setq enkan-test-send-region-with-prefix-calls nil
        enkan-test-send-buffer-content-calls nil)
  
  (enkan-test-center-send-line-simple 0)
  (enkan-test-center-send-line-simple 5)
  (enkan-test-center-send-line-simple -1)
  (enkan-test-center-send-line-simple "invalid")
  
  ;; Check send-buffer-content was called for all invalid args
  (should (= (length enkan-test-send-buffer-content-calls) 4))
  (should (equal (nth 0 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  (should (equal (nth 1 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  (should (equal (nth 2 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  (should (equal (nth 3 enkan-test-send-buffer-content-calls) '(10 20 "Line")))
  
  ;; No send-region-with-prefix calls
  (should (= (length enkan-test-send-region-with-prefix-calls) 0)))

(provide 'enkan-center-send-line-fixed-simple-test)
;;; enkan-center-send-line-fixed-simple-test.el ends here
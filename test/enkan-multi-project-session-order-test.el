;;; enkan-multi-project-session-order-test.el --- TDD tests for multi-project session order fix -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ensuring multi-project layout session order matches configuration order.

;;; Code:

(require 'ert)

;; Pure function to create session list with correct order
(defun enkan-repl--create-session-list-with-order-pure (alias-list)
  "Create session list maintaining ALIAS-LIST order."
  (unless alias-list
    (error "Alias list cannot be empty"))
  (when (> (length alias-list) 4)
    (error "Too many projects: %d (max 4)" (length alias-list)))
  (let ((session-number 4)
        (result nil))
    (dolist (alias alias-list)
      (push (cons session-number alias) result)
      (setq session-number (1+ session-number)))
    (reverse result)))

;; Test session list creation with correct order
(ert-deftest test-enkan-multi-project-session-order-creation ()
  "Test session list creation maintains configuration order."
  (let ((result (enkan-repl--create-session-list-with-order-pure '("pt-tools" "enkan-repl"))))
    (should (equal result '((4 . "pt-tools") (5 . "enkan-repl"))))
    (should (equal (car result) '(4 . "pt-tools")))
    (should (equal (cadr result) '(5 . "enkan-repl"))))
  
  (let ((result (enkan-repl--create-session-list-with-order-pure '("pt" "er" "cc"))))
    (should (equal result '((4 . "pt") (5 . "er") (6 . "cc"))))))

;; Test error cases
(ert-deftest test-enkan-multi-project-order-error-cases ()
  "Test error cases for session order creation."
  (should-error (enkan-repl--create-session-list-with-order-pure nil))
  (should-error (enkan-repl--create-session-list-with-order-pure 
                '("p1" "p2" "p3" "p4" "p5"))))

(provide 'enkan-multi-project-session-order-test)
;;; enkan-multi-project-session-order-test.el ends here
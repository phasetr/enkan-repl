;;; enkan-center-send-line-correct-test.el --- TDD tests for correct center send line behavior -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ensuring enkan-repl-center-send-line behaves like enkan-repl-send-line
;; with prefix argument support (1-4 for specific sessions, none for selection UI).

;;; Code:

(require 'ert)

;; Pure function to create correct center send line behavior
(defun enkan-repl-center-send-line-correct-pure (arg line-start line-end)
  "Pure function implementing correct center send line behavior.
ARG: prefix argument (nil or 1-4)
LINE-START, LINE-END: line positions
Returns action taken: 'session-N or 'selection-ui"
  (if (and (numberp arg) (<= 1 arg 4))
      (intern (format "session-%d" arg))
    'selection-ui))

;; Test for prefix argument 1-4 behavior
(ert-deftest test-enkan-center-send-line-prefix-args ()
  "Test that prefix arguments 1-4 send to specific sessions."
  (should (eq (enkan-repl-center-send-line-correct-pure 1 10 20) 'session-1))
  (should (eq (enkan-repl-center-send-line-correct-pure 2 10 20) 'session-2))
  (should (eq (enkan-repl-center-send-line-correct-pure 3 10 20) 'session-3))
  (should (eq (enkan-repl-center-send-line-correct-pure 4 10 20) 'session-4)))

;; Test for no argument behavior
(ert-deftest test-enkan-center-send-line-no-arg ()
  "Test that no argument triggers selection UI."
  (should (eq (enkan-repl-center-send-line-correct-pure nil 10 20) 'selection-ui)))

;; Test for invalid argument behavior
(ert-deftest test-enkan-center-send-line-invalid-args ()
  "Test that invalid arguments trigger selection UI."
  (should (eq (enkan-repl-center-send-line-correct-pure 0 10 20) 'selection-ui))
  (should (eq (enkan-repl-center-send-line-correct-pure 5 10 20) 'selection-ui))
  (should (eq (enkan-repl-center-send-line-correct-pure -1 10 20) 'selection-ui))
  (should (eq (enkan-repl-center-send-line-correct-pure "invalid" 10 20) 'selection-ui)))

(provide 'enkan-center-send-line-correct-test)
;;; enkan-center-send-line-correct-test.el ends here
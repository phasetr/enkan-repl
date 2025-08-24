;;; enkan-session-layout-window-test.el --- TDD tests for session layout window splitting fixes -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ensuring session layout functions create correct window configurations.
;; Problems found:
;; - 3session-layout: Wrong split count (4 splits for 3 sessions)
;; - 4session-layout: Wrong initial split ratio (0.5 instead of 0.6)

;;; Code:

(require 'ert)

;; Pure function to calculate correct window split ratios
(defun enkan-repl--calculate-session-layout-ratios-pure (session-count)
  "Calculate correct window split ratios for SESSION-COUNT.
Returns plist with :left-ratio and :expected-windows."
  (cond
   ((= session-count 2)
    (list :left-ratio 0.6 :expected-windows 5 :right-splits 1))
   ((= session-count 3)
    (list :left-ratio 0.6 :expected-windows 6 :right-splits 2))
   ((= session-count 4)
    (list :left-ratio 0.6 :expected-windows 7 :right-splits 3))
   (t (error "Invalid session count: %d" session-count))))

;; Pure function to validate window split logic
(defun enkan-repl--validate-window-split-logic-pure (session-count splits-performed)
  "Validate window split logic for SESSION-COUNT and SPLITS-PERFORMED.
Returns plist with :valid and :message."
  (let ((expected (enkan-repl--calculate-session-layout-ratios-pure session-count)))
    (let ((expected-splits (plist-get expected :right-splits)))
      (if (= splits-performed expected-splits)
          (list :valid t :message (format "Correct: %d splits for %d sessions" 
                                         splits-performed session-count))
        (list :valid nil :message (format "Error: %d splits performed but %d expected for %d sessions"
                                         splits-performed expected-splits session-count))))))

;; Test window split ratio calculations
(ert-deftest test-enkan-session-layout-ratio-calculations ()
  "Test correct ratio calculations for different session counts."
  ;; 2 sessions
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 2)))
    (should (= (plist-get result :left-ratio) 0.6))
    (should (= (plist-get result :expected-windows) 5))
    (should (= (plist-get result :right-splits) 1)))
  
  ;; 3 sessions
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 3)))
    (should (= (plist-get result :left-ratio) 0.6))
    (should (= (plist-get result :expected-windows) 6))
    (should (= (plist-get result :right-splits) 2)))
  
  ;; 4 sessions
  (let ((result (enkan-repl--calculate-session-layout-ratios-pure 4)))
    (should (= (plist-get result :left-ratio) 0.6))
    (should (= (plist-get result :expected-windows) 7))
    (should (= (plist-get result :right-splits) 3))))

;; Test window split validation logic
(ert-deftest test-enkan-session-layout-split-validation ()
  "Test validation of window split logic."
  ;; Correct splits
  (let ((result (enkan-repl--validate-window-split-logic-pure 2 1)))
    (should (plist-get result :valid)))
  (let ((result (enkan-repl--validate-window-split-logic-pure 3 2)))
    (should (plist-get result :valid)))
  (let ((result (enkan-repl--validate-window-split-logic-pure 4 3)))
    (should (plist-get result :valid)))
  
  ;; Incorrect splits (current bugs)
  (let ((result (enkan-repl--validate-window-split-logic-pure 3 3)))
    (should-not (plist-get result :valid))
    (should (string-match-p "Error.*3 splits performed but 2 expected" 
                           (plist-get result :message))))
  (let ((result (enkan-repl--validate-window-split-logic-pure 4 4)))
    (should-not (plist-get result :valid))
    (should (string-match-p "Error.*4 splits performed but 3 expected" 
                           (plist-get result :message)))))

;; Test error cases
(ert-deftest test-enkan-session-layout-error-cases ()
  "Test error cases for session layout calculations."
  (should-error (enkan-repl--calculate-session-layout-ratios-pure 1))
  (should-error (enkan-repl--calculate-session-layout-ratios-pure 5))
  (should-error (enkan-repl--calculate-session-layout-ratios-pure 0)))

(provide 'enkan-session-layout-window-test)
;;; enkan-session-layout-window-test.el ends here
;;; enkan-window-split-sequence-test.el --- TDD tests for correct window split sequences -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for correct window splitting sequences based on working 2session-layout.
;; Problems: 3session and 4session layouts have incorrect split sequences.

;;; Code:

(require 'ert)

;; Pure function to generate correct window split sequence
(defun enkan-repl--generate-window-split-sequence-pure (session-count)
  "Generate correct window split sequence for SESSION-COUNT.
Returns list of split operations based on working 2session-layout pattern."
  (cond
   ((= session-count 2)
    '(;; Step 1: Split main window - left 60%, right 40%
      (:action split-window-right :ratio 0.6)
      ;; Step 2: Move to right side and split into 2 columns
      (:action other-window :count 1)
      (:action split-window-right :ratio nil)
      ;; Step 3: Move back to left side and split bottom
      (:action other-window :count 2)  ; Move 2 windows back to left
      (:action split-window-below :ratio 0.65)
      ;; Step 4: Split bottom-left into work/reserve
      (:action other-window :count 1)
      (:action split-window-right :ratio 0.5)
      ;; Step 5: Balance and set to rightmost window
      (:action other-window :count 4)
      (:action balance-windows)))
   ((= session-count 3)
    '(;; Step 1: Split main window - left 60%, right 40% 
      (:action split-window-right :ratio 0.6)
      ;; Step 2: Move to right and create 3 columns
      (:action other-window :count 1)
      (:action split-window-right :ratio nil)
      (:action other-window :count 1)
      (:action split-window-right :ratio nil)
      ;; Step 3: Move back to left (3 windows back) and split bottom
      (:action other-window :count 3)
      (:action split-window-below :ratio 0.6)
      ;; Step 4: Split bottom-left into work/reserve  
      (:action other-window :count 1)
      (:action split-window-right :ratio 0.5)
      ;; Step 5: Balance
      (:action balance-windows)))
   ((= session-count 4)
    '(;; Step 1: Split main window - left 60%, right 40%
      (:action split-window-right :ratio 0.6)
      ;; Step 2: Move to right and create 4 columns
      (:action other-window :count 1)
      (:action split-window-right :ratio nil)
      (:action other-window :count 1) 
      (:action split-window-right :ratio nil)
      (:action other-window :count 1)
      (:action split-window-right :ratio nil)
      ;; Step 3: Move back to left (4 windows back) and split bottom
      (:action other-window :count 4)
      (:action split-window-below :ratio 0.6)
      ;; Step 4: Split bottom-left into work/reserve
      (:action other-window :count 1)
      (:action split-window-right :ratio 0.5)
      ;; Step 5: Balance
      (:action balance-windows)))
   (t (error "Invalid session count: %d" session-count))))

;; Pure function to validate split sequence
(defun enkan-repl--validate-split-sequence-pure (session-count sequence)
  "Validate SEQUENCE for SESSION-COUNT matches expected pattern.
Returns plist with :valid and :message."
  (let ((expected (enkan-repl--generate-window-split-sequence-pure session-count)))
    (if (equal sequence expected)
        (list :valid t :message (format "Correct sequence for %d sessions" session-count))
      (list :valid nil :message 
            (format "Incorrect sequence for %d sessions. Expected %d steps, got %d" 
                   session-count (length expected) (length sequence))))))

;; Test 2session sequence (reference)
(ert-deftest test-enkan-window-split-2session-sequence ()
  "Test 2session split sequence matches working implementation."
  (let ((sequence (enkan-repl--generate-window-split-sequence-pure 2)))
    ;; Should have correct number of operations
    (should (= (length sequence) 9))
    ;; First operation should be main split
    (should (eq (plist-get (nth 0 sequence) :action) 'split-window-right))
    (should (= (plist-get (nth 0 sequence) :ratio) 0.6))
    ;; Should include balance-windows
    (should (eq (plist-get (nth 8 sequence) :action) 'balance-windows))))

;; Test 3session sequence
(ert-deftest test-enkan-window-split-3session-sequence ()
  "Test 3session split sequence is correctly generated."
  (let ((sequence (enkan-repl--generate-window-split-sequence-pure 3)))
    ;; Should have correct main split
    (should (eq (plist-get (nth 0 sequence) :action) 'split-window-right))
    (should (= (plist-get (nth 0 sequence) :ratio) 0.6))
    ;; Should have 3 right-side splits total (2 for columns + 1 for work/reserve)
    (let ((right-splits (seq-count (lambda (op) 
                                    (eq (plist-get op :action) 'split-window-right))
                                  sequence)))
      (should (= right-splits 3)))  ; 3 total right splits
    ;; Should move back 3 windows to left side
    (should (= (plist-get (nth 5 sequence) :count) 3))))

;; Test 4session sequence
(ert-deftest test-enkan-window-split-4session-sequence ()
  "Test 4session split sequence is correctly generated."
  (let ((sequence (enkan-repl--generate-window-split-sequence-pure 4)))
    ;; Should have correct main split
    (should (eq (plist-get (nth 0 sequence) :action) 'split-window-right))
    (should (= (plist-get (nth 0 sequence) :ratio) 0.6))
    ;; Should have 4 right-side splits total (3 for columns + 1 for work/reserve)
    (let ((right-splits (seq-count (lambda (op)
                                    (eq (plist-get op :action) 'split-window-right))
                                  sequence)))
      (should (= right-splits 4)))  ; 4 total right splits  
    ;; Should move back 4 windows to left side
    (should (= (plist-get (nth 7 sequence) :count) 4))))

;; Test error cases
(ert-deftest test-enkan-window-split-sequence-errors ()
  "Test error cases for window split sequence generation."
  (should-error (enkan-repl--generate-window-split-sequence-pure 1))
  (should-error (enkan-repl--generate-window-split-sequence-pure 5)))

(provide 'enkan-window-split-sequence-test)
;;; enkan-window-split-sequence-test.el ends here
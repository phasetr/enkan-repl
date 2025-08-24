;;; enkan-window-order-debug-test.el --- TDD tests for debugging window order issues -*- lexical-binding: t -*-

;;; Commentary:
;; Tests to debug window order problems in 3session-layout.
;; Problem: Center file appearing in wrong position (3rd instead of 1st).

;;; Code:

(require 'ert)

;; Pure function to simulate window split sequence and predict order
(defun enkan-repl--predict-window-order-after-splits-pure (split-sequence)
  "Predict window order after executing SPLIT-SEQUENCE.
Returns list of window positions from left to right."
  ;; Start with single window (position 0)
  (let ((windows '(0))
        (current-pos 0))
    (dolist (split split-sequence)
      (cond
       ((eq split 'split-window-right)
        ;; When splitting right, new window goes to the right of current
        ;; Current window stays, new window added after it
        (let ((new-pos (1+ current-pos)))
          (setq windows (append (cl-subseq windows 0 (1+ current-pos))
                               (list new-pos)
                               (cl-subseq windows (1+ current-pos))))
          ;; Update positions of windows after the split point
          (setq windows (cl-loop for i from 0 below (length windows)
                                collect (if (> (nth i windows) current-pos)
                                           (1+ (nth i windows))
                                         (nth i windows))))))
       ((and (consp split) (eq (car split) 'other-window))
        ;; Move current position
        (setq current-pos (mod (+ current-pos (cdr split)) (length windows))))))
    windows))

;; Test current 3session split sequence
(ert-deftest test-enkan-3session-current-window-order ()
  "Test current 3session window split sequence and resulting order."
  (let ((sequence '(split-window-right        ; Split main -> [0, 1]
                   (other-window . 1)         ; Move to position 1
                   split-window-right         ; Split at pos 1 -> [0, 1, 2]
                   (other-window . 1)         ; Move to position 2  
                   split-window-right)))      ; Split at pos 2 -> [0, 1, 2, 3]
    (let ((result (enkan-repl--predict-window-order-after-splits-pure sequence)))
      ;; Should result in 4 windows
      (should (= (length result) 4))
      ;; Position 0 should be leftmost (center file)
      (should (= (nth 0 result) 0)))))

;; Pure function to determine correct window assignment for 3session layout
(defun enkan-repl--correct-3session-window-assignment-pure ()
  "Determine correct window assignment for 3session layout.
Returns plist with correct window->buffer mapping."
  (list :window-1 'center-file    ; Leftmost window
        :window-2 'session-4      ; Second window
        :window-3 'session-5      ; Third window  
        :window-4 'session-6))    ; Fourth window

;; Test correct window assignment
(ert-deftest test-enkan-3session-correct-assignment ()
  "Test correct window assignment for 3session layout."
  (let ((assignment (enkan-repl--correct-3session-window-assignment-pure)))
    (should (eq (plist-get assignment :window-1) 'center-file))
    (should (eq (plist-get assignment :window-2) 'session-4))
    (should (eq (plist-get assignment :window-3) 'session-5))
    (should (eq (plist-get assignment :window-4) 'session-6))))

;; Pure function to validate window-list index mapping
(defun enkan-repl--validate-window-list-mapping-pure (window-assignments)
  "Validate WINDOW-ASSIGNMENTS match expected layout.
Returns plist with :valid and :message."
  (let ((expected (enkan-repl--correct-3session-window-assignment-pure)))
    (if (and (eq (plist-get window-assignments :window-1) 
                 (plist-get expected :window-1))
             (eq (plist-get window-assignments :window-2) 
                 (plist-get expected :window-2))
             (eq (plist-get window-assignments :window-3) 
                 (plist-get expected :window-3))
             (eq (plist-get window-assignments :window-4) 
                 (plist-get expected :window-4)))
        (list :valid t :message "Window assignments are correct")
      (list :valid nil :message "Window assignments do not match expected layout"))))

(provide 'enkan-window-order-debug-test)
;;; enkan-window-order-debug-test.el ends here
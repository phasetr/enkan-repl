;;; hmenu-test.el --- Tests for hmenu.el -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for horizontal menu selection interface

;;; Code:

(require 'ert)
(require 'hmenu)
(require 'cl-lib)

;; Mock functions for testing
(defvar hmenu-test--last-key nil
  "Last key read during test.")

(defvar hmenu-test--key-sequence nil
  "Sequence of keys to simulate.")

(defvar hmenu-test--key-index 0
  "Current index in key sequence.")

(defun hmenu-test--mock-read-key ()
  "Mock read-key function for testing."
  (when (< hmenu-test--key-index (length hmenu-test--key-sequence))
    (let ((key (nth hmenu-test--key-index hmenu-test--key-sequence)))
      (setq hmenu-test--key-index (1+ hmenu-test--key-index))
      key)))

(defmacro with-mocked-input (keys &rest body)
  "Execute BODY with mocked input KEYS."
  `(let ((hmenu-test--key-sequence ,keys)
         (hmenu-test--key-index 0))
     (cl-letf (((symbol-function 'read-key) #'hmenu-test--mock-read-key)
               ((symbol-function 'message) (lambda (&rest _args) nil)))
       ,@body)))

;; Tests for hmenu--format-choices
(ert-deftest test-hmenu--format-choices-basic ()
  "Test basic choice formatting."
  (let ((choices '("First" "Second" "Third")))
    (let ((formatted (hmenu--format-choices choices 0)))
      (should (string-match-p "\\[1\\] First" formatted))
      (should (string-match-p "\\[2\\] Second" formatted))
      (should (string-match-p "\\[3\\] Third" formatted))
      (should (string-match-p " | " formatted)))))

(ert-deftest test-hmenu--format-choices-selection ()
  "Test formatting with selected item."
  (let ((choices '("First" "Second" "Third")))
    ;; First item selected
    (let ((formatted (hmenu--format-choices choices 0)))
      (should (get-text-property 0 'face formatted)))
    
    ;; Second item selected
    (let ((formatted (hmenu--format-choices choices 1)))
      (should (string-match-p "\\[2\\] Second" formatted)))))

(ert-deftest test-hmenu--format-choices-many-items ()
  "Test formatting with more than 9 items."
  (let ((choices (cl-loop for i from 1 to 12 collect (format "Item%d" i))))
    (let ((formatted (hmenu--format-choices choices 0)))
      ;; First 9 should have numbers
      (should (string-match-p "\\[1\\]" formatted))
      (should (string-match-p "\\[9\\]" formatted))
      ;; Items 10+ should have spaces instead
      (should (string-match-p "    Item10" formatted))
      (should (string-match-p "    Item11" formatted)))))

;; Tests for hmenu--get-choice-value
(ert-deftest test-hmenu--get-choice-value-strings ()
  "Test getting value from string list."
  (let ((choices '("First" "Second" "Third")))
    (should (string= (hmenu--get-choice-value choices 0) "First"))
    (should (string= (hmenu--get-choice-value choices 1) "Second"))
    (should (string= (hmenu--get-choice-value choices 2) "Third"))))

(ert-deftest test-hmenu--get-choice-value-alist ()
  "Test getting value from alist."
  (let ((choices '(("Display1" . value1)
                   ("Display2" . value2)
                   ("Display3" . value3))))
    (should (string= (hmenu--get-choice-value choices 0) "Display1"))
    (should (string= (hmenu--get-choice-value choices 1) "Display2"))
    (should (string= (hmenu--get-choice-value choices 2) "Display3"))))

;; Tests for hmenu main function
(ert-deftest test-hmenu-select-with-enter ()
  "Test selection with Enter key."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(?\r)  ; Enter key
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "First"))))))

(ert-deftest test-hmenu-select-with-space ()
  "Test selection with Space key."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(?\s)  ; Space key
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "First"))))))

(ert-deftest test-hmenu-navigate-right ()
  "Test navigation with right arrow."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(right ?\r)  ; Right then Enter
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Second"))))))

(ert-deftest test-hmenu-navigate-left ()
  "Test navigation with left arrow."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(right right left ?\r)  ; Right, Right, Left, Enter
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Second"))))))

(ert-deftest test-hmenu-navigate-with-hl-keys ()
  "Test navigation with h/l keys."
  (let ((choices '("First" "Second" "Third")))
    ;; Test 'l' for right
    (with-mocked-input '(108 ?\r)  ; l then Enter
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Second"))))
    
    ;; Test 'h' for left
    (with-mocked-input '(108 108 104 ?\r)  ; l, l, h, Enter
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Second"))))))

(ert-deftest test-hmenu-wrap-around ()
  "Test wrap-around navigation."
  (let ((choices '("First" "Second" "Third")))
    ;; Wrap from end to beginning
    (with-mocked-input '(left ?\r)  ; Left from first item
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Third"))))
    
    ;; Wrap from beginning to end
    (with-mocked-input '(right right right ?\r)  ; Right three times from first
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "First"))))))

(ert-deftest test-hmenu-cancel-with-escape ()
  "Test cancellation with ESC key."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(27)  ; ESC
      (let ((result (hmenu "Select:" choices)))
        (should (null result))))))

(ert-deftest test-hmenu-cancel-with-q ()
  "Test cancellation with q key."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(113)  ; q
      (let ((result (hmenu "Select:" choices)))
        (should (null result))))))

(ert-deftest test-hmenu-direct-number-selection ()
  "Test direct selection with number keys."
  (let ((choices '("First" "Second" "Third" "Fourth" "Fifth")))
    ;; Select first item with '1'
    (with-mocked-input '(?1)
      (should (string= (hmenu "Select:" choices) "First")))
    
    ;; Select third item with '3'
    (with-mocked-input '(?3)
      (should (string= (hmenu "Select:" choices) "Third")))
    
    ;; Select fifth item with '5'
    (with-mocked-input '(?5)
      (should (string= (hmenu "Select:" choices) "Fifth")))))

(ert-deftest test-hmenu-number-selection-limit ()
  "Test that number selection only works for first 9 items."
  (let ((choices (cl-loop for i from 1 to 12 collect (format "Item%d" i))))
    ;; '9' should select the 9th item
    (with-mocked-input '(?9)
      (should (string= (hmenu "Select:" choices) "Item9")))
    
    ;; '0' should not select anything (invalid)
    (with-mocked-input '(?0 ?\r)  ; 0 then Enter
      (should (string= (hmenu "Select:" choices) "Item1")))))

(ert-deftest test-hmenu-with-alist ()
  "Test hmenu with alist choices."
  (let ((choices '(("Display One" . :value1)
                   ("Display Two" . :value2)
                   ("Display Three" . :value3))))
    (with-mocked-input '(right ?\r)  ; Select second item
      (let ((result (hmenu "Select:" choices)))
        (should (string= result "Display Two"))))))

(ert-deftest test-hmenu-empty-choices-error ()
  "Test that empty choices list causes error."
  (should-error (hmenu "Select:" nil))
  (should-error (hmenu "Select:" '())))

;; Tests for hmenu-read-string
(ert-deftest test-hmenu-read-string-with-strings ()
  "Test hmenu-read-string with string list."
  (let ((choices '("First" "Second" "Third")))
    (with-mocked-input '(right ?\r)
      (let ((result (hmenu-read-string "Select:" choices)))
        (should (string= result "Second"))))))

(ert-deftest test-hmenu-read-string-with-alist ()
  "Test hmenu-read-string with alist."
  (let ((choices '(("Display One" . :value1)
                   ("Display Two" . :value2)
                   ("Display Three" . :value3))))
    (with-mocked-input '(right ?\r)
      (let ((result (hmenu-read-string "Select:" choices)))
        ;; Should return display string, not value
        (should (string= result "Display Two"))))))

;; Face customization tests
(ert-deftest test-hmenu-faces-defined ()
  "Test that all hmenu faces are defined."
  (should (facep 'hmenu-prompt-face))
  (should (facep 'hmenu-choice-face))
  (should (facep 'hmenu-selected-face))
  (should (facep 'hmenu-separator-face)))

;; Variable tests
(ert-deftest test-hmenu-separator-variable ()
  "Test hmenu-separator variable."
  (should (stringp hmenu-separator))
  (should (string= hmenu-separator " | ")))

(provide 'hmenu-test)
;;; hmenu-test.el ends here
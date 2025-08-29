;;; hmenu.el --- Horizontal menu selection interface -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, interface
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a clean horizontal selection interface for multiple choices
;; with keyboard navigation support.

;;; Code:

(require 'cl-lib)

(defface hmenu-prompt-face
  '((t (:inherit minibuffer-prompt)))
  "Face for hmenu prompt text."
  :group 'hmenu)

(defface hmenu-choice-face
  '((t (:inherit default)))
  "Face for unselected hmenu choices."
  :group 'hmenu)

(defface hmenu-selected-face
  '((t (:inherit highlight :weight bold)))
  "Face for currently selected hmenu choice."
  :group 'hmenu)

(defface hmenu-separator-face
  '((t (:inherit shadow)))
  "Face for choice separators."
  :group 'hmenu)

(defvar hmenu-separator " | "
  "String used to separate menu choices.")

(defun hmenu--format-choices (choices selected-index)
  "Format CHOICES list with SELECTED-INDEX highlighted.
Returns formatted string for display."
  (let ((formatted-choices
          (cl-loop for choice in choices
            for index from 0
            collect (let* ((number-prefix (if (< index 9)
                                            (format "[%d] " (1+ index))
                                            "    "))
                          (full-choice (concat number-prefix choice)))
                      (if (= index selected-index)
                        (propertize full-choice 'face 'hmenu-selected-face)
                        (propertize full-choice 'face 'hmenu-choice-face))))))
    (mapconcat 'identity formatted-choices
      (propertize hmenu-separator 'face 'hmenu-separator-face))))

(defun hmenu--get-choice-value (choices selected-index)
  "Get the value for SELECTED-INDEX from CHOICES.
CHOICES can be list of strings or alist of (display . value) pairs."
  (let ((choice (nth selected-index choices)))
    (if (consp choice)
      (cdr choice)  ; alist case: return value
      choice)))       ; string list case: return string itself

(defun hmenu (prompt choices)
  "Display horizontal menu with PROMPT and CHOICES, return selected value.
CHOICES can be:
- List of strings: ('choice1' 'choice2' 'choice3')
- Alist of (display . value) pairs: (('Display1' . value1) ('Display2' . value2))

Navigation:
- Left/Right arrows or h/l: move selection
- Enter/Space/RET: confirm selection
- C-g/ESC/q: cancel selection
- Number keys (1-9): direct selection

Returns selected value or nil if cancelled."
  (when (null choices)
    (error "No choices provided to hmenu"))
  (let* ((choice-displays (if (consp (car choices))
                            (mapcar #'car choices)  ; alist case
                            choices))                 ; string list case
          (selected-index 0)
          (max-index (1- (length choices)))
          (continue t)
          (result nil))
    ;; Limit direct selection to first 9 items
    (let ((numbered-choices (cl-subseq choice-displays 0 (min 9 (length choice-displays)))))
      (message "Direct selection: %s"
        (mapconcat (lambda (choice)
                     (format "%d:%s"
                       (1+ (cl-position choice numbered-choices))
                       choice))
          numbered-choices " ")))
    (while continue
      (let* ((formatted (hmenu--format-choices choice-displays selected-index))
              (prompt-text (propertize (format "%s " prompt) 'face 'hmenu-prompt-face))
              (full-message (concat prompt-text formatted)))
        (message full-message)
        (let ((key (read-key)))
          (cond
            ;; Navigation keys
            ((or (eq key 'right) (eq key 6) (eq key 108))  ; right, C-f(6), l(108)
              (setq selected-index (if (>= selected-index max-index) 0 (1+ selected-index))))
            ((or (eq key 'left) (eq key 2) (eq key 104))   ; left, C-b(2), h(104)
              (setq selected-index (if (<= selected-index 0) max-index (1- selected-index))))
            ;; Confirm selection
            ((or (eq key 13) (eq key 10) (eq key 32))  ; Enter(13), C-j(10), Space(32)
              (setq result (hmenu--get-choice-value choices selected-index))
              (setq continue nil))
            ;; Cancel selection
            ((or (eq key 7) (eq key 27) (eq key 113))  ; C-g(7), ESC(27), q(113)
              (setq result nil)
              (setq continue nil))
           ;; Direct number selection (1-9)
            ((and (integerp key) (>= key ?1) (<= key ?9))
              (let ((num (- key ?0)))
                (when (<= num (length choices))
                  (setq selected-index (1- num))
                  (setq result (hmenu--get-choice-value choices selected-index))
                  (setq continue nil))))
            ;; Invalid key
            (t
              (ding)
              (message "Invalid key. Use arrows/h/l to navigate, Enter to select, C-g to cancel")
              (sit-for 1))))))
    (message "")  ; Clear message line
    result))

(defun hmenu-read-string (prompt choices)
  "Like `hmenu' but ensures return value is a string.
Useful when CHOICES contains alist but you want the display string."
  (let ((choice-displays (if (consp (car choices))
                           (mapcar #'car choices)
                           choices)))
    (nth (cl-position (hmenu prompt choices) choice-displays :test #'equal)
      choice-displays)))

(provide 'hmenu)

;;; hmenu.el ends here

;;; enkan-center-file-keybinding-fix-test.el --- Tests for center file keybinding fix -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ensuring C-M-i is correctly bound to enkan-repl-center-send-line
;; in center file mode using TDD and pure functions.

;;; Code:

(require 'ert)

;; Pure function to create center file mode keymap
(defun enkan-center-file-create-keymap-pure ()
  "Create center file mode keymap with proper bindings.
Pure function - no side effects."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") 'enkan-repl-center-send-region)
    (define-key map (kbd "C-M-e") 'enkan-repl-center-send-enter)
    (define-key map (kbd "C-M-i") 'enkan-repl-center-send-line)
    (define-key map (kbd "C-M-t") 'enkan-repl-center-other-window)
    (define-key map (kbd "C-M-b") 'enkan-repl-center-recenter-bottom)
    map))

;; Pure function to check keymap binding
(defun enkan-center-file-check-binding-pure (keymap key expected-command)
  "Check if KEY in KEYMAP is bound to EXPECTED-COMMAND.
Pure function - no side effects."
  (let ((actual-command (lookup-key keymap (kbd key))))
    (eq actual-command expected-command)))

;; Test for keymap creation
(ert-deftest test-enkan-center-file-keymap-creation ()
  "Test that center file keymap is created correctly."
  (let ((keymap (enkan-center-file-create-keymap-pure)))
    (should (keymapp keymap))
    (should (enkan-center-file-check-binding-pure 
             keymap "C-M-i" 'enkan-repl-center-send-line))))

;; Test for C-M-i binding specifically
(ert-deftest test-enkan-center-file-c-m-i-binding ()
  "Test that C-M-i is bound to enkan-repl-center-send-line."
  (let ((keymap (enkan-center-file-create-keymap-pure)))
    (should (eq (lookup-key keymap (kbd "C-M-i")) 
                'enkan-repl-center-send-line))))

;; Test for all expected bindings
(ert-deftest test-enkan-center-file-all-bindings ()
  "Test all expected center file mode bindings."
  (let ((keymap (enkan-center-file-create-keymap-pure)))
    (should (eq (lookup-key keymap (kbd "M-<return>")) 
                'enkan-repl-center-send-region))
    (should (eq (lookup-key keymap (kbd "C-M-e")) 
                'enkan-repl-center-send-enter))
    (should (eq (lookup-key keymap (kbd "C-M-i")) 
                'enkan-repl-center-send-line))
    (should (eq (lookup-key keymap (kbd "C-M-t")) 
                'enkan-repl-center-other-window))
    (should (eq (lookup-key keymap (kbd "C-M-b")) 
                'enkan-repl-center-recenter-bottom))))

(provide 'enkan-center-file-keybinding-fix-test)
;;; enkan-center-file-keybinding-fix-test.el ends here
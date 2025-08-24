;;; center-keybinding-override-test.el --- Tests for center file keybinding override -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for center file keybinding override functionality.
;; These tests verify that center file keybindings properly override base keybindings.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-center-file-global-mode-keymap-definition ()
  "Test that center file global mode keymap is properly defined."
  (should (keymapp enkan-center-file-global-mode-map))
  ;; Test that C-M-1 through C-M-5 are bound to center functions
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-1")) 'enkan-repl-center-send-1))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-2")) 'enkan-repl-center-send-2))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-3")) 'enkan-repl-center-send-3))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-4")) 'enkan-repl-center-send-4))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-5")) 'enkan-repl-center-send-5)))

(ert-deftest test-center-file-global-mode-toggle ()
  "Test that center file global mode can be toggled on/off."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Ensure mode is off
          (when enkan-center-file-global-mode
            (enkan-center-file-global-mode -1))
          (should-not enkan-center-file-global-mode)
          
          ;; Turn mode on
          (enkan-center-file-global-mode 1)
          (should enkan-center-file-global-mode)
          
          ;; Turn mode off
          (enkan-center-file-global-mode -1)
          (should-not enkan-center-file-global-mode))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-file-keybinding-override-behavior ()
  "Test that center file mode properly overrides base keybindings."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (with-temp-buffer
          ;; Turn on center file global mode
          (enkan-center-file-global-mode 1)
          (should enkan-center-file-global-mode)
          
          ;; In this buffer, C-M-1 should be bound to center function
          (let ((binding (key-binding (kbd "C-M-1"))))
            (should (eq binding 'enkan-repl-center-send-1)))
          
          ;; Test other keybindings
          (should (eq (key-binding (kbd "C-M-2")) 'enkan-repl-center-send-2))
          (should (eq (key-binding (kbd "C-M-3")) 'enkan-repl-center-send-3))
          (should (eq (key-binding (kbd "C-M-4")) 'enkan-repl-center-send-4))
          (should (eq (key-binding (kbd "C-M-5")) 'enkan-repl-center-send-5)))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-file-mode-disabled-behavior ()
  "Test behavior when center file mode is disabled."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (with-temp-buffer
          ;; Ensure center file mode is off
          (enkan-center-file-global-mode -1)
          (should-not enkan-center-file-global-mode)
          
          ;; C-M-1 through C-M-5 should not be bound to center functions
          ;; They may be unbound or bound to other functions, but not center functions
          (let ((binding-1 (key-binding (kbd "C-M-1")))
                (binding-2 (key-binding (kbd "C-M-2")))
                (binding-3 (key-binding (kbd "C-M-3")))
                (binding-4 (key-binding (kbd "C-M-4")))
                (binding-5 (key-binding (kbd "C-M-5"))))
            (should-not (eq binding-1 'enkan-repl-center-send-1))
            (should-not (eq binding-2 'enkan-repl-center-send-2))
            (should-not (eq binding-3 'enkan-repl-center-send-3))
            (should-not (eq binding-4 'enkan-repl-center-send-4))
            (should-not (eq binding-5 'enkan-repl-center-send-5))))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-file-escape-keybinding-override ()
  "Test that escape key is properly overridden in center file mode."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (with-temp-buffer
          ;; Turn on center file global mode
          (enkan-center-file-global-mode 1)
          
          ;; Escape key should be bound to center escape function
          (should (eq (key-binding (kbd "<escape>")) 'enkan-repl-center-send-escape)))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(provide 'center-keybinding-override-test)

;;; center-keybinding-override-test.el ends here
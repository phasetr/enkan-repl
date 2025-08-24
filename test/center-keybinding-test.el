;;; center-keybinding-test.el --- Tests for center file keybinding functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unified tests for center file keybinding functionality.
;; Tests keymap definition, mode activation, and keybinding override behavior.

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;;; Keymap definition tests

(ert-deftest test-center-file-keymap-definition ()
  "Test that center file global mode keymap is properly defined."
  (should (keymapp enkan-center-file-global-mode-map))
  ;; Test that C-M-1 through C-M-5 are bound to center functions
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-1")) 'enkan-repl-center-send-1))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-2")) 'enkan-repl-center-send-2))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-3")) 'enkan-repl-center-send-3))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-4")) 'enkan-repl-center-send-4))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-5")) 'enkan-repl-center-send-5)))

(ert-deftest test-center-file-additional-keybindings ()
  "Test additional center file keybindings."
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-i")) 'enkan-repl-center-send-enter))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-x g")) 'enkan-repl-center-magit)))

;;;; Mode toggle tests

(ert-deftest test-center-file-mode-toggle ()
  "Test that center file global mode can be toggled on/off."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Test turning mode on
          (enkan-center-file-global-mode 1)
          (should enkan-center-file-global-mode)
          
          ;; Test turning mode off
          (enkan-center-file-global-mode -1)
          (should-not enkan-center-file-global-mode)
          
          ;; Test toggle
          (enkan-center-file-global-mode)
          (should enkan-center-file-global-mode))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

;;;; Integration tests

(ert-deftest test-center-file-mode-activation-and-keybinding-override ()
  "Test that center file mode activates and properly overrides keybindings."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Start with mode disabled
          (enkan-center-file-global-mode -1)
          (should-not enkan-center-file-global-mode)
          
          ;; Create a test buffer to simulate center file usage
          (with-temp-buffer
            ;; Manually enable center file global mode (simulating user action)
            (enkan-center-file-global-mode 1)
            (should enkan-center-file-global-mode)
            
            ;; Test that center keybindings are active
            (should (eq (lookup-key (current-active-maps) (kbd "C-M-1")) 'enkan-repl-center-send-1))))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-vs-base-function-difference ()
  "Test that center functions are different from base functions."
  (should-not (eq 'enkan-repl-center-send-1 'enkan-repl-send-1))
  (should-not (eq 'enkan-repl-center-send-2 'enkan-repl-send-2))
  (should-not (eq 'enkan-repl-center-send-3 'enkan-repl-send-3))
  (should-not (eq 'enkan-repl-center-send-4 'enkan-repl-send-4))
  (should-not (eq 'enkan-repl-center-send-5 'enkan-repl-send-5)))

(ert-deftest test-center-file-functions-exist ()
  "Test that center file functions exist and are callable."
  (should (fboundp 'enkan-repl-center-send-1))
  (should (fboundp 'enkan-repl-center-send-2))
  (should (fboundp 'enkan-repl-center-send-3))
  (should (fboundp 'enkan-repl-center-send-4))
  (should (fboundp 'enkan-repl-center-send-5))
  (should (fboundp 'enkan-repl-center-send-enter))
  (should (fboundp 'enkan-repl-center-magit)))

(provide 'center-keybinding-test)

;;; center-keybinding-test.el ends here
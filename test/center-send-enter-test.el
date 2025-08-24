;;; center-send-enter-test.el --- Tests for center file send enter function -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for center file send enter functionality.
;; These tests verify the center send enter function works correctly.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-center-send-enter-function-exists ()
  "Test that center send enter function exists and is callable."
  (should (fboundp 'enkan-repl-center-send-enter))
  (should (commandp 'enkan-repl-center-send-enter)))

(ert-deftest test-center-send-text-with-selection-function-exists ()
  "Test that center send text with selection helper function exists."
  (should (fboundp 'enkan-repl--center-send-text-with-selection)))

(ert-deftest test-center-send-enter-keybinding-in-global-mode ()
  "Test that C-M-e is bound to center send enter in global mode."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Enable center file global mode
          (enkan-center-file-global-mode 1)
          (should enkan-center-file-global-mode)
          
          ;; Test that C-M-e is bound to center send enter
          (should (eq (key-binding (kbd "C-M-e")) 'enkan-repl-center-send-enter)))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-send-enter-vs-base-function ()
  "Test that center send enter is different from base send enter."
  ;; Both functions should exist
  (should (fboundp 'enkan-repl-center-send-enter))
  (should (fboundp 'enkan-repl-send-enter))
  
  ;; They should be different functions
  (should-not (eq (symbol-function 'enkan-repl-center-send-enter) 
                  (symbol-function 'enkan-repl-send-enter))))

(ert-deftest test-center-send-enter-keymap-definition ()
  "Test that center send enter is properly defined in center file keymap."
  (should (keymapp enkan-center-file-global-mode-map))
  (should (eq (lookup-key enkan-center-file-global-mode-map (kbd "C-M-e")) 
              'enkan-repl-center-send-enter)))

(ert-deftest test-center-send-enter-keybinding-priority ()
  "Test that center send enter keybinding takes priority when mode is enabled."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Test with center mode disabled
          (enkan-center-file-global-mode -1)
          (with-temp-buffer
            (let ((current-binding (key-binding (kbd "C-M-e"))))
              ;; Should not be center function when mode is disabled
              (should-not (eq current-binding 'enkan-repl-center-send-enter))))
          
          ;; Test with center mode enabled
          (enkan-center-file-global-mode 1)
          (with-temp-buffer
            (let ((current-binding (key-binding (kbd "C-M-e"))))
              ;; Should be center function when mode is enabled
              (should (eq current-binding 'enkan-repl-center-send-enter)))))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(provide 'center-send-enter-test)

;;; center-send-enter-test.el ends here
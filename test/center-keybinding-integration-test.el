;;; center-keybinding-integration-test.el --- Integration test for center file keybinding override -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Integration tests for center file keybinding override in real usage scenarios.
;; These tests simulate actual user interaction with center file mode.

;;; Code:

(require 'ert)
(require 'enkan-repl)

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
            
            ;; Test that keybindings are properly overridden
            (should (eq (key-binding (kbd "C-M-1")) 'enkan-repl-center-send-1))
            (should (eq (key-binding (kbd "C-M-2")) 'enkan-repl-center-send-2))
            (should (eq (key-binding (kbd "C-M-3")) 'enkan-repl-center-send-3))
            (should (eq (key-binding (kbd "C-M-4")) 'enkan-repl-center-send-4))
            (should (eq (key-binding (kbd "C-M-5")) 'enkan-repl-center-send-5))
            (should (eq (key-binding (kbd "<escape>")) 'enkan-repl-center-send-escape))
            
            ;; Test toggle functionality
            (enkan-toggle-center-file-global-mode)
            (should-not enkan-center-file-global-mode)
            
            ;; Keybindings should no longer be center functions
            (should-not (eq (key-binding (kbd "C-M-1")) 'enkan-repl-center-send-1))
            
            ;; Toggle back on
            (enkan-toggle-center-file-global-mode)
            (should enkan-center-file-global-mode)
            (should (eq (key-binding (kbd "C-M-1")) 'enkan-repl-center-send-1))))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(ert-deftest test-center-file-functions-exist ()
  "Test that all center file functions are properly defined."
  ;; Test that center file functions exist and are callable
  (should (fboundp 'enkan-repl-center-send-1))
  (should (fboundp 'enkan-repl-center-send-2))
  (should (fboundp 'enkan-repl-center-send-3))
  (should (fboundp 'enkan-repl-center-send-4))
  (should (fboundp 'enkan-repl-center-send-5))
  (should (fboundp 'enkan-repl-center-send-escape))
  
  ;; Test that functions are interactive
  (should (commandp 'enkan-repl-center-send-1))
  (should (commandp 'enkan-repl-center-send-2))
  (should (commandp 'enkan-repl-center-send-3))
  (should (commandp 'enkan-repl-center-send-4))
  (should (commandp 'enkan-repl-center-send-5))
  (should (commandp 'enkan-repl-center-send-escape)))

(ert-deftest test-center-vs-base-function-difference ()
  "Test that center functions are different from base functions."
  ;; Center functions should exist
  (should (fboundp 'enkan-repl-center-send-1))
  (should (fboundp 'enkan-repl-center-send-2))
  (should (fboundp 'enkan-repl-center-send-3))
  
  ;; Base functions should exist
  (should (fboundp 'enkan-repl-send-1))
  (should (fboundp 'enkan-repl-send-2))
  (should (fboundp 'enkan-repl-send-3))
  (should (fboundp 'enkan-repl-send-4))
  (should (fboundp 'enkan-repl-send-5))
  
  ;; They should be different functions
  (should-not (eq (symbol-function 'enkan-repl-center-send-1) 
                  (symbol-function 'enkan-repl-send-1)))
  (should-not (eq (symbol-function 'enkan-repl-center-send-2) 
                  (symbol-function 'enkan-repl-send-2)))
  (should-not (eq (symbol-function 'enkan-repl-center-send-3) 
                  (symbol-function 'enkan-repl-send-3))))

(ert-deftest test-keybinding-priority-override ()
  "Test that center file keybindings take priority over base keybindings."
  (let ((original-state enkan-center-file-global-mode))
    (unwind-protect
        (progn
          ;; Test with center mode disabled
          (enkan-center-file-global-mode -1)
          (with-temp-buffer
            (let ((current-binding (key-binding (kbd "C-M-1"))))
              ;; Should not be center function when mode is disabled
              (should-not (eq current-binding 'enkan-repl-center-send-1))))
          
          ;; Test with center mode enabled
          (enkan-center-file-global-mode 1)
          (with-temp-buffer
            (let ((current-binding (key-binding (kbd "C-M-1"))))
              ;; Should be center function when mode is enabled
              (should (eq current-binding 'enkan-repl-center-send-1)))))
      ;; Restore original state
      (if original-state
          (enkan-center-file-global-mode 1)
        (enkan-center-file-global-mode -1)))))

(provide 'center-keybinding-integration-test)

;;; center-keybinding-integration-test.el ends here
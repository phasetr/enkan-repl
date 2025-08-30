;;; alias-initialization-test.el --- Test alias initialization in setup/finish functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Test that enkan-repl-project-aliases is properly initialized and cleared
;; in enkan-repl-setup and enkan-repl-teardown.

;;; Code:

(require 'ert)

;; Load the main package
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el"
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

(ert-deftest test-alias-setup-in-auto-setup ()
  "Test that enkan-repl-setup properly sets up aliases."
  ;; Set global variables directly
  (setq enkan-repl-projects
        '(("test-layout" "er" "pt")))
  (setq enkan-repl-target-directories
        '(("er" . ("enkan-repl" . "/test/enkan-repl"))
          ("pt" . ("pt-tools" . "/test/pt-tools"))))
  (setq enkan-repl-project-aliases nil)
  (setq enkan-repl-session-list nil)
  (setq enkan-repl--session-counter 0)
  (setq enkan-repl--current-project nil)
  (setq enkan-center-file-global-mode nil)
  ;; Mock functions to avoid actual process creation
  (cl-letf (((symbol-function 'enkan-repl-start-eat) (lambda (&optional force) t))
            ((symbol-function 'enkan-center-file-global-mode) (lambda (arg)
                                                                 (setq enkan-center-file-global-mode (> arg 0))))
            ((symbol-function 'message) (lambda (&rest args) nil)))
    ;; Test that aliases are nil before setup
    (should (null enkan-repl-project-aliases))
    ;; Run auto-setup
    (enkan-repl-setup "test-layout")
    ;; Verify that aliases are properly set
    (should enkan-repl-project-aliases)
    (should (equal '(("er" . "enkan-repl") ("pt" . "pt-tools"))
                   enkan-repl-project-aliases))
    (should (equal '((1 . "enkan-repl") (2 . "pt-tools"))
                   enkan-repl-session-list))))

(ert-deftest test-alias-clear-in-finish-all-sessions ()
  "Test that enkan-repl-teardown properly clears aliases."
  ;; Setup initial state with aliases
  (setq enkan-repl-project-aliases '(("er" . "enkan-repl") ("pt" . "pt-tools")))
  (setq enkan-repl-session-list '((1 . "enkan-repl") (2 . "pt-tools")))
  (setq enkan-repl--session-counter 2)
  (setq enkan-repl--current-project "test-layout")
  (setq enkan-center-file-global-mode t)
  (setq enkan-repl-target-directories
        '(("er" . ("enkan-repl" . "/test/enkan-repl"))
          ("pt" . ("pt-tools" . "/test/pt-tools"))))

  ;; Mock functions
  (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
             (lambda (path) nil))  ; No buffers found
            ((symbol-function 'y-or-n-p) (lambda (prompt) t))  ; Always yes
            ((symbol-function 'enkan-center-file-global-mode)
             (lambda (arg) (when (numberp arg) (setq enkan-center-file-global-mode (> arg 0)))))
            ((symbol-function 'message) (lambda (&rest args) nil)))

    ;; Verify initial state
    (should enkan-repl-project-aliases)
    (should enkan-center-file-global-mode)

    ;; Run finish-all-sessions
    (enkan-repl-teardown)

    ;; Verify that everything is cleared
    (should (null enkan-repl-project-aliases))
    (should (null enkan-repl-session-list))
    (should (= 0 enkan-repl--session-counter))
    (should (null enkan-repl--current-project))
    (should (null enkan-center-file-global-mode))))

(provide 'alias-initialization-test)
;;; alias-initialization-test.el ends here

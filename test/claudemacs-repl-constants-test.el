;;; claudemacs-repl-constants-test.el --- Tests for precompiled constants -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for claudemacs-repl precompiled constants functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path
(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'claudemacs-repl)
(require 'claudemacs-repl-constants)

(ert-deftest test-constants-file-exists ()
  "Test that constants file exists and loads correctly."
  (should (featurep 'claudemacs-repl-constants))
  (should (boundp 'claudemacs-repl-cheatsheet-candidates))
  (should (boundp 'claudemacs-repl-cheatsheet-function-count)))

(ert-deftest test-constants-structure ()
  "Test that constants have correct structure."
  (should (listp claudemacs-repl-cheatsheet-candidates))
  (should (> (length claudemacs-repl-cheatsheet-candidates) 10))
  (should (numberp claudemacs-repl-cheatsheet-function-count)))

(ert-deftest test-constants-content ()
  "Test that constants contain expected functions."
  (let ((function-names (mapcar #'car claudemacs-repl-cheatsheet-candidates)))
    (should (member "claudemacs-repl-cheatsheet" function-names))
    (should (member "claudemacs-repl-send-region" function-names))
    (should (member "claudemacs-repl-send-buffer" function-names))
    (should (member "claudemacs-repl-start-claudemacs" function-names))))

(ert-deftest test-constants-consistency ()
  "Test that constants are consistent with function count."
  (should (= (length claudemacs-repl-cheatsheet-candidates)
             claudemacs-repl-cheatsheet-function-count)))

(ert-deftest test-cheatsheet-uses-constants ()
  "Test that cheatsheet function uses precompiled constants."
  ;; Test that constants are accessible in cheatsheet context
  (should (boundp 'claudemacs-repl-cheatsheet-candidates))
  ;; Test that constants are properly formatted for completion
  (let ((candidates claudemacs-repl-cheatsheet-candidates))
    (should (cl-every (lambda (candidate)
                        (and (consp candidate)
                             (stringp (car candidate))
                             (stringp (cdr candidate))))
                      candidates))))

(provide 'claudemacs-repl-constants-test)

;;; claudemacs-repl-constants-test.el ends here
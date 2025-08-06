;;; claudemacs-repl-utils-doc-test.el --- Tests for documentation generation helpers -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for claudemacs-repl documentation generation utilities.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path
(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'claudemacs-repl-utils)

(ert-deftest test-extract-category-from-docstring ()
  "Test category extraction from docstrings."
  (should (string= "Text Sender"
                   (claudemacs-repl-utils--extract-category-from-docstring
                    "Send region to Claude.\n\nCategory: Text Sender")))
  (should (string= "Command Palette"
                   (claudemacs-repl-utils--extract-category-from-docstring
                    "Display cheatsheet.\n\nCategory: Command Palette")))
  (should (null (claudemacs-repl-utils--extract-category-from-docstring
                 "No category info")))
  (should (null (claudemacs-repl-utils--extract-category-from-docstring nil))))

(ert-deftest test-get-categorized-functions ()
  "Test function categorization from real file."
  (let ((claudemacs-repl-file (expand-file-name "claudemacs-repl.el"))
        categorized-functions)
    (should (file-exists-p claudemacs-repl-file))
    (setq categorized-functions (claudemacs-repl-utils--get-categorized-functions claudemacs-repl-file))

    ;; Should have all expected categories
    (should (assoc "Command Palette" categorized-functions))
    (should (assoc "Text Sender" categorized-functions))
    (should (assoc "Session Controller" categorized-functions))
    (should (assoc "Utilities" categorized-functions))

    ;; Command Palette should have cheatsheet
    (let ((command-palette-funcs (cdr (assoc "Command Palette" categorized-functions))))
      (should (cl-some (lambda (f) (string= (plist-get f :name) "claudemacs-repl-cheatsheet"))
                       command-palette-funcs)))

    ;; Text Sender should have multiple functions
    (let ((text-sender-funcs (cdr (assoc "Text Sender" categorized-functions))))
      (should (> (length text-sender-funcs) 5)))

    ;; Functions should be ordered by category priority
    (should (string= "Command Palette" (car (car categorized-functions))))))

(ert-deftest test-generate-org-section ()
  "Test org section generation."
  (let* ((functions '((:name "test-func-1" :docstring "First test function")
                     (:name "test-func-2" :docstring "Second test function")))
         (result (claudemacs-repl-utils--generate-org-section "Test Category" functions 2)))
    (should (string-match-p "^\\*\\* Test Category" result))
    (should (string-match-p "~M-x test-func-1~ - First test function" result))
    (should (string-match-p "~M-x test-func-2~ - Second test function" result))))

(ert-deftest test-generate-categorized-documentation ()
  "Test full categorized documentation generation."
  (let ((claudemacs-repl-file (expand-file-name "claudemacs-repl.el"))
        (result (claudemacs-repl-utils--generate-categorized-documentation
                 (expand-file-name "claudemacs-repl.el") 3)))
    (should (string-match-p "^\\*\\*\\* Command Palette" result))
    (should (string-match-p "^\\*\\*\\* Text Sender" result))
    (should (string-match-p "^\\*\\*\\* Session Controller" result))
    (should (string-match-p "^\\*\\*\\* Utilities" result))
    (should (string-match-p "~M-x claudemacs-repl-cheatsheet~" result))
    (should (string-match-p "~M-x claudemacs-repl-send-region~" result))))

(provide 'claudemacs-repl-utils-doc-test)

;;; claudemacs-repl-utils-doc-test.el ends here

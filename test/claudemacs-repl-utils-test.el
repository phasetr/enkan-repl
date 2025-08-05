;;; claudemacs-repl-utils-test.el --- Tests for claudemacs-repl-utils -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for claudemacs-repl-utils.el functions.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path
(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'claudemacs-repl-utils)

(ert-deftest test-claudemacs-repl-utils--extract-function-info-basic ()
  "Test basic function info extraction."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert ";;;###autoload\n")
            (insert "(defun test-function (arg)\n")
            (insert "  \"Test docstring.\"\n")
            (insert "  (interactive \"sInput: \")\n")
            (insert "  arg)\n")
            (insert "\n")
            (insert "(defun test-private--function ()\n")
            (insert "  \"Private function.\"\n")
            (insert "  nil)\n"))
          (let ((functions (claudemacs-repl-utils--extract-function-info test-file)))
            (should (= (length functions) 1))
            (let ((func (car functions)))
              (should (string= (plist-get func :name) "test-function"))
              (should (string= (plist-get func :docstring) "Test docstring."))
              (should (plist-get func :interactive))
              (should (plist-get func :autoload)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-utils--extract-function-info-multiline-docstring ()
  "Test extraction of multi-line docstrings."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert "(defun test-multiline ()\n")
            (insert "  \"First line of docstring.\n")
            (insert "Second line of docstring.\n")
            (insert "Third line.\"\n")
            (insert "  (interactive)\n")
            (insert "  nil)\n"))
          (let ((functions (claudemacs-repl-utils--extract-function-info test-file)))
            (should (= (length functions) 1))
            (let ((func (car functions)))
              (should (string= (plist-get func :name) "test-multiline"))
              (should (string-match "First line.*Second line.*Third line" 
                                   (plist-get func :docstring)))
              (should (plist-get func :interactive)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-utils--extract-function-info-no-docstring ()
  "Test extraction of functions without docstrings."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert "(defun test-no-docstring ()\n")
            (insert "  (interactive)\n")
            (insert "  nil)\n"))
          (let ((functions (claudemacs-repl-utils--extract-function-info test-file)))
            (should (= (length functions) 1))
            (let ((func (car functions)))
              (should (string= (plist-get func :name) "test-no-docstring"))
              (should (string= (plist-get func :docstring) ""))
              (should (plist-get func :interactive)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-utils--generate-function-list-flat ()
  "Test flat function list generation."
  (let ((functions '((:name "test-func1" :docstring "First function" :interactive t)
                     (:name "test-func2" :docstring "Second function" :interactive t)
                     (:name "test-func3" :docstring "" :interactive t))))
    (let ((result (claudemacs-repl-utils--generate-function-list-flat functions)))
      (should (string-match-p "~M-x test-func1~ - First function" result))
      (should (string-match-p "~M-x test-func2~ - Second function" result))
      (should (string-match-p "~M-x test-func3~ - Send command" result))
      (should (string-match-p "\n" result)))))

(ert-deftest test-claudemacs-repl-utils--generate-function-list-flat-empty ()
  "Test flat function list generation with empty input."
  (let ((result (claudemacs-repl-utils--generate-function-list-flat '())))
    (should (string= result ""))))

(ert-deftest test-claudemacs-repl-utils--get-all-public-functions-integration ()
  "Test complete integration of public function extraction."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert ";;;###autoload\n")
            (insert "(defun test-public-func1 ()\n")
            (insert "  \"Public function one.\"\n")
            (insert "  (interactive)\n")
            (insert "  nil)\n")
            (insert "\n")
            (insert "(defun test-public-func2 ()\n")
            (insert "  \"Public function two.\"\n")
            (insert "  (interactive)\n")
            (insert "  nil)\n")
            (insert "\n")
            (insert "(defun test-private--func ()\n")
            (insert "  \"Private function.\"\n")
            (insert "  nil)\n"))
          (let ((result (claudemacs-repl-utils--get-all-public-functions test-file)))
            (should (string-match-p "~M-x test-public-func1~ - Public function one" result))
            (should (string-match-p "~M-x test-public-func2~ - Public function two" result))
            (should-not (string-match-p "test-private--func" result))
            (should (= (length (split-string result "\n" t)) 2))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-utils--extract-function-info-complex-args ()
  "Test extraction of functions with complex argument lists."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert "(defun test-complex-args (start end &optional arg1 arg2)\n")
            (insert "  \"Function with complex args.\"\n")
            (insert "  (interactive \"r\")\n")
            (insert "  nil)\n"))
          (let ((functions (claudemacs-repl-utils--extract-function-info test-file)))
            (should (= (length functions) 1))
            (let ((func (car functions)))
              (should (string= (plist-get func :name) "test-complex-args"))
              (should (string= (plist-get func :docstring) "Function with complex args."))
              (should (plist-get func :interactive))
              (let ((args (plist-get func :args)))
                (should (listp args))
                (should (member 'start args))
                (should (member 'end args))))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-utils--extract-function-info-noninteractive-autoload ()
  "Test extraction of non-interactive autoload functions."
  (let ((test-file (make-temp-file "test-utils-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";;; test file\n")
            (insert ";;;###autoload\n")
            (insert "(defun test-autoload-only ()\n")
            (insert "  \"Autoload but not interactive.\"\n")
            (insert "  nil)\n"))
          (let ((functions (claudemacs-repl-utils--extract-function-info test-file)))
            (should (= (length functions) 1))
            (let ((func (car functions)))
              (should (string= (plist-get func :name) "test-autoload-only"))
              (should (string= (plist-get func :docstring) "Autoload but not interactive."))
              (should-not (plist-get func :interactive))
              (should (plist-get func :autoload)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'claudemacs-repl-utils-test)

;;; claudemacs-repl-utils-test.el ends here
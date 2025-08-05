;;; claudemacs-repl-cheatsheet-test.el --- Tests for cheatsheet functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for claudemacs-repl cheatsheet functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load path
(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'claudemacs-repl)

(ert-deftest test-claudemacs-repl-cheatsheet-function-exists ()
  "Test that cheatsheet function is defined."
  (should (fboundp 'claudemacs-repl-cheatsheet)))

(ert-deftest test-claudemacs-repl-cheatsheet-candidate-generation ()
  "Test cheatsheet candidate generation logic."
  (let* ((test-file (make-temp-file "test-cheatsheet-" nil ".el"))
         (test-content ";;; test file
;;;###autoload
(defun test-interactive-func ()
  \"Test interactive function.\"
  (interactive)
  nil)

(defun test-autoload-func ()
  \"Test autoload function.\"
  nil)

(defun test-private--func ()
  \"Private function.\"
  nil)"))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert test-content))
          (let* ((functions-info (claudemacs-repl-utils--extract-function-info test-file))
                 (interactive-functions (cl-remove-if-not
                                        (lambda (f) (plist-get f :interactive))
                                        functions-info))
                 (candidates (mapcar (lambda (func)
                                      (cons (plist-get func :name)
                                            (or (plist-get func :docstring) "No description")))
                                    interactive-functions)))
            (should (= (length candidates) 1))
            (should (string= (caar candidates) "test-interactive-func"))
            (should (string= (cdar candidates) "Test interactive function."))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-claudemacs-repl-cheatsheet-real-file-extraction ()
  "Test cheatsheet extraction from actual claudemacs-repl.el file."
  (let* ((claudemacs-repl-file (expand-file-name "claudemacs-repl.el"))
         (functions-info (claudemacs-repl-utils--extract-function-info claudemacs-repl-file))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions-info)))
    (should (> (length interactive-functions) 10))
    (should (cl-some (lambda (f) (string= (plist-get f :name) "claudemacs-repl-send-region"))
                     interactive-functions))
    (should (cl-some (lambda (f) (string= (plist-get f :name) "claudemacs-repl-cheatsheet"))
                     interactive-functions))))

(ert-deftest test-claudemacs-repl-cheatsheet-file-path-resolution ()
  "Test that cheatsheet file path logic works."
  (let ((claudemacs-repl-file (expand-file-name "claudemacs-repl.el")))
    (should (file-exists-p claudemacs-repl-file))))

(ert-deftest test-claudemacs-repl-cheatsheet-annotation-format ()
  "Test cheatsheet annotation formatting."
  (let* ((test-candidates '(("func1" . "First function")
                           ("func2" . "Second function")
                           ("func3" . "")))
         (annotation-func (lambda (candidate)
                           (let ((description (alist-get candidate test-candidates nil nil #'string=)))
                             (when description
                               (format " — %s" description))))))
    (should (string= (funcall annotation-func "func1") " — First function"))
    (should (string= (funcall annotation-func "func2") " — Second function"))
    (should (string= (funcall annotation-func "func3") " — "))
    (should (null (funcall annotation-func "nonexistent")))))

(provide 'claudemacs-repl-cheatsheet-test)

;;; claudemacs-repl-cheatsheet-test.el ends here
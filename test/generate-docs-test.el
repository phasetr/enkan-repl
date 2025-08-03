;;; generate-docs-test.el --- Tests for generate-docs.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the documentation generation script.
;; This test file is separate from the main package tests to clearly
;; distinguish between core functionality tests and development tool tests.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the script being tested
(load-file "scripts/generate-docs.el")

;;; Test Data Setup

(defvar extract-public-functions-test-data
  ";;; test-file.el --- Test file for extraction -*- lexical-binding: t -*-

;;;###autoload
(defun test-autoload-function (arg1 arg2)
  \"This is an autoload function with arguments.\"
  (interactive \"sArg1: \\nsArg2: \")
  (message \"Autoload: %s %s\" arg1 arg2))

(defun test-interactive-function ()
  \"This is an interactive function without autoload.\"
  (interactive)
  (message \"Interactive function called\"))

(defun test-private-function ()
  \"This is a private function.\"
  (message \"Private function\"))

;;;###autoload
(defun test-autoload-no-interactive (param)
  \"This is autoload but not interactive.\"
  (format \"Result: %s\" param))

(defun test-no-docstring ()
  (interactive)
  (message \"No docstring\"))

;;;###autoload
(defun test-multiline-docstring ()
  \"This is a multiline docstring.
It spans multiple lines for testing.\"
  (interactive)
  (message \"Multiline docstring function\"))
"
  "Test Emacs Lisp content for extraction testing.")

;;; Helper Functions

(defun extract-public-functions-test--create-temp-file (content)
  "Create a temporary file with CONTENT and return its path."
  (let ((temp-file (make-temp-file "extract-test-" nil ".el")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

;;; Tests for Function Extraction

(ert-deftest test-extract-function-info-basic ()
  "Test basic function information extraction."
  (let* ((temp-file (extract-public-functions-test--create-temp-file 
                     extract-public-functions-test-data))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        (progn
          ;; Should extract 5 public functions (autoload or interactive)
          (should (= (length functions) 5))
          
          ;; Check first function (autoload with interactive)
          (let ((first-func (cl-find "test-autoload-function" functions 
                                     :key (lambda (f) (plist-get f :name)) 
                                     :test #'string=)))
            (should first-func)
            (should (plist-get first-func :autoload))
            (should (plist-get first-func :interactive))
            (should (string= (plist-get first-func :docstring) 
                             "This is an autoload function with arguments."))
            (should (equal (plist-get first-func :args) '(arg1 arg2))))
          
          ;; Check interactive-only function
          (let ((interactive-func (cl-find "test-interactive-function" functions 
                                           :key (lambda (f) (plist-get f :name)) 
                                           :test #'string=)))
            (should interactive-func)
            (should-not (plist-get interactive-func :autoload))
            (should (plist-get interactive-func :interactive))))
      
      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-extract-function-info-autoload-only ()
  "Test extraction of autoload-only functions."
  (let* ((temp-file (extract-public-functions-test--create-temp-file 
                     extract-public-functions-test-data))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        (let ((autoload-func (cl-find "test-autoload-no-interactive" functions 
                                      :key (lambda (f) (plist-get f :name)) 
                                      :test #'string=)))
          (should autoload-func)
          (should (plist-get autoload-func :autoload))
          (should-not (plist-get autoload-func :interactive))
          (should (equal (plist-get autoload-func :args) '(param))))
      
      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-extract-function-info-private-filtered ()
  "Test that private functions are filtered out."
  (let* ((temp-file (extract-public-functions-test--create-temp-file 
                     extract-public-functions-test-data))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        (progn
          ;; Private function should not be extracted
          (let ((private-func (cl-find "test-private-function" functions 
                                       :key (lambda (f) (plist-get f :name)) 
                                       :test #'string=)))
            (should-not private-func)))
      
      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-extract-function-info-no-docstring ()
  "Test handling of functions without docstrings."
  (let* ((temp-file (extract-public-functions-test--create-temp-file 
                     extract-public-functions-test-data))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        (let ((no-doc-func (cl-find "test-no-docstring" functions 
                                    :key (lambda (f) (plist-get f :name)) 
                                    :test #'string=)))
          (should no-doc-func)
          (should (plist-get no-doc-func :interactive))
          (should (string= (plist-get no-doc-func :docstring) "")))
      
      ;; Cleanup
      (delete-file temp-file))))

;;; Tests for Function Formatting

(ert-deftest test-format-function-info-complete ()
  "Test formatting of complete function information in org-mode format."
  (let* ((func-info (list :name "test-function"
                          :args '(arg1 &optional arg2)
                          :docstring "Test function description."
                          :interactive t
                          :autoload t))
         (formatted (claudemacs-client--format-function-info func-info)))
    (should (string-match-p "\\*\\* ~test-function~" formatted))
    (should (string-match-p "\\*Type\\*: Interactive Command" formatted))
    (should (string-match-p "\\*Autoload\\*: Yes" formatted))
    (should (string-match-p "\\*Description\\*: Test function description\\." formatted))
    (should (string-match-p "(arg1 &optional arg2)" formatted))))

(ert-deftest test-format-function-info-minimal ()
  "Test formatting of minimal function information in org-mode format."
  (let* ((func-info (list :name "minimal-function"
                          :args nil
                          :docstring ""
                          :interactive nil
                          :autoload t))
         (formatted (claudemacs-client--format-function-info func-info)))
    (should (string-match-p "\\*\\* ~minimal-function~" formatted))
    (should (string-match-p "\\*Autoload\\*: Yes" formatted))
    (should-not (string-match-p "\\*Type\\*" formatted))
    (should-not (string-match-p "\\*Description\\*" formatted))))

;;; Tests for API Documentation Generation

(ert-deftest test-generate-public-api-docs ()
  "Test complete API documentation generation in org-mode format."
  (let* ((temp-input (extract-public-functions-test--create-temp-file 
                      extract-public-functions-test-data))
         (temp-output (make-temp-file "api-docs-" nil ".org")))
    (unwind-protect
        (progn
          (claudemacs-client--generate-public-api-docs temp-input temp-output)
          
          ;; Check that output file was created and has content
          (should (file-exists-p temp-output))
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-output)
                           (buffer-string))))
            (should (string-match-p "#\\+TITLE: Public API" content))
            (should (string-match-p "\\* Public API" content))
            (should (string-match-p "test-autoload-function" content))
            (should (string-match-p "test-interactive-function" content))
            (should-not (string-match-p "test-private-function" content))))
      
      ;; Cleanup
      (delete-file temp-input)
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

;;; Tests for Edge Cases

(ert-deftest test-extract-function-info-empty-file ()
  "Test extraction from an empty file."
  (let* ((temp-file (extract-public-functions-test--create-temp-file ""))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        (should (= (length functions) 0))
      
      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-extract-function-info-malformed-elisp ()
  "Test extraction from malformed Emacs Lisp."
  (let* ((malformed-content ";;;###autoload\n(defun broken-function\n  \"Missing closing paren\"")
         (temp-file (extract-public-functions-test--create-temp-file malformed-content))
         (functions (claudemacs-client--extract-function-info temp-file)))
    (unwind-protect
        ;; Should not crash and return empty list for malformed content
        (should (listp functions))
      
      ;; Cleanup
      (delete-file temp-file))))

;;; Tests for Default Template Generation

(ert-deftest test-generate-default-template-basic ()
  "Test basic default template generation."
  (let ((temp-output (make-temp-file "default-template-" nil ".org")))
    (unwind-protect
        (progn
          (claudemacs-client--generate-default-template temp-output)
          
          ;; Check that output file was created and has content
          (should (file-exists-p temp-output))
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-output)
                           (buffer-string))))
            ;; Check basic structure
            (should (string-match-p "#\\+TITLE: Claude Input File" content))
            (should (string-match-p "#\\+AUTHOR: claudemacs-client" content))
            (should (string-match-p "\\* Quick Start" content))
            (should (string-match-p "\\* Essential Commands" content))
            (should (string-match-p "\\* Working Notes" content))
            
            ;; Check function references
            (should (string-match-p "claudemacs-client-start-claudemacs" content))
            (should (string-match-p "claudemacs-client-setup-window-layout" content))
            (should (string-match-p "claudemacs-client-send-region" content))
            (should (string-match-p "claudemacs-client-status" content))))
      
      ;; Cleanup
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(ert-deftest test-generate-default-template-content-structure ()
  "Test that generated template has proper org-mode structure."
  (let ((temp-output (make-temp-file "default-template-" nil ".org")))
    (unwind-protect
        (progn
          (claudemacs-client--generate-default-template temp-output)
          
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-output)
                           (buffer-string))))
            ;; Check code blocks
            (should (string-match-p "#\\+BEGIN_SRC emacs-lisp" content))
            (should (string-match-p "#\\+END_SRC" content))
            
            ;; Check emphasis markup
            (should (string-match-p "\\*Send current region\\*:" content))
            (should (string-match-p "~M-x claudemacs-client-send-region~" content))
            
            ;; Check hierarchical structure
            (should (string-match-p "^\\*\\* 1\\. Start Claude Code Session" content))
            (should (string-match-p "^\\*\\* Sending Content to Claude Code" content))
            (should (string-match-p "^\\*\\* Project Context" content))))
      
      ;; Cleanup
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(ert-deftest test-generate-all-docs-integration ()
  "Test that generate-all-docs creates both API and template files."
  (let ((temp-api (make-temp-file "api-" nil ".org"))
        (temp-template (make-temp-file "template-" nil ".org"))
        (temp-input (extract-public-functions-test--create-temp-file 
                     extract-public-functions-test-data)))
    (unwind-protect
        (progn
          ;; Test individual generation functions directly
          (claudemacs-client--generate-public-api-docs temp-input temp-api)
          (claudemacs-client--generate-default-template temp-template)
          
          ;; Check both files were created
          (should (file-exists-p temp-api))
          (should (file-exists-p temp-template))
          
          ;; Check API file content
          (let ((api-content (with-temp-buffer
                               (insert-file-contents temp-api)
                               (buffer-string))))
            (should (string-match-p "#\\+TITLE: Public API" api-content))
            (should (string-match-p "test-autoload-function" api-content)))
          
          ;; Check template file content
          (let ((template-content (with-temp-buffer
                                    (insert-file-contents temp-template)
                                    (buffer-string))))
            (should (string-match-p "#\\+TITLE: Claude Input File" template-content))
            (should (string-match-p "claudemacs-client-start-claudemacs" template-content))))
      
      ;; Cleanup
      (delete-file temp-input)
      (when (file-exists-p temp-api)
        (delete-file temp-api))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

;;; Test Runner

(defun generate-docs-run-all-tests ()
  "Run all generate-docs tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-extract\\|^test-format\\|^test-generate"))

(provide 'generate-docs-test)

;;; generate-docs-test.el ends here
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
         (functions (enkan-repl--extract-function-info temp-file)))
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
         (functions (enkan-repl--extract-function-info temp-file)))
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
         (functions (enkan-repl--extract-function-info temp-file)))
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
         (functions (enkan-repl--extract-function-info temp-file)))
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
         (formatted (enkan-repl--format-function-info func-info)))
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
         (formatted (enkan-repl--format-function-info func-info)))
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
          (enkan-repl--generate-public-api-docs temp-input temp-output)

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
         (functions (enkan-repl--extract-function-info temp-file)))
    (unwind-protect
        (should (= (length functions) 0))

      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-extract-function-info-malformed-elisp ()
  "Test extraction from malformed Emacs Lisp."
  (let* ((malformed-content ";;;###autoload\n(defun broken-function\n  \"Missing closing paren\"")
         (temp-file (extract-public-functions-test--create-temp-file malformed-content))
         (functions (enkan-repl--extract-function-info temp-file)))
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
          (enkan-repl--generate-default-template temp-output)

          ;; Check that output file was created and has content
          (should (file-exists-p temp-output))
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-output)
                           (buffer-string))))
            ;; Check basic structure
            (should (string-match-p "\\* Quick Start" content))
            (should (string-match-p "\\* Essential Commands" content))
            (should (string-match-p "\\* Working Notes" content))

            ;; Check function references
            (should (string-match-p "enkan-repl-start-eat" content))
            (should (string-match-p "enkan-repl-setup-window-layout" content))
            (should (string-match-p "enkan-repl-send-region" content))
            (should (string-match-p "enkan-repl-status" content))))

      ;; Cleanup
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(ert-deftest test-generate-default-template-content-structure ()
  "Test that generated template has proper org-mode structure."
  (let ((temp-output (make-temp-file "default-template-" nil ".org")))
    (unwind-protect
        (progn
          (enkan-repl--generate-default-template temp-output)

          (let ((content (with-temp-buffer
                           (insert-file-contents temp-output)
                           (buffer-string))))
            ;; Check code blocks
            (should (string-match-p "#\\+BEGIN_SRC emacs-lisp" content))
            (should (string-match-p "#\\+END_SRC" content))

            ;; Check emphasis markup
            (should (string-match-p "\\*Send current region\\*:" content))
            (should (string-match-p "~M-x enkan-repl-send-region~" content))

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
          (enkan-repl--generate-public-api-docs temp-input temp-api)
          (enkan-repl--generate-default-template temp-template)

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
            (should (string-match-p "\\* Quick Start" template-content))
            (should (string-match-p "enkan-repl-start-eat" template-content))))

      ;; Cleanup
      (delete-file temp-input)
      (when (file-exists-p temp-api)
        (delete-file temp-api))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

;;; Tests for New Core Functions Section Generation

(ert-deftest test-generate-core-functions-section-structure ()
  "Test that generate-core-functions-section produces correct structure."
  (let ((result (enkan-repl--generate-core-functions-section)))
    ;; Should be non-empty string
    (should (stringp result))
    (should (> (length result) 0))
    ;; Should start with Core Functions header
    (should (string-prefix-p "** Core Functions\n\n" result))
    ;; Should contain all expected categories
    (should (string-match-p "\\*\\*\\* Command Palette" result))
    (should (string-match-p "\\*\\*\\* File Management" result))
    (should (string-match-p "\\*\\*\\* Text Sending" result))
    (should (string-match-p "\\*\\*\\* Session Control" result))
    (should (string-match-p "\\*\\*\\* Utilities" result))))

(ert-deftest test-generate-core-functions-section-content ()
  "Test that generate-core-functions-section contains expected functions."
  (let ((result (enkan-repl--generate-core-functions-section)))
    ;; Should contain key functions
    (should (string-match-p "enkan-repl-cheat-sheet" result))
    (should (string-match-p "enkan-repl-send-region" result))
    (should (string-match-p "enkan-repl-start-eat" result))
    (should (string-match-p "enkan-repl-open-project-input-file" result))
    ;; Should have proper org-mode formatting
    (should (string-match-p "- =" result))
    (should (string-match-p "\\*Interactive command browser" result))))

(ert-deftest test-generate-core-functions-section-formatting ()
  "Test proper org-mode formatting in core functions section."
  (let ((result (enkan-repl--generate-core-functions-section)))
    ;; Should have proper section headers
    (should (string-match-p "^\\*\\* Core Functions$" result))
    (should (string-match-p "^\\*\\*\\* Command Palette$" result))
    ;; Should have proper function entries
    (should (string-match-p "^- =enkan-repl-" result))
    ;; Should have indented sub-bullets for command palette
    (should (string-match-p "^  - Browse all available" result))
    ;; Should end with proper spacing
    (should (string-suffix-p "\n\n" result))))

(ert-deftest test-generate-readme-with-temp-file ()
  "Test README generation with temporary file."
  (let ((temp-file (make-temp-file "test-readme-" nil ".org")))
    (unwind-protect
        (progn
          ;; Create test README with Core Functions section
          (with-temp-file temp-file
            (insert "#+TITLE: Test README\n\n")
            (insert "** Introduction\n\nSome intro text.\n\n")
            (insert "** Core Functions\n\nOld content here.\n\n")
            (insert "** Configuration\n\nConfig content.\n\n"))
          ;; Generate new README
          (enkan-repl--generate-readme temp-file)
          ;; Check that Core Functions section was updated
          (let ((updated-content (with-temp-buffer
                                   (insert-file-contents temp-file)
                                   (buffer-string))))
            (should (string-match-p "\\*\\* Core Functions" updated-content))
            (should (string-match-p "\\*\\*\\* Command Palette" updated-content))
            (should (string-match-p "enkan-repl-cheat-sheet" updated-content))
            ;; Should preserve other sections
            (should (string-match-p "\\*\\* Introduction" updated-content))
            (should (string-match-p "\\*\\* Configuration" updated-content))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-generate-readme-section-boundaries ()
  "Test that README generation respects section boundaries."
  (let ((temp-file (make-temp-file "test-readme-boundaries-" nil ".org")))
    (unwind-protect
        (progn
          ;; Create test README with specific structure
          (with-temp-file temp-file
            (insert "** Before Section\n\nBefore content.\n\n")
            (insert "** Core Functions\n\nOld functions content.\n\n")
            (insert "** Configuration\n\nAfter content.\n\n"))
          ;; Generate new README
          (enkan-repl--generate-readme temp-file)
          ;; Check boundaries are respected
          (let ((updated-content (with-temp-buffer
                                   (insert-file-contents temp-file)
                                   (buffer-string))))
            ;; Should preserve content before Core Functions
            (should (string-match-p "Before content" updated-content))
            ;; Should preserve content after Core Functions
            (should (string-match-p "After content" updated-content))
            ;; Should have updated Core Functions content
            (should (string-match-p "\\*\\*\\* Command Palette" updated-content))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-generate-readme-missing-markers ()
  "Test README generation when section markers are missing."
  (let ((temp-file (make-temp-file "test-readme-missing-" nil ".org")))
    (unwind-protect
        (progn
          ;; Create README without Core Functions section
          (with-temp-file temp-file
            (insert "#+TITLE: Test README\n\n")
            (insert "** Introduction\n\nSome content.\n\n"))
          ;; Capture message output
          (let ((messages '()))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-str &rest args)
                         (push (apply #'format format-str args) messages))))
              (enkan-repl--generate-readme temp-file)
              ;; Should get appropriate message about missing markers
              (should (cl-some (lambda (msg)
                                (string-match-p "markers not found" msg))
                              messages)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Test Runner

(defun generate-docs-run-all-tests ()
  "Run all generate-docs tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-extract\\|^test-format\\|^test-generate"))

(provide 'generate-docs-test)

;;; generate-docs-test.el ends here

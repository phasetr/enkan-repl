;;; generate-docs.el --- Generate documentation for Emacs Lisp packages -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This script generates documentation for Emacs Lisp packages, including:
;; - Public API documentation from function definitions
;; - Default template files for project setup

;;; Code:

(require 'cl-lib)

;; Add parent directory to load-path to access enkan-repl-utils
(let ((parent-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path parent-dir))

(require 'enkan-repl-utils)

;; For batch execution

(defun enkan-repl--generate-core-functions-section ()
  "Generate Core Functions section from constants file dynamically."
  ;; Load constants file to get function list with categories
  (unless (featurep 'enkan-repl-constants)
    (let* ((current-dir (file-name-directory (or load-file-name
                                                 buffer-file-name
                                                 default-directory)))
           (constants-file (expand-file-name "../enkan-repl-constants.el" current-dir)))
      (when (file-exists-p constants-file)
        (load constants-file nil t)))
    ;; Also try loading from current directory as fallback
    (unless (boundp 'enkan-repl-cheat-sheet-candidates)
      (let ((fallback-file (expand-file-name "enkan-repl-constants.el" default-directory)))
        (when (file-exists-p fallback-file)
          (load fallback-file nil t)))))

  (let ((result "** Core Functions\n\n")
        (categories-hash (make-hash-table :test 'equal))
        (category-order '("Command Palette" "File Management" "Text Sending"
                         "Session Control" "Utilities")))

    ;; Group functions by category from constants
    (dolist (func-info enkan-repl-cheat-sheet-candidates)
      (let* ((func-name (car func-info))
             (description (cdr func-info))
             (category (if (string-match "Category: \\([^\"]+\\)" description)
                          (match-string 1 description)
                        "Utilities")))
        ;; Clean up description by removing category marker
        (setq description (replace-regexp-in-string "\\s-*Category:.*$" "" description))

        ;; Map categories to Core Functions sections
        (let ((section-name (cond
                            ((string= category "Command Palette") "Command Palette")
                            ((string= category "Text Sender") "Text Sending")
                            ((string= category "Session Controller") "Session Control")
                            ((string= category "Utilities")
                             (cond
                              ((string-match "project-input-file" func-name) "File Management")
                              (t "Utilities")))
                            (t category))))
          (unless (gethash section-name categories-hash)
            (puthash section-name '() categories-hash))
          (puthash section-name
                   (cons (cons func-name description) (gethash section-name categories-hash))
                   categories-hash))))

    ;; Generate output in category order
    (dolist (category category-order)
      (when (gethash category categories-hash)
        (setq result (concat result "*** " category "\n"))

        ;; Special handling for Command Palette
        (if (string= category "Command Palette")
            (setq result (concat result
                               "- =enkan-repl-cheat-sheet= - *Interactive command browser with fuzzy search*\n"
                               "  - Browse all available functions with descriptions\n"
                               "  - Filter functions by typing partial names\n"
                               "  - Functions organized by category for easy discovery\n"
                               "  - No need to memorize function names\n\n"))
          ;; Regular handling for other categories
          (dolist (func (reverse (gethash category categories-hash)))
            (let ((func-name (car func))
                  (description (cdr func)))
              ;; Special formatting for certain functions
              (cond
               ((string= func-name "enkan-repl-send-region")
                (setq result (concat result "- =" func-name "= - Send selected text (the author most commonly used)\n")))
               ((string= func-name "enkan-repl-send-rest-of-buffer")
                (setq result (concat result "- =" func-name "= - Send from cursor to end\n")))
               ((string= func-name "enkan-repl-send-buffer")
                (setq result (concat result "- =" func-name "= - Send entire buffer\n")))
               ((string= func-name "enkan-repl-send-line")
                (setq result (concat result "- =" func-name "= - Send current line only\n")))
               ((string= func-name "enkan-repl-send-enter")
                (setq result (concat result "- =" func-name "= - Send enter key\n")))
               ((string-match "enkan-repl-send-[1-3]" func-name)
                ;; Combine 1/2/3 into single line
                (unless (string-match "enkan-repl-send-1/2/3" result)
                  (setq result (concat result "- =enkan-repl-send-1/2/3= - Send numbered choices for AI prompts\n"))))
               ((string= func-name "enkan-repl-open-project-input-file")
                (setq result (concat result "- =" func-name "= - Create/open persistent input file for current directory\n")))
               ((string= func-name "enkan-repl-setup-window-layout")
                (setq result (concat result "- =" func-name "= - Arrange windows for (the author's) optimal workflow\n")))
               (t
                (setq result (concat result "- =" func-name "= - " description "\n"))))))
          (setq result (concat result "\n")))))
    result))

(defun enkan-repl--generate-readme (output-file)
  "Generate README.org file with Core Functions section matching manual updates."
  (let* ((readme-content (if (file-exists-p output-file)
                             (with-temp-buffer
                               (insert-file-contents output-file)
                               (buffer-string))
                           ""))
         (new-section (enkan-repl--generate-core-functions-section)))
    ;; Find start and end positions for Core Functions section
    (let ((start-pos (string-match "\\*\\* Core Functions" readme-content))
          (end-pos (string-match "\\*\\* Configuration" readme-content)))
      (if (and start-pos end-pos)
          ;; Replace the Core Functions section
          (let ((before (substring readme-content 0 start-pos))
                (after (substring readme-content end-pos)))
            (with-temp-file output-file
              (insert before)
              (insert new-section)
              (insert after)))
        ;; If markers not found, keep file unchanged
        (message "Core Functions section markers not found - file unchanged")))))

(defun generate-readme ()
  "Generate README.org with updated Core Functions section."
  (enkan-repl--generate-readme "README.org")
  (message "README.org generated successfully"))

(defun generate-all-docs ()
  "Generate all documentation files."
  (generate-readme)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

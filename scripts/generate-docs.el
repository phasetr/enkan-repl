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
  "Generate Core Functions section matching manual update structure."
  (concat "** Core Functions\n\n"
          "*** Command Palette\n"
          "- =enkan-repl-cheat-sheet= - *Interactive command browser with fuzzy search*\n"
          "  - Browse all available functions with descriptions\n"
          "  - Filter functions by typing partial names\n"
          "  - Functions organized by category for easy discovery\n"
          "  - No need to memorize function names\n\n"
          "*** File Management\n"
          "- =enkan-repl-open-project-input-file= - Create/open persistent input file for current directory\n\n"
          "*** Text Sending\n"
          "- =enkan-repl-send-region= - Send selected text (the author most commonly used)\n"
          "- =enkan-repl-send-rest-of-buffer= - Send from cursor to end\n"
          "- =enkan-repl-send-buffer= - Send entire buffer\n"
          "- =enkan-repl-send-line= - Send current line only\n\n"
          "*** Session Control\n"
          "- =enkan-repl-send-enter= - Send enter key\n"
          "- =enkan-repl-send-1/2/3= - Send numbered choices for AI prompts\n\n"
          "*** Utilities\n"
          "- =enkan-repl-start-eat= - Start eat terminal session in appropriate directory\n"
          "- =enkan-repl-finish-eat= - Terminate eat session and close its buffer\n"
          "- =enkan-repl-list-sessions= - Display list of active sessions with delete functionality\n"
          "- =enkan-repl-setup-window-layout= - Arrange windows for (the author's) optimal workflow\n"
          "- =enkan-repl-status= - Display connection status and diagnostics\n\n"))

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
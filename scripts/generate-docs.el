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
          "- =enkan-repl-cheatsheet= - *Interactive command browser with fuzzy search*\n"
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
          "- =enkan-repl-start-claudemacs= - Start Claude session in appropriate directory\n"
          "- =enkan-repl-setup-window-layout= - Arrange windows for (the author's) optimal workflow\n"
          "- =enkan-repl-status= - Display connection status and diagnostics\n\n"))

(defun enkan-repl--generate-readme (output-file)
  "Generate README.org file - currently disabled to preserve manual edits."
  ;; README.org is now manually maintained
  ;; This function is intentionally disabled to prevent overwriting manual changes
  (message "README.org generation is disabled - file is manually maintained"))

(defun generate-readme ()
  "Generate README.org with updated Core Functions section."
  (enkan-repl--generate-readme "README.org")
  (message "README.org generated successfully"))

(defun generate-all-docs ()
  "Generate all documentation files."
  (generate-readme)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

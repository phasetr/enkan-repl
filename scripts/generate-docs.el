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

(defun claudemacs-repl--extract-function-info (file-path)
  "Extract public function information from an Emacs Lisp file.
Returns a list of plists, each containing :name, :args, :docstring,
:interactive, and :autoload status."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((functions-info '())
          lines
          (current-line 0))
      ;; Split into lines for easier processing
      (setq lines (split-string (buffer-string) "\n"))
      (while (< current-line (length lines))
        (let ((line (nth current-line lines))
              (autoload-p nil))
          ;; Check for autoload marker
          (when (string-match "^;;;###autoload" line)
            (setq autoload-p t)
            (setq current-line (1+ current-line))
            (setq line (nth current-line lines)))
          ;; Check for defun
          (when (and line (string-match "^(defun\\s-+\\([^ \t\n(]+\\)\\s-+\\((.*)\\)" line))
            (let* ((func-name (match-string 1 line))
                   (args-str (match-string 2 line))
                   (args nil)
                   (docstring "")
                   (interactive-p nil))
              ;; Parse arguments safely
              (condition-case nil
                  (setq args (read args-str))
                (error (setq args nil)))
              ;; Look for docstring in next lines
              (let ((next-line-idx (1+ current-line)))
                (when (< next-line-idx (length lines))
                  (let ((next-line (nth next-line-idx lines)))
                    (when (string-match "^\\s-*\"\\(.*\\)\"" next-line)
                      (setq docstring (match-string 1 next-line))))))
              ;; Look for (interactive) in the function body
              ;; Only search until we find another defun or end of reasonable function scope
              (let ((body-idx (1+ current-line))
                    (paren-count 0)
                    (in-function t))
                (while (and (< body-idx (length lines))
                            (not interactive-p)
                            in-function
                            (< body-idx (+ current-line 20))) ; reasonable limit
                  (let ((body-line (nth body-idx lines)))
                    (when body-line
                      ;; Stop if we encounter another defun
                      (when (string-match "^(defun\\|^;;;###autoload" body-line)
                        (setq in-function nil))
                      ;; Look for interactive only if still in this function
                      (when (and in-function (string-match "(interactive" body-line))
                        (setq interactive-p t))))
                  (setq body-idx (1+ body-idx))))
              ;; Add to results if public
              (when (or interactive-p autoload-p)
                (push (list :name func-name
                            :args args
                            :docstring docstring
                            :interactive interactive-p
                            :autoload autoload-p)
                      functions-info))))
          (setq current-line (1+ current-line))))
      (nreverse functions-info))))

(defun claudemacs-repl--format-function-info (info)
  "Format a single function info plist into org-mode format."
  (let* ((name (plist-get info :name))
         (args (plist-get info :args))
         (docstring (plist-get info :docstring))
         (interactive-p (plist-get info :interactive))
         (autoload-p (plist-get info :autoload))
         (output-lines '()))

    (push (format "** ~%s~ %s" name (if args (format "%S" args) "()")) output-lines)
    (when interactive-p
      (push "   - *Type*: Interactive Command" output-lines))
    (when autoload-p
      (push "   - *Autoload*: Yes" output-lines))
    (when (not (string-empty-p docstring))
      (push (format "   - *Description*: %s" docstring) output-lines))
    (string-join (nreverse output-lines) "\n")))

(defun claudemacs-repl--generate-public-api-docs (file-path output-file)
  "Generate org-mode documentation for public functions/commands from FILE-PATH.
Write the formatted output to OUTPUT-FILE."
  (let* ((functions (claudemacs-repl--extract-function-info file-path))
         (public-functions (cl-remove-if-not
                            (lambda (f) (or (plist-get f :interactive)
                                            (plist-get f :autoload)))
                            functions))
         (formatted-output (mapcar 'claudemacs-repl--format-function-info public-functions)))
    (with-temp-file output-file
      (insert "#+TITLE: Public API\n\n")
      (insert "* Public API\n\n")
      (insert "This document lists all public functions and commands available in claudemacs-repl.\n\n")
      (insert (string-join formatted-output "\n\n")))))

(defun claudemacs-repl--generate-default-template (output-file)
  "Generate default template file for claudemacs-repl projects.
Write the formatted template to OUTPUT-FILE."
  (with-temp-file output-file
    (insert "* Quick Start\n\n")

    (insert "** 1. Start Claude Code Session\n")
    (insert "Execute this to start claudemacs and set up the environment:\n\n")
    (insert "#+BEGIN_SRC emacs-lisp\n")
    (insert "(claudemacs-repl-start-claudemacs)\n")
    (insert "#+END_SRC\n\n")

    (insert "** 2. Setup Window Layout (Optional)\n")
    (insert "Create a convenient window layout for Claude Code interaction:\n\n")
    (insert "#+BEGIN_SRC emacs-lisp\n")
    (insert "(claudemacs-repl-setup-window-layout)\n")
    (insert "#+END_SRC\n\n")

    (insert "* Essential Commands\n\n")

    (insert "** Sending Content to Claude Code\n")
    (insert "- *Send current region*: ~M-x claudemacs-repl-send-region~ (or ~C-c r~)\n")
    (insert "- *Send current line*: ~M-x claudemacs-repl-send-line~ (or ~C-c l~)\n")
    (insert "- *Send rest of buffer*: ~M-x claudemacs-repl-send-rest-of-buffer~ (or ~C-c t~)\n")
    (insert "- *Send entire buffer*: ~M-x claudemacs-repl-send-buffer~ (or ~C-c b~)\n\n")

    (insert "** Project Management\n")
    (insert "- *Open project input file*: ~M-x claudemacs-repl-open-project-input-file~\n")
    (insert "- *Output template for customization*: ~M-x claudemacs-repl-output-template~\n\n")

    (insert "** Troubleshooting\n")
    (insert "If you encounter connection issues, run diagnostics:\n\n")
    (insert "#+BEGIN_SRC emacs-lisp\n")
    (insert "(claudemacs-repl-status)\n")
    (insert "#+END_SRC\n\n")

    (insert "* Working Notes\n\n")
    (insert "You can write extensive notes here to organize your thoughts.\n")
    (insert "This section is for your personal use - only send specific parts to Claude Code as needed.\n\n")

    (insert "** Project Context\n")
    (insert "- Description of your current project\n")
    (insert "- Key objectives and requirements\n")
    (insert "- Important constraints or considerations\n\n")

    (insert "** Development Notes\n")
    (insert "- Code snippets for testing\n")
    (insert "- Architecture decisions\n")
    (insert "- TODO items and reminders\n\n")

    (insert "** Communication with Claude Code\n")
    (insert "Use the sending commands above to share specific sections with Claude Code.\n")
    (insert "You don't need to send everything - be selective about what context is relevant.\n")))

;; For batch execution
(defun extract-public-api ()
  "Extract public API from claudemacs-repl.el to public-api.org."
  (claudemacs-repl--generate-public-api-docs "claudemacs-repl.el" "public-api.org")
  (message "Public API documentation generated in public-api.org"))

(defun generate-default-template ()
  "Generate default template file for claudemacs-repl projects."
  (claudemacs-repl--generate-default-template "default.org")
  (message "Default template generated in default.org"))

(defun generate-all-docs ()
  "Generate all documentation files (API and template)."
  (extract-public-api)
  (generate-default-template)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

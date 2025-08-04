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
    (when (not (= (length docstring) 0))
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
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (project-root (if (string-match-p "scripts/?$" script-dir)
                          (file-name-directory (directory-file-name script-dir))
                        default-directory))
         (package-file (expand-file-name "claudemacs-repl.el" project-root))
         (functions (claudemacs-repl--extract-function-info package-file))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions)))
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

      ;; Generate commands dynamically from extracted function info
      (let ((send-functions (cl-remove-if-not
                            (lambda (f) (string-match-p "send-" (plist-get f :name)))
                            interactive-functions))
            (project-functions (cl-remove-if-not
                               (lambda (f) (string-match-p "\\(open\\|output\\|start\\|setup\\)" (plist-get f :name)))
                               interactive-functions))
            (utility-functions (cl-remove-if-not
                               (lambda (f) (string-match-p "\\(status\\|toggle\\)" (plist-get f :name)))
                               interactive-functions)))

        (when send-functions
          (insert "** Sending Content to Claude Code\n")
          (dolist (func send-functions)
            (insert (format "- *%s*: ~M-x %s~\n"
                           (replace-regexp-in-string "claudemacs-repl-" "" (plist-get func :name))
                           (plist-get func :name))))
          (insert "\n"))

        (when project-functions
          (insert "** Project Management\n")
          (dolist (func project-functions)
            (insert (format "- *%s*: ~M-x %s~\n"
                           (replace-regexp-in-string "claudemacs-repl-" "" (plist-get func :name))
                           (plist-get func :name))))
          (insert "\n"))

        (when utility-functions
          (insert "** Troubleshooting & Utilities\n")
          (dolist (func utility-functions)
            (insert (format "- *%s*: ~M-x %s~\n"
                           (replace-regexp-in-string "claudemacs-repl-" "" (plist-get func :name))
                           (plist-get func :name))))
          (insert "\n")))

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
      (insert "You don't need to send everything - be selective about what context is relevant.\n"))))

;; For batch execution
(defun extract-public-api ()
  "Extract public API from claudemacs-repl.el to public-api.org."
  (claudemacs-repl--generate-public-api-docs "claudemacs-repl.el" "public-api.org")
  (message "Public API documentation generated in public-api.org"))

(defun generate-default-template ()
  "Generate default template file for claudemacs-repl projects."
  (claudemacs-repl--generate-default-template "default.org")
  (message "Default template generated in default.org"))

(defun claudemacs-repl--generate-readme (output-file)
  "Generate README.org file with dynamically updated function lists.
Write the formatted README to OUTPUT-FILE."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (project-root (if (string-match-p "scripts/?$" script-dir)
                          (file-name-directory (directory-file-name script-dir))
                        default-directory))
         (package-file (expand-file-name "claudemacs-repl.el" project-root))
         (functions (claudemacs-repl--extract-function-info package-file))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions))
         (send-functions (cl-remove-if-not
                         (lambda (f) (string-match-p "send-" (plist-get f :name)))
                         interactive-functions)))
    (with-temp-file output-file
      ;; Static header content
      (insert "#+TITLE: claudemacs-repl\n")
      (insert "#+AUTHOR: [phasetr]\n")
      (insert "#+EMAIL: phasetr@gmail.com\n")
      (insert "#+OPTIONS: toc:2 num:nil\n\n")
      
      (insert "[[https://melpa.org/#/claudemacs-repl][file:https://melpa.org/packages/claudemacs-repl-badge.svg]]\n")
      (insert "[[https://stable.melpa.org/#/claudemacs-repl][file:https://stable.melpa.org/packages/claudemacs-repl-badge.svg]]\n")
      (insert "[[https://github.com/phasetr/claudemacs-repl/actions/workflows/ci.yml][file:https://github.com/phasetr/claudemacs-repl/actions/workflows/ci.yml/badge.svg]]\n\n")
      
      (insert "Enhanced REPL utilities for seamless Claude Code interaction through Emacs.\n\n")
      
      (insert "** Table of Contents\n")
      (insert ":PROPERTIES:\n")
      (insert ":TOC:      :include all :depth 2\n")
      (insert ":END:\n")
      (insert ":CONTENTS:\n")
      (insert "- [[#installation][Installation]]\n")
      (insert "- [[#usage][Usage]]\n")
      (insert "- [[#features][Features]]\n")
      (insert "- [[#background][Background]]\n")
      (insert "- [[#contributing][Contributing]]\n")
      (insert "- [[#license][License]]\n")
      (insert ":END:\n\n")
      
      (insert "** Installation\n\n")
      (insert "*** From MELPA\n\n")
      (insert "=claudemacs-repl= is available on [[https://melpa.org/][MELPA]].\n\n")
      (insert "#+BEGIN_SRC emacs-lisp\n")
      (insert "(use-package claudemacs-repl\n")
      (insert "  :ensure t\n")
      (insert "  :after claudemacs)\n")
      (insert "#+END_SRC\n\n")
      
      (insert "*** Manual Installation\n\n")
      (insert "#+BEGIN_SRC bash\n")
      (insert "git clone https://github.com/phasetr/claudemacs-repl.git\n")
      (insert "#+END_SRC\n\n")
      (insert "Add to your Emacs configuration:\n\n")
      (insert "#+BEGIN_SRC emacs-lisp\n")
      (insert "(add-to-list 'load-path \"/path/to/claudemacs-repl\")\n")
      (insert "(require 'claudemacs-repl)\n")
      (insert "#+END_SRC\n\n")
      
      (insert "** Usage\n\n")
      (insert "*** Quick Start\n\n")
      (insert "1. Start a Claude Code session: ~M-x claudemacs-repl-start-claudemacs~\n")
      (insert "2. Open project input file: ~M-x claudemacs-repl-open-project-input-file~\n")
      (insert "3. Write your thoughts and send them with ~M-x claudemacs-repl-send-region~\n\n")
      
      (insert "*** Workflow\n\n")
      (insert "=claudemacs-repl= creates persistent org-mode files for each project directory.\n")
      (insert "These files serve as your thinking space - write extensively, then send\n")
      (insert "only relevant parts to Claude Code using the send commands.\n\n")
      
      (insert "** Background\n\n")
      (insert "I frequently collaborate with Claude Code and found myself\n")
      (insert "keeping inspired thoughts noted for immediate use after AI work is complete.\n")
      (insert "For text work, I naturally turned to Emacs.\n")
      (insert "After trying several packages that didn't quite fit,\n")
      (insert "I decided to build my own on top of [[https://github.com/cpoile/claudemacs][claudemacs]].\n\n")
      
      (insert "** Features\n\n")
      
      ;; Dynamically generate Text Sending Capabilities
      (insert "*** Text Sending Capabilities\n")
      (dolist (func send-functions)
        (let* ((name (plist-get func :name))
               (docstring (plist-get func :docstring))
               (display-name (replace-regexp-in-string "claudemacs-repl-" "" name))
               (description (cond
                            ((string-match-p "region" name) "Send selected text to Claude")
                            ((string-match-p "buffer" name) "Send entire buffer content")
                            ((string-match-p "rest-of-buffer" name) "Send from cursor position to end")
                            ((string-match-p "line" name) "Send current line")
                            ((string-match-p "enter" name) "Send enter key for prompts")
                            ((string-match-p "send-[123]" name) "Send numbered choices for AI prompts")
                            ((string-match-p "escape" name) "Send ESC key to interrupt operations")
                            (t (or docstring "Send command")))))
          (insert (format "- =%s= - %s\n" name description))))
      (insert "- *File Path Support*: Send file paths for Claude to read directly (e.g., =~/project/config.json=)\n\n")
      
      ;; Static project management content
      (insert "*** Project Management\n")
      (insert "These features rely on the underlying claudemacs functionality,\n")
      (insert "and their constraints depend on that implementation.\n\n")
      (insert "- *Persistent Input Files*: Org-mode files with filename-encoded directory mapping\n")
      (insert "  (based on claudemacs buffer name mode detection specification as of Aug 2025)\n")
      (insert "- *Template System*: Customizable templates with default.org included\n")
      (insert "- *Directory-based Targeting*: Automatically match files to appropriate claudemacs sessions\n")
      (insert "- *Multi-project Support*: Handle multiple Claude sessions in different directories simultaneously\n\n")
      
      (insert "*** Session Management\n")
      (insert "- =claudemacs-repl-start-claudemacs= - Launch Claude session in appropriate directory\n")
      (insert "  (only wraps =claudemacs-transient-menu=)\n\n")
      
      (insert "*** Session Control\n")
      (insert "- =claudemacs-repl-send-enter= - Send enter key\n")
      (insert "- =claudemacs-repl-send-1/2/3= - Send numbered choices for AI prompts\n\n")
      
      (insert "*** Utilities\n")
      (insert "- =claudemacs-repl-start-claudemacs= - Start Claude session in appropriate directory\n")
      (insert "- =claudemacs-repl-setup-window-layout= - Arrange windows for (the author's) optimal workflow\n")
      (insert "- =claudemacs-repl-status= - Diagnostic information for troubleshooting\n")
      (insert "- =claudemacs-repl-toggle-debug-mode= - Enable/disable debug messages\n\n")
      
      (insert "** Contributing\n\n")
      (insert "See [[file:CONTRIBUTING.md][CONTRIBUTING.md]] for development setup and guidelines.\n\n")
      
      (insert "** License\n\n")
      (insert "MIT License. See [[file:LICENSE][LICENSE]] file for details.\n"))))

(defun generate-readme ()
  "Generate README.org with dynamically updated function lists."
  (claudemacs-repl--generate-readme "README.org")
  (message "README.org generated successfully"))

(defun generate-all-docs ()
  "Generate all documentation files (API, template, and README)."
  (extract-public-api)
  (generate-default-template)
  (generate-readme)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

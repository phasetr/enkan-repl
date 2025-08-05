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
              ;; Look for docstring in next lines (support multi-line)
              (let ((next-line-idx (1+ current-line))
                    (in-docstring nil)
                    (docstring-lines '()))
                (while (and (< next-line-idx (length lines))
                            (< next-line-idx (+ current-line 10))) ; reasonable limit
                  (let ((next-line (nth next-line-idx lines)))
                    (cond
                     ;; Start of docstring
                     ((and (not in-docstring) 
                           (string-match "^\\s-*\"\\(.*\\)\"\\s-*$" next-line))
                      ;; Single line docstring
                      (setq docstring (match-string 1 next-line))
                      (setq next-line-idx (length lines))) ; break
                     ((and (not in-docstring)
                           (string-match "^\\s-*\"\\(.*\\)" next-line))
                      ;; Start of multi-line docstring
                      (setq in-docstring t)
                      (push (match-string 1 next-line) docstring-lines))
                     ;; End of multi-line docstring
                     ((and in-docstring
                           (string-match "\\(.*\\)\"\\s-*$" next-line))
                      (push (match-string 1 next-line) docstring-lines)
                      (setq docstring (mapconcat 'identity (nreverse docstring-lines) " "))
                      (setq next-line-idx (length lines))) ; break
                     ;; Middle of multi-line docstring
                     (in-docstring
                      (push (string-trim next-line) docstring-lines))
                     ;; Not a docstring line, break
                     (t (setq next-line-idx (length lines)))))
                  (setq next-line-idx (1+ next-line-idx))))
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

(defun generate-default-template ()
  "Generate default template file for claudemacs-repl projects."
  (claudemacs-repl--generate-default-template "default.org")
  (message "Default template generated in default.org"))

(defun claudemacs-repl--generate-readme (output-file)
  "Generate README.org file with dynamically updated function lists.
Only updates the Text Sending Capabilities section, preserving other content."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (project-root (if (string-match-p "scripts/?$" script-dir)
                          (file-name-directory (directory-file-name script-dir))
                        default-directory))
         (package-file (expand-file-name "claudemacs-repl.el" project-root))
         (functions (claudemacs-repl--extract-function-info package-file))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions))
         (send-functions interactive-functions))
    
    ;; Read existing README.org content
    (let ((readme-content (if (file-exists-p output-file)
                              (with-temp-buffer
                                (insert-file-contents output-file)
                                (buffer-string))
                            ""))
          (new-section ""))
      
      ;; Generate new Functions/Commands section
      (setq new-section "** Functions/Commands\n\n")
      (setq new-section (concat new-section "We open the following functions/commands.\n\n"))
      (dolist (func send-functions)
        (let* ((name (plist-get func :name))
               (docstring (plist-get func :docstring))
               (description (or docstring "Send command")))
          (setq new-section (concat new-section (format "- =%s= - %s\n" name description)))))
      (setq new-section (concat new-section "\n"))
      
      ;; Find start and end positions
      (let ((start-pos (string-match "\\*\\* Functions/Commands" readme-content))
            (end-pos (string-match "\\*\\* Installation" readme-content)))
        (if (and start-pos end-pos)
            ;; Replace the section
            (let ((before (substring readme-content 0 start-pos))
                  (after (substring readme-content end-pos)))
              (with-temp-file output-file
                (insert before)
                (insert new-section)
                (insert after)))
          ;; If markers not found, create the file with original logic for fallback
          (with-temp-file output-file
            (insert readme-content)
            (insert "\n\n")
            (insert new-section)))))))

(defun generate-readme ()
  "Generate README.org with dynamically updated function lists."
  (claudemacs-repl--generate-readme "README.org")
  (message "README.org generated successfully"))

(defun generate-all-docs ()
  "Generate all documentation files (template and README)."
  (generate-default-template)
  (generate-readme)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

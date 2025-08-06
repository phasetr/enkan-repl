;;; claudemacs-repl-utils.el --- Utility functions for claudemacs-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for claudemacs-repl package.
;; This file provides shared functionality for documentation generation
;; and dynamic function listing that can be used by both the main package
;; and build/documentation scripts.

;;; Code:

(require 'cl-lib)

(defun claudemacs-repl-utils--extract-function-info (file-path)
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

(defun claudemacs-repl-utils--generate-function-list-flat (functions)
  "Generate flat function list in org-mode format.
FUNCTIONS should be a list of function info plists from
`claudemacs-repl-utils--extract-function-info'."
  (let ((result ""))
    (dolist (func functions)
      (let* ((name (plist-get func :name))
             (docstring (plist-get func :docstring))
             (description (if (and docstring (not (zerop (length docstring))))
                              docstring
                            "Send command")))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name description)))))
    result))

(defun claudemacs-repl-utils--get-all-public-functions (file-path)
  "Get all public functions from FILE-PATH as flat org-mode list.
Returns a string containing org-mode formatted function list."
  (let* ((functions (claudemacs-repl-utils--extract-function-info file-path))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions)))
    (claudemacs-repl-utils--generate-function-list-flat interactive-functions)))

(defun claudemacs-repl-utils--extract-category-from-docstring (docstring)
  "Extract category information from DOCSTRING.
Returns category string or nil if not found."
  (when (and docstring (stringp docstring))
    (if (string-match "Category: \\(.*\\)$" docstring)
        (match-string 1 docstring)
      nil)))

(defun claudemacs-repl-utils--get-categorized-functions (file-path)
  "Get functions organized by category from FILE-PATH.
Returns an alist where each element is (CATEGORY . FUNCTIONS-LIST).
FUNCTIONS-LIST contains plists with :name, :docstring, :category."
  (let ((functions-info (claudemacs-repl-utils--extract-function-info file-path))
        (categorized-functions '())
        (categories '("Command Palette" "Text Sender" "Session Controller" "Utilities")))
    
    ;; Extract categories from docstrings and group functions
    (dolist (func functions-info)
      (when (plist-get func :interactive)
        (let* ((docstring (plist-get func :docstring))
               (category (claudemacs-repl-utils--extract-category-from-docstring docstring))
               (clean-docstring (if category
                                   (replace-regexp-in-string "  Category: .*$" "" docstring)
                                 docstring))
               (func-info `(:name ,(plist-get func :name)
                           :docstring ,clean-docstring
                           :category ,(or category "Uncategorized"))))
          ;; Add to appropriate category list
          (let ((category-entry (assoc (or category "Uncategorized") categorized-functions)))
            (if category-entry
                (setcdr category-entry (append (cdr category-entry) (list func-info)))
              (push (cons (or category "Uncategorized") (list func-info)) categorized-functions))))))
    
    ;; Sort by predefined category order
    (let ((sorted-result '()))
      (dolist (category categories)
        (let ((category-functions (cdr (assoc category categorized-functions))))
          (when category-functions
            (push (cons category category-functions) sorted-result))))
      ;; Add any uncategorized functions at the end
      (let ((uncategorized (cdr (assoc "Uncategorized" categorized-functions))))
        (when uncategorized
          (push (cons "Uncategorized" uncategorized) sorted-result)))
      (nreverse sorted-result))))

(defun claudemacs-repl-utils--generate-org-section (category functions header-level)
  "Generate org-mode section for CATEGORY with FUNCTIONS at HEADER-LEVEL.
HEADER-LEVEL should be a number (1 for *, 2 for **, etc.)."
  (let ((header-prefix (make-string header-level ?*))
        (result ""))
    (setq result (concat result (format "%s %s\n\n" header-prefix category)))
    (dolist (func functions)
      (let ((name (plist-get func :name))
            (docstring (plist-get func :docstring)))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name (or docstring "No description"))))))
    (concat result "\n")))

(defun claudemacs-repl-utils--generate-categorized-documentation (file-path header-level)
  "Generate categorized documentation from FILE-PATH with HEADER-LEVEL.
Returns org-mode formatted string with functions organized by category."
  (let ((categorized-functions (claudemacs-repl-utils--get-categorized-functions file-path))
        (result ""))
    (dolist (category-entry categorized-functions)
      (let ((category (car category-entry))
            (functions (cdr category-entry)))
        (setq result (concat result (claudemacs-repl-utils--generate-org-section category functions header-level)))))
    result))

(provide 'claudemacs-repl-utils)

;;; claudemacs-repl-utils.el ends here
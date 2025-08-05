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

;; Add parent directory to load-path to access claudemacs-repl-utils
(let ((parent-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path parent-dir))

(require 'claudemacs-repl-utils)

;; For batch execution

(defun claudemacs-repl--generate-readme (output-file)
  "Generate README.org file with dynamically updated function lists.
Only updates the Text Sending Capabilities section, preserving other content."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (project-root (if (string-match-p "scripts/?$" script-dir)
                          (file-name-directory (directory-file-name script-dir))
                        default-directory))
         (package-file (expand-file-name "claudemacs-repl.el" project-root))
         (function-list (claudemacs-repl-utils--get-all-public-functions package-file)))
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
      (setq new-section (concat new-section function-list))
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
  "Generate all documentation files (README only)."
  (generate-readme)
  (message "All documentation generated successfully"))

;;; generate-docs.el ends here

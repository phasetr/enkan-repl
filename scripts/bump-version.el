;;; bump-version.el --- Update package version in enkan-repl.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This script updates the version string in enkan-repl.el.
;; It's designed to be called by semantic-release during the release process.

;;; Code:

(defun enkan-repl--update-version (new-version)
  "Update the version string in `enkan-repl.el` to NEW-VERSION."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (project-root (file-name-directory (directory-file-name script-dir)))
         (package-file (expand-file-name "enkan-repl.el" project-root)))
    (unless (file-exists-p package-file)
      (error "Package file not found: %s" package-file))
    (let* ((file-content (with-temp-buffer
                           (insert-file-contents package-file)
                           (buffer-string)))
           ;; Match current format: ;; Version: 0.0.0
           (updated-content (replace-regexp-in-string
                             "^;; Version: \\([0-9]+\\.[0-9]+\\.[0-9]+\\(?:-[a-zA-Z0-9\\.-]+\\)?\\)"
                             (format ";; Version: %s" new-version)
                             file-content nil t)))
      (unless (string= file-content updated-content)
        (with-temp-file package-file
          (insert updated-content))
        (message "Updated version in %s to %s" package-file new-version)
        t))))

;; Command line function for semantic-release
(defun bump-version-cli ()
  "CLI entry point for version bumping."
  (interactive)
  (let ((args (cdr command-line-args)))
    ;; Find the version argument after the script name
    (let ((version-arg nil))
      (while (and args (not version-arg))
        (let ((arg (car args)))
          (when (and (stringp arg)
                     (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" arg))
            (setq version-arg arg)))
        (setq args (cdr args)))

      (if version-arg
          (progn
            (enkan-repl--update-version version-arg)
            (message "Version updated to: %s" version-arg))
        (error "No valid version string found in arguments: %S" command-line-args)))))

;; Batch mode execution
(when noninteractive
  (bump-version-cli))

(provide 'bump-version)

;;; bump-version.el ends here

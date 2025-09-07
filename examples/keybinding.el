;;; keybinding.el --- Keybinding example for enkan-repl center file mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Example keybinding configuration for enkan-repl center file mode.
;; Copy and customize this for your own configuration.

;;; Code:

(require 'enkan-repl)
(require 'window-layouts)

;; Optional integration: magit helper
;; This is provided as an example-only command so that core does not depend on magit.
;; Copy/adapt as needed if you use magit.
(declare-function magit-status "magit" (&optional directory))

(defun enkan-repl-magit (&optional pfx)
  "Open magit for selected project from enkan-repl-projects with optional PFX.
With prefix arg, select from existing enkan buffers.
Requires magit to be installed."
  (interactive "P")
  (unless (fboundp 'magit-status)
    (user-error "magit is not available. Please install magit."))
  (let ((result (enkan-repl--target-directory-info
                 (enkan-repl--ws-current-project)
                 enkan-repl-projects
                 enkan-repl-target-directories
                 "Select project for magit:"
                 #'file-directory-p
                 pfx)))
    (pcase (plist-get result :status)
      ((or 'no-project 'no-paths 'no-buffers)
       (message (plist-get result :message)))
      ('invalid
       (message "Directory does not exist: %s" (plist-get result :path)))
      ((or 'single 'selected)
       (let ((path (plist-get result :path)))
         (when (and path (file-directory-p path))
           (let ((default-directory path))
             (magit-status)))))
      ('cancelled
       (message "Selection cancelled")))))

;; Set your center file path
(setq enkan-repl-center-file "~/enkan-center.org")
(enkan-repl-global-minor-mode 1)

(setopt enkan-repl-global-minor-bindings
  (append enkan-repl-global-minor-bindings
    '(("<escape>" . enkan-repl-send-escape)
       ("C-c C-f" . enkan-repl-toggle-global-mode)
       ("C-x g"   . enkan-repl-magit)
       ("C-M-e"   . enkan-repl-send-enter)
       ("C-M-i"   . enkan-repl-send-line)
       ("C-M-1"   . enkan-repl-send-1)
       ("C-M-2"   . enkan-repl-send-2)
       ("C-M-3"   . enkan-repl-send-3)
       ("C-M-4"   . enkan-repl-send-4)
       ("C-M-5"   . enkan-repl-send-5)
       ("C-M-<return>" . enkan-repl-send-region)
       ("C-M-@"   . enkan-repl-open-project-directory)
       ("C-t"     . other-window-or-split)
       ("C-M-b"   . enkan-repl-recenter-bottom)
       ("C-M-o"   . enkan-repl-open-center-file)
       ("C-M-s"   . enkan-repl-setup)
       ("C-M-t"   . enkan-repl-teardown)
       ("C-M-w"   . enkan-repl-workspace-switch)
       ("C-M-l"   . enkan-repl-setup-current-project-layout))))

;; Refresh keymap after setting bindings
(enkan-repl--refresh-global-minor-map)

(provide 'keybinding)

;;; keybinding.el ends here

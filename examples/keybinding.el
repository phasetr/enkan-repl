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
       ("C-M-l"   . enkan-repl-setup-current-project-layout))))

;; Refresh keymap after setting bindings
(enkan-repl--refresh-global-minor-map)

(provide 'keybinding)

;;; keybinding.el ends here

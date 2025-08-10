;;; commands.el --- Custom commands for personal use -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Custom commands for personal use.

;;; Code:

(defun other-window-or-split ()
  "split it horizontally and move
if the window is not split."
  (interactive)
  (cond ((one-window-p)
          (split-window-horizontally)
          (other-window 1))
    (t
      (other-window 1))))

(provide 'commands)

;;; commands.el ends here

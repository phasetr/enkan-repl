;;; keybinding.el --- Sample keybindings for enkan-repl development -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file provides sample keybindings for enkan-repl development.
;; These bindings can be used as a base configuration and partially
;; overridden by 3pane mode.

;;; Commentary:

;; Sample keybinding configuration for enkan-repl development workflow.
;; These bindings are designed to work well with AI-assisted development.
;;
;; The keybindings are organized into categories:
;; - Window navigation
;; - Buffer operations
;; - Text sending to REPL
;; - Development utilities
;;
;; When 3pane mode is active, some of these bindings will be overridden
;; to provide 3pane-specific functionality.

;;; Code:

;;; ========================================
;;; Base Keymap Definition
;;; ========================================

(defvar enkan-base-keymap
  (let ((map (make-sparse-keymap)))
    ;; Window Navigation
    (define-key map (kbd "C-t") 'other-window-or-split)
    (define-key map (kbd "M-t") 'other-window-or-split)
    (define-key map (kdb "C-M-l") 'enkan-repl-setup-window-layout)

    ;; Text Sending (Basic REPL interaction)
    (define-key map (kbd "C-M-<return>") 'enkan-repl-send-region)
    (define-key map (kbd "C-M-i") 'enkan-repl-send-line)

    ;; Quick Actions (Will be enhanced by 3pane)
    (define-key map (kbd "<escape>") 'enkan-repl-send-escape)
    (define-key map (kbd "C-M-1") 'enkan-repl-send-1)
    (define-key map (kbd "C-M-2") 'enkan-repl-send-2)
    (define-key map (kbd "C-M-3") 'enkan-repl-send-3)
    (define-key map (kbd "C-M-b") 'enkan-repl-recenter-bottom)

    ;; Project Management
    (define-key map (kbd "C-M-s") 'enkan-repl-start-eat)
    (define-key map (kbd "C-M-s") 'enkan-repl-finish-eat)

    ;; Help and Documentation
    (define-key map (kbd "C-M-c") 'enkan-repl-cheat-sheet)

    map)
  "Base keymap for enkan-repl development.
This keymap provides standard bindings that work in any context.
Some bindings will be overridden when some mode is active.")

;;; ========================================
;;; Keymap Installation Functions
;;; ========================================

(defun enkan-install-base-keymap ()
  "Install the base keymap globally.
This provides a consistent set of keybindings for enkan-repl development."
  (interactive)
  ;; Install keybindings globally
  (dolist (binding (cdr enkan-base-keymap))
    (when (consp binding)
      (let ((key (car binding))
             (command (cdr binding)))
        (global-set-key key command))))
  (message "Enkan base keybindings installed globally."))

(defun enkan-uninstall-base-keymap ()
  "Uninstall the base keymap globally."
  (interactive)
  ;; Remove keybindings
  (dolist (binding (cdr enkan-base-keymap))
    (when (consp binding)
      (let ((key (car binding)))
        (global-unset-key key))))
  (message "Enkan base keybindings removed."))

;;; ========================================
;;; Helper Functions for Keybinding Management
;;; ========================================

(defun enkan-describe-base-keybindings ()
  "Display a description of all base keybindings."
  (interactive)
  (with-output-to-temp-buffer "*Enkan Base Keybindings*"
    (princ "Enkan-REPL Base Keybindings\n")
    (princ "============================\n\n")

    (princ "Window Navigation:\n")
    (princ "  C-t         - Switch to other window (overridden in 3pane)\n")
    (princ "  M-t         - Other window or split\n")
    (princ "  C-x t       - Toggle split direction\n\n")

    (princ "Buffer Operations:\n")
    (princ "  C-c b r     - Revert buffer\n")
    (princ "  C-c b k     - Kill current buffer\n")
    (princ "  C-c b s     - Save buffer\n")
    (princ "  C-c b S     - Save some buffers\n\n")

    (princ "Text Sending:\n")
    (princ "  C-c C-c     - Send entire buffer\n")
    (princ "  C-c C-r     - Send region\n")
    (princ "  C-c C-l     - Send current line\n")
    (princ "  C-c C-e     - Send rest of buffer\n\n")

    (princ "Quick Actions:\n")
    (princ "  ESC         - Quit (overridden in 3pane to send ESC)\n")
    (princ "  C-M-1       - Delete other windows (overridden in 3pane)\n")
    (princ "  C-M-2       - Split window below (overridden in 3pane)\n")
    (princ "  C-M-3       - Split window right (overridden in 3pane)\n\n")

    (princ "Development Utilities:\n")
    (princ "  C-c d f     - Find function definition\n")
    (princ "  C-c d v     - Find variable definition\n")
    (princ "  C-c d l     - Find library\n")
    (princ "  C-c d d     - Describe function\n")
    (princ "  C-c d k     - Describe key\n")
    (princ "  C-c d m     - Describe mode\n\n")

    (princ "File Navigation:\n")
    (princ "  C-c f f     - Find file\n")
    (princ "  C-c f r     - Recent files\n")
    (princ "  C-c f d     - Open dired\n")
    (princ "  C-c f j     - Jump to dired\n\n")

    (princ "Search and Replace:\n")
    (princ "  C-c s s     - Regexp search\n")
    (princ "  C-c s r     - Query replace regexp\n")
    (princ "  C-c s o     - Occur\n")
    (princ "  C-c s g     - Grep\n\n")

    (princ "Evaluation:\n")
    (princ "  C-c e e     - Eval last sexp\n")
    (princ "  C-c e b     - Eval buffer\n")
    (princ "  C-c e r     - Eval region\n")
    (princ "  C-c e f     - Eval defun\n\n")

    (princ "Help and Documentation:\n")
    (princ "  C-c h h     - Enkan cheat sheet\n")
    (princ "  C-c h s     - Enkan status\n")
    (princ "  C-c h d     - Toggle debug mode\n\n")

    (princ "Project Management:\n")
    (princ "  C-c p o     - Open project input file\n")
    (princ "  C-c p s     - Start eat session\n")
    (princ "  C-c p w     - Setup window layout\n\n")

    (princ "Note: Bindings marked as 'overridden in 3pane' will have\n")
    (princ "      different behavior when 3pane mode is active.\n")))

;;; ========================================
;;; Integration with 3pane Mode
;;; ========================================

(defun enkan-keybinding-3pane-compatibility-check ()
  "Check which base keybindings will be overridden by 3pane mode."
  (interactive)
  (let ((overrides '(("C-t" . "Switch between input/misc windows")
                      ("<escape>" . "Send ESC to eat buffer")
                      ("C-M-1" . "Send 1 to eat buffer")
                      ("C-M-2" . "Send 2 to eat buffer")
                      ("C-M-3" . "Send 3 to eat buffer"))))
    (with-output-to-temp-buffer "*3pane Keybinding Overrides*"
      (princ "Keybindings that will be overridden in 3pane mode:\n")
      (princ "==================================================\n\n")
      (dolist (override overrides)
        (princ (format "  %-10s -> %s\n" (car override) (cdr override)))))))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'keybinding)

;;; keybinding.el ends here

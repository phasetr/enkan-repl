;;; keybinding-constants.el --- Keybinding definitions for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file provides centralized keybinding definitions for enkan-repl.
;; All keybinding configurations should reference these constants.

;;; Commentary:

;; Centralized keybinding definitions to avoid duplication and ensure
;; consistency across all enkan-repl components.
;;
;; Structure:
;; - Each binding is defined as (KEY COMMAND DESCRIPTION CATEGORY)
;; - Categories group related bindings
;; - Mode-specific overrides are also defined here

;;; Code:

;;; ========================================
;;; Base Keybinding Definitions
;;; ========================================

(defconst enkan-keybinding-definitions
  '(;; Window Navigation
     ("C-M-l" enkan-repl-setup "Setup window layout" window-navigation)

     ;; Text Sending
     ("C-M-<return>" enkan-repl-send-region "Send region to REPL" text-sending)
     ("C-M-i" enkan-repl-send-line "Send current line to REPL" text-sending)

     ;; Quick Actions
     ("<escape>" enkan-repl-send-escape "Send ESC to REPL" quick-actions)
     ("C-M-1" enkan-repl-send-1 "Send 1 to REPL" quick-actions)
     ("C-M-2" enkan-repl-send-2 "Send 2 to REPL" quick-actions)
     ("C-M-3" enkan-repl-send-3 "Send 3 to REPL" quick-actions)
     ("C-M-b" enkan-repl-recenter-bottom "Recenter at bottom" quick-actions)

     ;; Project Management
     ("C-M-s" enkan-repl-start-eat "Start eat session" project-management)
     ("C-M-f" enkan-repl-finish-eat "Finish eat session" project-management)

     ;; Help and Documentation
     ("C-M-c" enkan-repl-cheat-sheet "Enkan cheat sheet" help))
  "Base keybinding definitions for enkan-repl.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-keybinding-categories
  '((window-navigation . "Window Navigation")
     (text-sending . "Text Sending")
     (quick-actions . "Quick Actions")
     (project-management . "Project Management")
     (help . "Help and Documentation"))
  "Alist of category symbols to display names.")

;;; ========================================
;;; Mode-specific Override Definitions
;;; ========================================

(defconst enkan-simple-3pane-keybinding-overrides
  '(("C-M-t" enkan-simple-3pane-other-window "Switch between input/misc windows" window-navigation)
     ("<escape>" enkan-simple-3pane-send-escape "Send ESC to eat buffer" quick-actions)
     ("C-M-1" enkan-simple-3pane-send-1 "Send 1 to eat buffer" quick-actions)
     ("C-M-2" enkan-simple-3pane-send-2 "Send 2 to eat buffer" quick-actions)
     ("C-M-3" enkan-simple-3pane-send-3 "Send 3 to eat buffer" quick-actions))
  "Keybinding overrides for 3pane mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-simple-3pane-command-definitions
  '((enkan-simple-3pane-setup "Setup 3-pane window layout")
    (enkan-simple-3pane-reset "Reset 3-pane layout")
    (enkan-simple-3pane-describe-keybindings "Show 3pane keybindings")
    (enkan-simple-3pane-other-window "Switch between input/misc windows")
    (enkan-simple-3pane-lock-windows "Lock input and eat windows")
    (enkan-simple-3pane-unlock-windows "Unlock input and eat windows")
    (enkan-simple-3pane-send-escape "Send ESC to eat buffer")
    (enkan-simple-3pane-send-1 "Send 1 to eat buffer")
    (enkan-simple-3pane-send-2 "Send 2 to eat buffer")
    (enkan-simple-3pane-send-3 "Send 3 to eat buffer"))
  "Command definitions for 3pane mode cheat sheet.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-dual-task-keybinding-overrides
  '(("C-M-t" enkan-dual-task-other-window "Switch between input windows" window-navigation))
  "Keybinding overrides for dual task mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-dual-task-command-definitions
  '((enkan-dual-task-setup "Setup dual task window layout")
    (enkan-dual-task-reset "Reset dual task layout")
    (enkan-dual-task-describe-layout "Show dual task layout status")
    (enkan-dual-task-other-window "Switch between input windows"))
  "Command definitions for dual task mode.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-mode-keybinding-overrides
  `((enkan-simple-3pane-mode . ,enkan-simple-3pane-keybinding-overrides)
    (enkan-dual-task-mode . ,enkan-dual-task-keybinding-overrides))
  "Alist of modes to their keybinding override definitions.")

;;; ========================================
;;; Utility Functions
;;; ========================================

(defun enkan-keybinding-get-by-category (category &optional definitions)
  "Get all keybindings in CATEGORY from DEFINITIONS.
If DEFINITIONS is nil, use `enkan-keybinding-definitions'."
  (let ((defs (or definitions enkan-keybinding-definitions)))
    (seq-filter (lambda (def)
                  (eq (nth 3 def) category))
      defs)))

(defun enkan-keybinding-get-categories (&optional definitions)
  "Get all unique categories from DEFINITIONS.
If DEFINITIONS is nil, use `enkan-keybinding-definitions'."
  (let ((defs (or definitions enkan-keybinding-definitions)))
    (seq-uniq (mapcar (lambda (def) (nth 3 def)) defs))))

(defun enkan-keybinding-make-keymap (definitions)
  "Create a keymap from DEFINITIONS."
  (let ((map (make-sparse-keymap)))
    (dolist (def definitions)
      (let ((key (nth 0 def))
             (command (nth 1 def)))
        (define-key map (kbd key) command)))
    map))

(defun enkan-keybinding-format-description (definitions)
  "Format DEFINITIONS as a human-readable string."
  (let ((categories (enkan-keybinding-get-categories definitions))
         (result ""))
    (dolist (cat categories)
      (let ((cat-name (or (cdr (assq cat enkan-keybinding-categories))
                        (symbol-name cat)))
             (bindings (enkan-keybinding-get-by-category cat definitions)))
        (setq result (concat result cat-name ":\n"))
        (dolist (binding bindings)
          (setq result (concat result
                         (format "  %-15s - %s\n"
                           (nth 0 binding)
                           (nth 2 binding)))))
        (setq result (concat result "\n"))))
    result))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'keybinding-constants)

;;; keybinding-constants.el ends here

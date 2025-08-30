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
     ("C-M-e" enkan-repl-send-enter "Send enter to REPL" text-sending)
     ;; Quick Actions
     ("<escape>" enkan-repl-send-escape "Send ESC to REPL" quick-actions)
     ("C-M-1" enkan-repl-send-1 "Send 1 to REPL" quick-actions)
     ("C-M-2" enkan-repl-send-2 "Send 2 to REPL" quick-actions)
     ("C-M-3" enkan-repl-send-3 "Send 3 to REPL" quick-actions)
     ("C-M-4" enkan-repl-send-4 "Send 4 to REPL" quick-actions)
     ("C-M-5" enkan-repl-send-5 "Send 5 to REPL" quick-actions)
     ("C-M-b" enkan-repl-recenter-bottom "Recenter at bottom" quick-actions)
     ("C-t" other-window "Move to other windows" quick-actions)
     ;; Project Management
     ("C-M-s" enkan-repl-setup "Start eat session" project-management)
     ("C-M-t" enkan-repl-teardown "Finish eat session" project-management)
     ;; Help and Documentation
     ("C-M-c" enkan-repl-cheat-sheet "Enkan cheat sheet" help))
  "Base keybinding definitions for enkan-repl.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-keybinding-categories
  '((window-navigation . "Window Navigation")
     (text-sending . "Text Sending")
     (quick-actions . "Quick Actions")
     (multi-buffer . "Multi-buffer Layout")
     (project-management . "Project Management")
     (help . "Help and Documentation"))
  "Alist of category symbols to display names.")

;;; ========================================
;;; Mode-specific Override Definitions
;;; ========================================

(defconst enkan-center-file-keybinding-overrides
  '(("<escape>" enkan-repl-send-escape "Send ESC to REPL (center)" quick-actions)
    ("C-M-e" enkan-repl-send-enter "Send enter to REPL (center)" text-sending)
    ("C-M-i" enkan-repl-send-line "Send line for the center file" window-navigation)
    ("C-M-e" enkan-repl-send-enter "Send enter for the center file" quick-actions)
    ("C-M-t" other-window "Switch between center/work/reserve windows" window-navigation)
    ("C-M-b" enkan-repl-recenter-bottom "Recenter at bottom (center)" quick-actions)
    ("C-M-o" enkan-repl-open-center-file "Open the center file" quick-actions)
    ("C-M-s" enkan-repl-setup "Auto setup sessions using project window configuration" session-management)
    ("C-M-f" enkan-repl-teardown "Terminate all registered center sessions" session-management)
    ("C-c C-f" enkan-toggle-center-file-global-mode "Toggle center file global mode" global-mode))
  "Keybinding overrides for center file multi-buffer mode.
Each entry is (KEY COMMAND DESCRIPTION CATEGORY).")

(defconst enkan-center-file-command-definitions
  '((enkan-repl-center-setup "Setup center file multi-buffer window layout")
     (enkan-repl-center-reset "Reset center file multi-buffer layout")
     (enkan-repl-send-enter "Send enter to REPL (center)")
     (enkan-repl-center-setup-2session-layout "Setup 2-session layout")
     (enkan-repl-send-escape "Send ESC to REPL (center)")
     (enkan-repl-send-1 "Send 1 to REPL (center)")
     (enkan-repl-send-2 "Send 2 to REPL (center)")
     (enkan-repl-center-register-current-session "Register current session"))
  "Command definitions for center file multi-buffer mode.
Each entry is (COMMAND DESCRIPTION).")

(defconst enkan-mode-keybinding-overrides
  `((enkan-center-file-mode . ,enkan-center-file-keybinding-overrides))
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
  (let ((map (make-sparse-keymap))
        (prefix-keys '()))
    ;; First pass: identify prefix keys
    (dolist (def definitions)
      (let ((key-string (nth 0 def)))
        (when (string-match "^\\(.+\\) [0-9]$" key-string)
          (let ((prefix (match-string 1 key-string)))
            (unless (member prefix prefix-keys)
              (push prefix prefix-keys))))))
    ;; Second pass: define prefix keys
    (dolist (prefix prefix-keys)
      (define-key map (kbd prefix) (make-sparse-keymap)))
    ;; Third pass: define all keys
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

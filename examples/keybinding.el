;;; keybinding.el --- Base keybindings for enkan-repl development -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file provides base keybindings for enkan-repl development.
;; These bindings can be overridden by various window layout modes.

;;; Commentary:

;; Base keybinding configuration for enkan-repl development workflow.
;; All keybinding definitions are centralized in keybinding-constants.el
;; to avoid duplication and ensure consistency.

;;; Code:

(require 'keybinding-constants)

;;; ========================================
;;; Base Keymap Definition
;;; ========================================

(defvar enkan-base-keymap
  (enkan-keybinding-make-keymap enkan-keybinding-definitions)
  "Base keymap for enkan-repl development.
This keymap is generated from `enkan-keybinding-definitions'.
Some bindings may be overridden when specific window layout modes are active.")

;;; ========================================
;;; Keymap Installation Functions
;;; ========================================

(defun enkan-install-base-keymap ()
  "Install the base keymap globally.
This provides a consistent set of keybindings for enkan-repl development."
  (interactive)
  ;; Install keybindings globally
  (dolist (def enkan-keybinding-definitions)
    (let ((key (kbd (nth 0 def)))
          (command (nth 1 def)))
      (global-set-key key command)))
  (message "Enkan base keybindings installed globally."))

(defun enkan-uninstall-base-keymap ()
  "Uninstall the base keymap globally."
  (interactive)
  ;; Remove keybindings
  (dolist (def enkan-keybinding-definitions)
    (let ((key (kbd (nth 0 def))))
      (global-unset-key key)))
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
    (princ (enkan-keybinding-format-description enkan-keybinding-definitions))
    (princ "Note: These are base keybindings that apply globally.\n")
    (princ "      Center file mode may override some bindings\n")
    (princ "      to provide mode-specific functionality.\n")))

;;; ========================================
;;; Mode Override Information
;;; ========================================

(defun enkan-keybinding-check-overrides (&optional mode)
  "Check which base keybindings will be overridden by MODE.
If MODE is nil, check all known modes."
  (interactive (list (when current-prefix-arg
                       (intern (completing-read "Mode: "
                                                (mapcar #'car enkan-mode-keybinding-overrides))))))
  (let ((modes (if mode
                   (list (cons mode (cdr (assq mode enkan-mode-keybinding-overrides))))
                 enkan-mode-keybinding-overrides)))
    (with-output-to-temp-buffer "*Keybinding Overrides*"
      (princ "Keybinding Overrides by Window Layout Modes\n")
      (princ "============================================\n\n")
      (dolist (mode-entry modes)
        (when mode-entry
          (let ((mode-name (car mode-entry))
                (overrides (cdr mode-entry)))
            (princ (format "Mode: %s\n" mode-name))
            (princ "----------------------------------------\n")
            (when overrides
              (princ (enkan-keybinding-format-description overrides)))
            (princ "\n"))))
      (princ "Note: These overrides only apply when the respective mode is active.\n"))))

(defun enkan-keybinding-show-conflicts ()
  "Show potential conflicts between base and override keybindings."
  (interactive)
  (let ((base-keys (mapcar #'car enkan-keybinding-definitions))
        (conflicts '()))
    (dolist (mode-entry enkan-mode-keybinding-overrides)
      (let ((mode (car mode-entry))
            (overrides (cdr mode-entry)))
        (dolist (override overrides)
          (let ((key (car override)))
            (when (member key base-keys)
              (push (list mode key
                          (nth 2 (assoc key enkan-keybinding-definitions))
                          (nth 2 override))
                    conflicts))))))
    (with-output-to-temp-buffer "*Keybinding Conflicts*"
      (princ "Keybinding Conflicts (Base vs Mode Overrides)\n")
      (princ "==============================================\n\n")
      (if conflicts
          (dolist (conflict (nreverse conflicts))
            (princ (format "Mode: %s\n" (nth 0 conflict)))
            (princ (format "  Key: %s\n" (nth 1 conflict)))
            (princ (format "    Base:     %s\n" (nth 2 conflict)))
            (princ (format "    Override: %s\n\n" (nth 3 conflict))))
        (princ "No conflicts found.\n")))))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'keybinding)

;;; keybinding.el ends here

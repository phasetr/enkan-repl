;;; simple-3-pane-window-layout.el --- Simple 3-pane window layout for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file is an example configuration for enkan-repl.
;; It provides a 3-pane window layout optimized for elisp development
;; with Claude Code or other AI assistants.

;;; Commentary:

;; This example provides a 3-pane window layout configuration:
;; - Left: Input file buffer (locked, for composing prompts)
;; - Right-top: Miscellaneous files buffer (for viewing docs/tests)
;; - Right-bottom: eat terminal buffer (for AI interaction)
;;
;; Features:
;; - Quick switching between input and misc windows only
;; - eat buffer stays in semi-char mode without switching to it
;; - Send escape/choices to eat from any buffer
;; - Smaller text in eat buffer for better overview
;;
;; Usage:
;; 1. Load this file:
;;    (load-file "/path/to/enkan-repl/examples/simple-3-pane-window-layout.el")
;;
;; 2. Initialize layout:
;;    M-x enkan-3pane-setup
;;
;; 3. Use keybindings:
;;    C-c 3 s - Setup 3-pane layout
;;    C-c 3 o - Switch between input/misc windows
;;    C-c 3 l - Lock input buffer
;;    C-c 3 u - Unlock input buffer
;;    C-c 3 e - Send ESC to eat
;;    C-c 3 1/2/3 - Send choice to eat

;;; Code:

(require 'enkan-repl)

;;; Variables

(defvar enkan-simple-3pane-input-buffer nil
  "Buffer for input file (prompts).")

(defvar enkan-simple-3pane-input-left-up-window nil
  "Window for input file.")

(defvar enkan-simple-3pane-misc-left-down-window nil
  "Window for miscellaneous files.")

(defvar enkan-simple-3pane-eat-right-full-window nil
  "Window for eat terminal.")

(defcustom enkan-simple-3pane-eat-text-scale 0.85
  "Text scale adjustment for eat buffer.
Negative value makes text smaller."
  :type 'integer
  :group 'enkan-repl)

(defcustom enkan-simple-3pane-input-width-ratio 0.6
  "Width ratio for input window (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-simple-3pane-input-height-ratio 0.55
  "Height ratio for misc window in left pane (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

;;; Layout Management

(defun enkan-simple-3pane-setup ()
  "Setup 3-pane window layout for elisp development."
  (interactive)
  ;; set up the window layout
  (enkan-simple-3pane-window-setup)

  ;; Enable the minor mode for keybindings
  (unless enkan-simple-3pane-mode
    (enkan-simple-3pane-mode 1))
  ;; Setup display-buffer rules for misc window
  (enkan-simple-3pane-setup-display-buffer-rules)
  (message "3-pane layout initialized. Use C-t to switch windows."))

;;; Window Management

(defun enkan-simple-3pane-window-setup ()
  "Set up fundamental 3-pane window layout"
  ;; Save current buffer as input buffer
  (setq enkan-simple-3pane-input-buffer (current-buffer))
  ;; Delete other windows to start fresh
  (delete-other-windows)

  ;; Setup left window (input)
  (setq enkan-simple-3pane-input-left-up-window (selected-window))

  ;; Create right pane
  (let ((right-width (floor (* (window-width)
                              (- 1 enkan-simple-3pane-input-width-ratio)))))
    (split-window-horizontally (- right-width)))

  ;; Still in left-up window, split vertically
  ;; Create bottom window for misc
  (let ((top-height (floor (* (window-height)
                               enkan-simple-3pane-input-height-ratio))))
    (setq enkan-simple-3pane-misc-left-down-window (split-window-vertically top-height)))
  ;; Setup eat buffer in right window
  (other-window 2)
  (setq enkan-simple-3pane-eat-right-full-window (selected-window))

  ;; Get or create enkan-repl buffer for current file's directory
  (let* ((input-dir (file-name-directory (or (buffer-file-name enkan-simple-3pane-input-buffer)
                                              default-directory)))
         (enkan-buffer-name (concat "*enkan:" input-dir "*"))
         (existing-buffer (get-buffer enkan-buffer-name)))
    (if (and existing-buffer (buffer-live-p existing-buffer))
        (switch-to-buffer existing-buffer)
      ;; Create new eat session with appropriate name
      (eat)
      (rename-buffer enkan-buffer-name t)))
  ;; Adjust text scale in eat buffer only
  ;; Ensure this is buffer-local
  (with-current-buffer (current-buffer)
    (setq-local face-remapping-alist nil)
    (face-remap-add-relative 'default :height enkan-simple-3pane-eat-text-scale))
  ;; Return to input window
  (select-window enkan-simple-3pane-input-left-up-window)
  ;; Lock input buffer
  (enkan-simple-3pane-lock-input-buffer))

;;; Configuration Utilities

(defun enkan-simple-3pane-other-window ()
  "Switch between input and misc windows only.
Avoids switching to eat window."
  (interactive)
  (cond
    ;; From input window -> go to misc
    ((and enkan-simple-3pane-input-left-up-window
       (eq (selected-window) enkan-simple-3pane-input-left-up-window))
      (when (window-live-p enkan-simple-3pane-misc-left-down-window)
        (select-window enkan-simple-3pane-misc-left-down-window)))
    ;; From anywhere else -> go to input
    (t
      (when (window-live-p enkan-simple-3pane-input-left-up-window)
        (select-window enkan-simple-3pane-input-left-up-window)))))

(defun enkan-simple-3pane-lock-input-buffer ()
  "Lock input buffer to prevent opening other files in it."
  (interactive)
  (when (and enkan-simple-3pane-input-buffer
          (buffer-live-p enkan-simple-3pane-input-buffer))
    (let ((window (get-buffer-window enkan-simple-3pane-input-buffer)))
      (when window
        (set-window-dedicated-p window t)
        (message "Input buffer locked.")))))

(defun enkan-simple-3pane-unlock-input-buffer ()
  "Unlock input buffer to allow opening other files."
  (interactive)
  (when (and enkan-simple-3pane-input-buffer
          (buffer-live-p enkan-simple-3pane-input-buffer))
    (let ((window (get-buffer-window enkan-simple-3pane-input-buffer)))
      (when window
        (set-window-dedicated-p window nil)
        (message "Input buffer unlocked.")))))

;;; Remote eat Control

(defun enkan-simple-3pane-send-escape ()
  "Send ESC to eat buffer from any window."
  (interactive)
  (if (and enkan-simple-3pane-eat-right-full-window
        (window-live-p enkan-simple-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-simple-3pane-eat-right-full-window)
      (enkan-repl-send-escape)
      (message "Sent ESC to eat buffer"))
    (message "No eat window found. Run M-x enkan-simple-3pane-setup first.")))

(defun enkan-simple-3pane-send-1 ()
  "Send choice 1 to eat buffer from any window."
  (interactive)
  (if (and enkan-simple-3pane-eat-right-full-window
        (window-live-p enkan-simple-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-simple-3pane-eat-right-full-window)
      (enkan-repl-send-1)
      (message "Sent 1 to eat buffer"))
    (message "No eat window found. Run M-x enkan-simple-3pane-setup first.")))

(defun enkan-simple-3pane-send-2 ()
  "Send choice 2 to eat buffer from any window."
  (interactive)
  (if (and enkan-simple-3pane-eat-right-full-window
        (window-live-p enkan-simple-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-simple-3pane-eat-right-full-window)
      (enkan-repl-send-2)
      (message "Sent 2 to eat buffer"))
    (message "No eat window found. Run M-x enkan-simple-3pane-setup first.")))

(defun enkan-simple-3pane-send-3 ()
  "Send choice 3 to eat buffer from any window."
  (interactive)
  (if (and enkan-simple-3pane-eat-right-full-window
        (window-live-p enkan-simple-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-simple-3pane-eat-right-full-window)
      (enkan-repl-send-3)
      (message "Sent 3 to eat buffer"))
    (message "No eat window found. Run M-x enkan-simple-3pane-setup first.")))

;;; Helper Functions

(defun enkan-simple-3pane-setup-display-buffer-rules ()
  "Setup display-buffer rules to force misc buffers to left-down window."
  ;; Override the default display-buffer function
  (advice-add 'display-buffer :around #'enkan-simple-3pane-display-buffer-advice))

(defun enkan-simple-3pane-display-buffer-advice (orig-fun buffer &rest args)
  "Advice to redirect buffers to misc window when appropriate."
  (let ((buffer-name (buffer-name (get-buffer buffer))))
    (cond
     ;; Don't redirect eat buffers
     ((string-match-p "^\\*enkan:" buffer-name)
      (apply orig-fun buffer args))
     ;; Don't redirect buffers that are already displayed
     ((get-buffer-window buffer)
      (apply orig-fun buffer args))
     ;; Redirect everything else to misc window if it exists
     ((and enkan-simple-3pane-misc-left-down-window
           (window-live-p enkan-simple-3pane-misc-left-down-window)
           ;; But not if we're in the input window editing a file
           (not (and (eq (selected-window) enkan-simple-3pane-input-left-up-window)
                     (buffer-file-name buffer))))
      (set-window-buffer enkan-simple-3pane-misc-left-down-window buffer)
      (select-window enkan-simple-3pane-misc-left-down-window)
      enkan-simple-3pane-misc-left-down-window)
     ;; Default behavior
     (t (apply orig-fun buffer args)))))

(defun enkan-simple-3pane-reset ()
  "Reset all 3-pane related variables."
  (interactive)
  (setq enkan-simple-3pane-input-buffer nil
    enkan-simple-3pane-input-left-up-window nil
    enkan-simple-3pane-misc-left-down-window nil
    enkan-simple-3pane-eat-right-full-window nil)
  ;; Remove display-buffer advice
  (advice-remove 'display-buffer #'enkan-simple-3pane-display-buffer-advice)
  (message "3-pane layout reset."))

;;; Keybindings (optional - users can customize)

(defvar enkan-simple-3pane-mode-map nil
  "Keymap for 3-pane layout commands.")

(setq enkan-simple-3pane-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c 3 s") 'enkan-simple-3pane-setup)
        (define-key map (kbd "C-t") 'enkan-simple-3pane-other-window)
        (define-key map (kbd "M-t") 'other-window-or-split)
        (define-key map (kbd "C-c 3 l") 'enkan-simple-3pane-lock-input-buffer)
        (define-key map (kbd "C-c 3 u") 'enkan-simple-3pane-unlock-input-buffer)
        (define-key map (kbd "C-c 3 e") 'enkan-simple-3pane-send-escape)
        (define-key map (kbd "C-c 3 1") 'enkan-simple-3pane-send-1)
        (define-key map (kbd "C-c 3 2") 'enkan-simple-3pane-send-2)
        (define-key map (kbd "C-c 3 3") 'enkan-simple-3pane-send-3)
        (define-key map (kbd "C-c 3 r") 'enkan-simple-3pane-reset)
        map))

(define-minor-mode enkan-simple-3pane-mode
  "Minor mode for 3-pane window layout with enkan-repl."
  :lighter " 3pane"
  :keymap enkan-simple-3pane-mode-map  ; This is critical - must specify the keymap
  :global t
  (if enkan-simple-3pane-mode
    (message "enkan-simple-3pane-mode enabled. C-t to switch windows.")
    (message "enkan-simple-3pane-mode disabled.")))

;;; Provide

(provide 'simple-3-pane-window-layout)

;;; simple-3-pane-window-layout.el ends here

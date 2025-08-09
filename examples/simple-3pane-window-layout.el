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

(defvar enkan-3pane-input-buffer nil
  "Buffer for input file (prompts).")

(defvar enkan-3pane-input-left-up-window nil
  "Window for input file.")

(defvar enkan-3pane-misc-left-down-window nil
  "Window for miscellaneous files.")

(defvar enkan-3pane-eat-right-full-window nil
  "Window for eat terminal.")

(defcustom enkan-3pane-eat-text-scale -0.5
  "Text scale adjustment for eat buffer.
Negative value makes text smaller."
  :type 'integer
  :group 'enkan-repl)

(defcustom enkan-3pane-input-width-ratio 0.8
  "Width ratio for input window (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-3pane-misc-height-ratio 0.2
  "Height ratio for misc window in right pane (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

;;; Layout Management

(defun enkan-3pane-setup ()
  "Setup 3-pane window layout for elisp development."
  (interactive)
  ;; Save current buffer as input buffer
  (setq enkan-3pane-input-buffer (current-buffer))
  ;; Delete other windows to start fresh
  (delete-other-windows)

  ;; Setup left window (input)
  (setq enkan-3pane-input-left-up-window (selected-window))

  ;; Create right pane
  (let ((right-width (floor (* (window-width)
                              (- 1 enkan-3pane-input-width-ratio)))))
    (split-window-horizontally (- right-width)))

  ;; Move to right pane and split vertically
  ;; (other-window 1)
  (setq enkan-3pane-misc-left-down-window (selected-window))
  ;; Create bottom window for eat
  (let ((eat-height (floor (* (window-height)
                             (- 1 enkan-3pane-misc-height-ratio)))))
    (split-window-vertically (- eat-height)))
  ;; Setup eat buffer in bottom window
  (other-window 2)
  (setq enkan-3pane-eat-right-full-window (selected-window))

  ;; Start or switch to eat session
  (if (get-buffer "*eat*")
    (switch-to-buffer "*eat*")
    (eat))
  ;; Adjust text scale in eat buffer
  (text-scale-adjust enkan-3pane-eat-text-scale)
  ;; Return to input window
  (select-window enkan-3pane-input-left-up-window)
  ;; Lock input buffer
  (enkan-3pane-lock-input-buffer)
  (message "3-pane layout initialized. Use C-c 3 o to switch windows."))

(defun enkan-3pane-other-window ()
  "Switch between input and misc windows only.
Avoids switching to eat window."
  (interactive)
  (cond
    ;; From input window -> go to misc
    ((and enkan-3pane-input-left-up-window
       (eq (selected-window) enkan-3pane-input-left-up-window))
      (when (window-live-p enkan-3pane-misc-left-down-window)
        (select-window enkan-3pane-misc-left-down-window)))
    ;; From anywhere else -> go to input
    (t
      (when (window-live-p enkan-3pane-input-left-up-window)
        (select-window enkan-3pane-input-left-up-window)))))

(defun enkan-3pane-lock-input-buffer ()
  "Lock input buffer to prevent opening other files in it."
  (interactive)
  (when (and enkan-3pane-input-buffer
          (buffer-live-p enkan-3pane-input-buffer))
    (let ((window (get-buffer-window enkan-3pane-input-buffer)))
      (when window
        (set-window-dedicated-p window t)
        (message "Input buffer locked.")))))

(defun enkan-3pane-unlock-input-buffer ()
  "Unlock input buffer to allow opening other files."
  (interactive)
  (when (and enkan-3pane-input-buffer
          (buffer-live-p enkan-3pane-input-buffer))
    (let ((window (get-buffer-window enkan-3pane-input-buffer)))
      (when window
        (set-window-dedicated-p window nil)
        (message "Input buffer unlocked.")))))

;;; Remote eat Control

(defun enkan-3pane-send-escape ()
  "Send ESC to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-right-full-window
        (window-live-p enkan-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-3pane-eat-right-full-window)
      (enkan-repl-send-escape)
      (message "Sent ESC to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-1 ()
  "Send choice 1 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-right-full-window
        (window-live-p enkan-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-3pane-eat-right-full-window)
      (enkan-repl-send-1)
      (message "Sent 1 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-2 ()
  "Send choice 2 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-right-full-window
        (window-live-p enkan-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-3pane-eat-right-full-window)
      (enkan-repl-send-2)
      (message "Sent 2 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-3 ()
  "Send choice 3 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-right-full-window
        (window-live-p enkan-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-3pane-eat-right-full-window)
      (enkan-repl-send-3)
      (message "Sent 3 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

;;; Helper Functions

(defun enkan-3pane-reset ()
  "Reset all 3-pane related variables."
  (interactive)
  (setq enkan-3pane-input-buffer nil
    enkan-3pane-input-left-up-window nil
    enkan-3pane-misc-left-down-window nil
    enkan-3pane-eat-right-full-window nil)
  (message "3-pane layout reset."))

(defun enkan-3pane-status ()
  "Show status of 3-pane layout."
  (interactive)
  (message "3-pane status: Input:%s Misc:%s Eat:%s"
    (if (and enkan-3pane-input-left-up-window
          (window-live-p enkan-3pane-input-left-up-window))
      "active" "inactive")
    (if (and enkan-3pane-misc-left-down-window
          (window-live-p enkan-3pane-misc-left-down-window))
      "active" "inactive")
    (if (and enkan-3pane-eat-right-full-window
          (window-live-p enkan-3pane-eat-right-full-window))
      "active" "inactive")))

;;; Keybindings (optional - users can customize)

(defvar enkan-3pane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c 3 s") 'enkan-3pane-setup)
    (define-key map (kbd "C-c 3 o") 'enkan-3pane-other-window)
    (define-key map (kbd "C-c 3 l") 'enkan-3pane-lock-input-buffer)
    (define-key map (kbd "C-c 3 u") 'enkan-3pane-unlock-input-buffer)
    (define-key map (kbd "C-c 3 e") 'enkan-3pane-send-escape)
    (define-key map (kbd "C-c 3 1") 'enkan-3pane-send-1)
    (define-key map (kbd "C-c 3 2") 'enkan-3pane-send-2)
    (define-key map (kbd "C-c 3 3") 'enkan-3pane-send-3)
    (define-key map (kbd "C-c 3 r") 'enkan-3pane-reset)
    (define-key map (kbd "C-c 3 ?") 'enkan-3pane-status)
    map)
  "Keymap for 3-pane layout commands.")

(define-minor-mode enkan-3pane-mode
  "Minor mode for 3-pane window layout with enkan-repl."
  :lighter " 3pane"
  :keymap enkan-3pane-mode-map
  :global t
  (when enkan-3pane-mode
    (message "enkan-3pane-mode enabled. C-c 3 s to setup layout.")))

;;; Provide

(provide 'simple-3-pane-window-layout)

;;; simple-3-pane-window-layout.el ends here

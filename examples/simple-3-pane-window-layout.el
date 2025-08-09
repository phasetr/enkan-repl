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
;; - Left-top (60%): Input file buffer (locked, for composing prompts)
;; - Left-bottom (40%): Miscellaneous files buffer (for viewing docs/tests)
;; - Right (full height): eat terminal buffer (for AI interaction)
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

(defvar enkan-3pane-input-window nil
  "Window for input file.")

(defvar enkan-3pane-misc-window nil
  "Window for miscellaneous files.")

(defvar enkan-3pane-eat-window nil
  "Window for eat terminal.")

(defcustom enkan-3pane-eat-text-scale -1
  "Text scale adjustment for eat buffer.
Negative value makes text smaller."
  :type 'integer
  :group 'enkan-repl)

(defcustom enkan-3pane-left-width-ratio 0.5
  "Width ratio for left pane (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-3pane-input-height-ratio 0.6
  "Height ratio for input window in left pane (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

;;; Layout Management

(defun enkan-3pane-setup ()
  "Setup 3-pane window layout for elisp development.
Left-top: Input buffer (60%)
Left-bottom: Misc files (40%)
Right: eat terminal (full height)"
  (interactive)
  ;; Save current buffer as input buffer
  (setq enkan-3pane-input-buffer (current-buffer))
  
  ;; Delete other windows to start fresh
  (delete-other-windows)
  
  ;; Setup left-top window (input)
  (setq enkan-3pane-input-window (selected-window))
  
  ;; Create right pane for eat (50% width)
  (let ((right-width (floor (* (window-width) 
                                (- 1 enkan-3pane-left-width-ratio)))))
    (split-window-horizontally (- right-width)))
  
  ;; Setup eat in right window
  (other-window 1)
  (setq enkan-3pane-eat-window (selected-window))
  
  ;; Start or switch to eat session
  (if (get-buffer "*eat*")
      (switch-to-buffer "*eat*")
    (eat))
  
  ;; Adjust text scale in eat buffer
  (text-scale-adjust enkan-3pane-eat-text-scale)
  
  ;; Go back to left pane and split vertically
  (select-window enkan-3pane-input-window)
  
  ;; Create bottom window for misc files (40% of left pane)
  ;; split-window-vertically takes the size of the top window
  (let ((input-height (floor (* (window-height) 
                                 enkan-3pane-input-height-ratio))))
    (split-window-vertically input-height))
  
  ;; Setup misc window
  (other-window 1)
  (setq enkan-3pane-misc-window (selected-window))
  
  ;; Return to input window
  (select-window enkan-3pane-input-window)
  
  ;; Lock input buffer
  (enkan-3pane-lock-input-buffer)
  
  (message "3-pane layout initialized. Use C-c 3 o to switch windows."))

(defun enkan-3pane-other-window ()
  "Switch between input and misc windows only.
Avoids switching to eat window."
  (interactive)
  (cond
   ;; From input window -> go to misc
   ((and enkan-3pane-input-window
         (eq (selected-window) enkan-3pane-input-window))
    (when (window-live-p enkan-3pane-misc-window)
      (select-window enkan-3pane-misc-window)))
   ;; From anywhere else -> go to input
   (t
    (when (window-live-p enkan-3pane-input-window)
      (select-window enkan-3pane-input-window)))))

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
  (if (and enkan-3pane-eat-window
           (window-live-p enkan-3pane-eat-window))
      (save-window-excursion
        (select-window enkan-3pane-eat-window)
        (enkan-repl-send-escape)
        (message "Sent ESC to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-1 ()
  "Send choice 1 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-window
           (window-live-p enkan-3pane-eat-window))
      (save-window-excursion
        (select-window enkan-3pane-eat-window)
        (enkan-repl-send-1)
        (message "Sent 1 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-2 ()
  "Send choice 2 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-window
           (window-live-p enkan-3pane-eat-window))
      (save-window-excursion
        (select-window enkan-3pane-eat-window)
        (enkan-repl-send-2)
        (message "Sent 2 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

(defun enkan-3pane-send-3 ()
  "Send choice 3 to eat buffer from any window."
  (interactive)
  (if (and enkan-3pane-eat-window
           (window-live-p enkan-3pane-eat-window))
      (save-window-excursion
        (select-window enkan-3pane-eat-window)
        (enkan-repl-send-3)
        (message "Sent 3 to eat buffer"))
    (message "No eat window found. Run M-x enkan-3pane-setup first.")))

;;; Helper Functions

(defun enkan-3pane-reset ()
  "Reset all 3-pane related variables."
  (interactive)
  (setq enkan-3pane-input-buffer nil
        enkan-3pane-input-window nil
        enkan-3pane-misc-window nil
        enkan-3pane-eat-window nil)
  (message "3-pane layout reset."))

(defun enkan-3pane-status ()
  "Show status of 3-pane layout."
  (interactive)
  (message "3-pane status: Input:%s Misc:%s Eat:%s"
           (if (and enkan-3pane-input-window 
                    (window-live-p enkan-3pane-input-window))
               "active" "inactive")
           (if (and enkan-3pane-misc-window
                    (window-live-p enkan-3pane-misc-window))
               "active" "inactive")
           (if (and enkan-3pane-eat-window
                    (window-live-p enkan-3pane-eat-window))
               "active" "inactive")))

(defun enkan-3pane-balance-windows ()
  "Rebalance windows to default ratios."
  (interactive)
  (when (and enkan-3pane-input-window
             (window-live-p enkan-3pane-input-window))
    (select-window enkan-3pane-input-window)
    ;; Resize horizontally
    (let ((total-width (frame-width))
          (target-width (floor (* (frame-width) enkan-3pane-left-width-ratio))))
      (window-resize nil (- target-width (window-width)) t))
    ;; Resize vertically
    (let ((left-height (window-height (frame-root-window)))
          (target-height (floor (* (window-height (frame-root-window))
                                   enkan-3pane-input-height-ratio))))
      (window-resize nil (- target-height (window-height)) nil)))
  (message "Windows rebalanced."))

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
    (define-key map (kbd "C-c 3 b") 'enkan-3pane-balance-windows)
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
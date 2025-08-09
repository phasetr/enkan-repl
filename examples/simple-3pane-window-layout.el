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

(defvar enkan-simple-3pane-cheat-sheet-candidates
  '(("enkan-simple-3pane-setup" . "Setup 3-pane window layout")
     ("enkan-simple-3pane-send-escape" . "Send ESC to eat buffer (Esc)")
     ("enkan-simple-3pane-send-1" . "Send 1 to eat buffer (C-M-1)")
     ("enkan-simple-3pane-send-2" . "Send 2 to eat buffer (C-M-2)")
     ("enkan-simple-3pane-send-3" . "Send 3 to eat buffer (C-M-3)"))
  "Additional commands for 3-pane layout to add to cheat sheet.
Only important actions.")

(defvar enkan-simple-3pane-input-file-path nil
  "Path to the input file determined from the current directory.
This is set automatically when enkan-simple-3pane-setup is called.")

(defvar enkan-simple-3pane-input-buffer nil
  "Buffer for input file (prompts).")

(defvar enkan-simple-3pane-input-left-up-window nil
  "Window for input file.")

(defvar enkan-simple-3pane-misc-left-down-window nil
  "Window for miscellaneous files.")

(defvar enkan-simple-3pane-eat-right-full-window nil
  "Window for eat terminal.")

(defvar enkan-simple-3pane-misc-buffer-history nil
  "History of buffers displayed in misc window.")

(defvar enkan-simple-3pane-mode-map nil
  "Keymap for 3-pane layout commands.")

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

;;; minor mode

(define-minor-mode enkan-simple-3pane-mode
  "Minor mode for 3-pane window layout with enkan-repl."
  :lighter " 3pane"
  :keymap enkan-simple-3pane-mode-map ; This is critical - must specify the keymap
  :global t
  (if enkan-simple-3pane-mode
    (message "enkan-simple-3pane-mode enabled. C-t to switch windows.")
    (message "enkan-simple-3pane-mode disabled.")))

;;; Layout Management

(defun enkan-simple-3pane-setup ()
  "Setup 3-pane window layout for elisp development."
  (interactive)
  ;; set up the window layout
  (enkan-simple-3pane-window-setup)

  ;; Setup display-buffer rules for misc window
  (enkan-simple-3pane-setup-display-buffer-rules)

  ;; Enable the minor mode for keybindings
  (unless enkan-simple-3pane-mode
    (enkan-simple-3pane-mode 1))

  (message "3-pane layout initialized. Input file: %s"
    (file-name-nondirectory enkan-simple-3pane-input-file-path)))

;;; Window Management

(defun enkan-simple-3pane-window-setup ()
  "Set up fundamental 3-pane window layout"
  ;; Determine input file path from current directory
  (setq enkan-simple-3pane-input-file-path
    (enkan-repl--get-project-file-path default-directory))
  ;; Open or create the input file and set as input buffer
  (let ((input-file enkan-simple-3pane-input-file-path))
    ;; Create file if it doesn't exist
    (unless (file-exists-p input-file)
      (enkan-repl--create-project-input-file default-directory))
    ;; Open the file
    (setq enkan-simple-3pane-input-buffer (find-file-noselect input-file)))

  ;; Delete other windows to start fresh
  (delete-other-windows)

  ;; Switch to input buffer in current window
  (switch-to-buffer enkan-simple-3pane-input-buffer)

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
  ;; Lock input and eat windows
  (enkan-simple-3pane-lock-windows))

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

(defun enkan-simple-3pane-lock-windows ()
  "Lock input and eat windows to prevent opening other files in them."
  (interactive)
  (let ((locked-windows '()))
    ;; Lock input window
    (when (and enkan-simple-3pane-input-buffer
            (buffer-live-p enkan-simple-3pane-input-buffer))
      (let ((window (get-buffer-window enkan-simple-3pane-input-buffer)))
        (when window
          (set-window-dedicated-p window t)
          (push "input" locked-windows))))
    ;; Lock eat window
    (when (and enkan-simple-3pane-eat-right-full-window
            (window-live-p enkan-simple-3pane-eat-right-full-window))
      (set-window-dedicated-p enkan-simple-3pane-eat-right-full-window t)
      (push "eat" locked-windows))
    ;; Report what was locked
    (if locked-windows
      (message "Locked windows: %s" (string-join (nreverse locked-windows) ", "))
      (message "No windows to lock."))))

(defun enkan-simple-3pane-unlock-windows ()
  "Unlock input and eat windows to allow opening other files."
  (interactive)
  (let ((unlocked-windows '()))
    ;; Unlock input window
    (when (and enkan-simple-3pane-input-buffer
            (buffer-live-p enkan-simple-3pane-input-buffer))
      (let ((window (get-buffer-window enkan-simple-3pane-input-buffer)))
        (when window
          (set-window-dedicated-p window nil)
          (push "input" unlocked-windows))))
    ;; Unlock eat window
    (when (and enkan-simple-3pane-eat-right-full-window
            (window-live-p enkan-simple-3pane-eat-right-full-window))
      (set-window-dedicated-p enkan-simple-3pane-eat-right-full-window nil)
      (push "eat" unlocked-windows))
    ;; Report what was unlocked
    (if unlocked-windows
      (message "Unlocked windows: %s" (string-join (nreverse unlocked-windows) ", "))
      (message "No windows to unlock."))))

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
  (advice-add 'display-buffer :around #'enkan-simple-3pane-display-buffer-advice)
  ;; Prevent deletion of misc window
  (advice-add 'delete-window :around #'enkan-simple-3pane-protect-window-advice)
  (advice-add 'quit-window :around #'enkan-simple-3pane-quit-window-advice))

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
        ;; Track buffer history for misc window
        (let ((current-buffer (window-buffer enkan-simple-3pane-misc-left-down-window)))
          (unless (eq current-buffer buffer)
            (push current-buffer enkan-simple-3pane-misc-buffer-history)))
        (set-window-buffer enkan-simple-3pane-misc-left-down-window buffer)
        (select-window enkan-simple-3pane-misc-left-down-window)
        enkan-simple-3pane-misc-left-down-window)
      ;; Default behavior
      (t (apply orig-fun buffer args)))))

(defun enkan-simple-3pane-protect-window-advice (orig-fun &optional window)
  "Prevent deletion of protected windows in 3-pane layout."
  (let ((win (or window (selected-window))))
    (cond
      ;; Never allow deletion of our 3 main windows
      ((or (eq win enkan-simple-3pane-input-left-up-window)
         (eq win enkan-simple-3pane-misc-left-down-window)
         (eq win enkan-simple-3pane-eat-right-full-window))
        ;; Just switch to scratch buffer instead of deleting window
        (with-selected-window win
          (switch-to-buffer (get-buffer-create "*scratch*")))
        nil)
      ;; Allow deletion of other windows
      (t (apply orig-fun window nil)))))

(defun enkan-simple-3pane-quit-window-advice (orig-fun &optional kill window)
  "Prevent quit-window from deleting protected windows."
  (let ((win (or window (selected-window))))
    (if (or (eq win enkan-simple-3pane-input-left-up-window)
          (eq win enkan-simple-3pane-misc-left-down-window)
          (eq win enkan-simple-3pane-eat-right-full-window))
      ;; For protected windows, switch to previous buffer
      (progn
        (with-selected-window win
          (if (and (eq win enkan-simple-3pane-misc-left-down-window)
                enkan-simple-3pane-misc-buffer-history)
            ;; For misc window, use buffer history
            (let ((prev-buffer (pop enkan-simple-3pane-misc-buffer-history)))
              (while (and prev-buffer
                       (not (buffer-live-p prev-buffer))
                       enkan-simple-3pane-misc-buffer-history)
                (setq prev-buffer (pop enkan-simple-3pane-misc-buffer-history)))
              (if (and prev-buffer (buffer-live-p prev-buffer))
                (switch-to-buffer prev-buffer)
                (switch-to-buffer (other-buffer))))
            ;; For other windows, use standard other-buffer
            (switch-to-buffer (other-buffer))))
        nil)
      ;; For other windows, use original behavior
      (apply orig-fun kill window nil))))

(defun enkan-simple-3pane-reset ()
  "Reset all 3-pane related variables."
  (interactive)
  (setq enkan-simple-3pane-input-buffer nil
    enkan-simple-3pane-input-left-up-window nil
    enkan-simple-3pane-misc-left-down-window nil
    enkan-simple-3pane-eat-right-full-window nil
    enkan-simple-3pane-misc-buffer-history nil)
  ;; Remove all advices
  (advice-remove 'display-buffer #'enkan-simple-3pane-display-buffer-advice)
  (advice-remove 'delete-window #'enkan-simple-3pane-protect-window-advice)
  (advice-remove 'quit-window #'enkan-simple-3pane-quit-window-advice)
  (message "3-pane layout reset."))

;;; Keybindings (optional - users can customize)

(setq enkan-simple-3pane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") 'enkan-simple-3pane-other-window)
    (define-key map (kbd "M-t") 'other-window-or-split)
    (define-key map (kbd "Esc") 'enkan-simple-3pane-send-escape)
    (define-key map (kbd "C-M-1") 'enkan-simple-3pane-send-1)
    (define-key map (kbd "C-M-2") 'enkan-simple-3pane-send-2)
    (define-key map (kbd "C-M-3") 'enkan-simple-3pane-send-3)
    map))

;;; Cheat Sheet Extension

(defun enkan-simple-3pane-cheat-sheet-advice (orig-fun)
  "Advice to add 3-pane commands to enkan-repl cheat sheet."
  (if enkan-simple-3pane-mode
    ;; When 3pane mode is active, show combined cheat sheet
    (progn
      (unless (featurep 'enkan-repl-constants)
        (require 'enkan-repl-constants))
      (let* ((original-candidates (if (boundp 'enkan-repl-cheat-sheet-candidates)
                                    enkan-repl-cheat-sheet-candidates
                                    '()))
              ;; Combine original and 3pane candidates
              (candidates (append original-candidates
                            enkan-simple-3pane-cheat-sheet-candidates)))
        (let ((completion-extra-properties
                `(:annotation-function
                   (lambda (candidate)
                     (let ((description (alist-get candidate ',candidates nil nil #'string=)))
                       (when description
                         (format " — %s" description)))))))
          (let ((selected-command (completing-read "enkan-repl & 3pane commands: " candidates)))
            (when selected-command
              (call-interactively (intern selected-command)))))))
    ;; When 3pane mode is not active, use original function
    (funcall orig-fun)))

;; Add advice when 3pane mode is enabled
(defun enkan-simple-3pane-setup-cheat-sheet-advice ()
  "Setup advice for cheat sheet integration."
  (advice-add 'enkan-repl-cheat-sheet :around #'enkan-simple-3pane-cheat-sheet-advice))

(defun enkan-simple-3pane-remove-cheat-sheet-advice ()
  "Remove advice for cheat sheet integration."
  (advice-remove 'enkan-repl-cheat-sheet #'enkan-simple-3pane-cheat-sheet-advice))

;; Automatically add advice when mode is enabled
(add-hook 'enkan-simple-3pane-mode-hook
  (lambda ()
    (if enkan-simple-3pane-mode
      (enkan-simple-3pane-setup-cheat-sheet-advice)
      (enkan-simple-3pane-remove-cheat-sheet-advice))))

;;; Provide

(provide 'simple-3-pane-window-layout)

;;; simple-3-pane-window-layout.el ends here

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
;; - Left-top: Input file buffer (60% width, 55% height, locked)
;; - Left-bottom: Miscellaneous files buffer (60% width, 45% height)
;; - Right: eat terminal buffer (40% width, full height, locked)
;;
;; Features:
;; - Quick switching between input and misc windows (C-t)
;; - eat buffer stays in right pane without switching to it
;; - Send escape/choices to eat from any buffer
;; - Smaller text in eat buffer for better overview
;; - Window protection and buffer history management
;; - Cheat-sheet integration with enkan-repl
;;
;; Usage:
;; 1. Load this file:
;;    (load-file "/path/to/enkan-repl/examples/simple-3-pane-window-layout.el")
;;
;; 2. Initialize layout:
;;    M-x enkan-simple-3pane-setup
;;
;; 3. Use keybindings:
;;    C-t - Switch between input/misc windows
;;    Esc - Send ESC to eat
;;    C-M-1/2/3 - Send choice to eat

;;; Code:

(require 'enkan-repl)

;;; ========================================
;;; Customization Variables
;;; ========================================

(defcustom enkan-simple-3pane-eat-text-scale 0.85
  "Text scale adjustment for eat buffer.
Negative value makes text smaller."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-simple-3pane-input-width-ratio 0.6
  "Width ratio for input window (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-simple-3pane-input-height-ratio 0.55
  "Height ratio for input window in left pane (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

;;; ========================================
;;; Internal Variables
;;; ========================================

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

(defvar enkan-simple-3pane-cheat-sheet-candidates
  '(("enkan-simple-3pane-setup" . "Setup 3-pane window layout")
     ("enkan-simple-3pane-reset" . "Reset 3-pane layout")
     ("enkan-simple-3pane-other-window" . "Switch between input/misc windows (C-t)")
     ("enkan-simple-3pane-lock-windows" . "Lock input and eat windows")
     ("enkan-simple-3pane-unlock-windows" . "Unlock input and eat windows")
     ("enkan-simple-3pane-send-escape" . "Send ESC to eat buffer (Esc)")
     ("enkan-simple-3pane-send-1" . "Send 1 to eat buffer (C-M-1)")
     ("enkan-simple-3pane-send-2" . "Send 2 to eat buffer (C-M-2)")
     ("enkan-simple-3pane-send-3" . "Send 3 to eat buffer (C-M-3)"))
  "Additional commands for 3-pane layout to add to cheat sheet.")

;;; ========================================
;;; Keymap Definition
;;; ========================================

(setq enkan-simple-3pane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") 'enkan-simple-3pane-other-window)
    (define-key map (kbd "M-t") 'other-window-or-split)
    (define-key map (kbd "<escape>") 'enkan-simple-3pane-send-escape)
    (define-key map (kbd "C-M-1") 'enkan-simple-3pane-send-1)
    (define-key map (kbd "C-M-2") 'enkan-simple-3pane-send-2)
    (define-key map (kbd "C-M-3") 'enkan-simple-3pane-send-3)
    map))

;;; ========================================
;;; Minor Mode Definition
;;; ========================================

(define-minor-mode enkan-simple-3pane-mode
  "Minor mode for 3-pane window layout with enkan-repl."
  :lighter " 3pane"
  :keymap enkan-simple-3pane-mode-map
  :global t
  (if enkan-simple-3pane-mode
    (progn
      (enkan-simple-3pane-setup-cheat-sheet-advice)
      (message "enkan-simple-3pane-mode enabled. C-t to switch windows."))
    (progn
      (enkan-simple-3pane-remove-cheat-sheet-advice)
      (message "enkan-simple-3pane-mode disabled."))))

;;; ========================================
;;; Main Setup Functions
;;; ========================================

(defun enkan-simple-3pane-setup ()
  "Setup 3-pane window layout for elisp development."
  (interactive)
  ;; Enable the minor mode
  (enkan-simple-3pane-mode 1)

  ;; Setup the window layout
  (enkan-simple-3pane-window-setup)

  ;; Setup display-buffer rules for misc window
  (enkan-simple-3pane-setup-display-buffer-rules)

  (message "3-pane layout initialized. Input file: %s"
    (file-name-nondirectory enkan-simple-3pane-input-file-path)))

(defun enkan-simple-3pane-reset ()
  "Reset all 3-pane related variables and remove advices."
  (interactive)
  ;; Clear variables
  (setq enkan-simple-3pane-input-buffer nil
    enkan-simple-3pane-input-file-path nil
    enkan-simple-3pane-input-left-up-window nil
    enkan-simple-3pane-misc-left-down-window nil
    enkan-simple-3pane-eat-right-full-window nil
    enkan-simple-3pane-misc-buffer-history nil)

  ;; Remove all advices
  (advice-remove 'display-buffer #'enkan-simple-3pane-display-buffer-advice)
  (advice-remove 'delete-window #'enkan-simple-3pane-protect-window-advice)
  (advice-remove 'quit-window #'enkan-simple-3pane-quit-window-advice)

  ;; Disable the mode
  (enkan-simple-3pane-mode -1)

  (message "3-pane layout reset."))

;;; ========================================
;;; Window Layout Functions
;;; ========================================

(defun enkan-simple-3pane-window-setup ()
  "Set up fundamental 3-pane window layout."
  ;; Determine input file path from current directory
  (setq enkan-simple-3pane-input-file-path
    (enkan-repl--get-project-file-path default-directory))

  ;; Open or create the input file
  (let ((input-file enkan-simple-3pane-input-file-path))
    (unless (file-exists-p input-file)
      (enkan-repl--create-project-input-file default-directory))
    (setq enkan-simple-3pane-input-buffer (find-file-noselect input-file)))

  ;; Delete other windows to start fresh
  (delete-other-windows)

  ;; Switch to input buffer in current window
  (switch-to-buffer enkan-simple-3pane-input-buffer)
  (setq enkan-simple-3pane-input-left-up-window (selected-window))

  ;; Create right pane (eat window)
  (let ((right-width (floor (* (window-width)
                              (- 1 enkan-simple-3pane-input-width-ratio)))))
    (split-window-horizontally (- right-width)))

  ;; Still in left-up window, split vertically to create misc window
  (let ((top-height (floor (* (window-height)
                             enkan-simple-3pane-input-height-ratio))))
    (setq enkan-simple-3pane-misc-left-down-window
      (split-window-vertically top-height)))

  ;; Setup eat buffer in right window
  (other-window 2)
  (setq enkan-simple-3pane-eat-right-full-window (selected-window))

  ;; Get or create enkan-repl buffer
  (let* ((input-dir (file-name-directory
                      (or (buffer-file-name enkan-simple-3pane-input-buffer)
                        default-directory)))
          (enkan-buffer-name (concat "*enkan:" input-dir "*"))
          (existing-buffer (get-buffer enkan-buffer-name)))
    (if (and existing-buffer (buffer-live-p existing-buffer))
      (switch-to-buffer existing-buffer)
      ;; Use enkan-repl-start-eat to create new session
      (let ((default-directory input-dir))
        (enkan-repl-start-eat))))

  ;; Adjust text scale in eat buffer
  (with-current-buffer (current-buffer)
    (setq-local face-remapping-alist nil)
    (face-remap-add-relative 'default :height enkan-simple-3pane-eat-text-scale))

  ;; Return to input window
  (select-window enkan-simple-3pane-input-left-up-window)

  ;; Lock windows
  (enkan-simple-3pane-lock-windows))

;;; ========================================
;;; Window Navigation Functions
;;; ========================================

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

;;; ========================================
;;; Window Lock/Unlock Functions
;;; ========================================

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
    ;; Report
    (if locked-windows
      (message "Locked windows: %s"
        (string-join (nreverse locked-windows) ", "))
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
    ;; Report
    (if unlocked-windows
      (message "Unlocked windows: %s"
        (string-join (nreverse unlocked-windows) ", "))
      (message "No windows to unlock."))))

;;; ========================================
;;; Remote eat Control Functions
;;; ========================================

(defun enkan-simple-3pane-send-escape ()
  "Send ESC to eat buffer from any window."
  (interactive)
  (enkan-simple-3pane--send-to-eat
    'enkan-repl-send-escape "ESC"))

(defun enkan-simple-3pane-send-1 ()
  "Send choice 1 to eat buffer from any window."
  (interactive)
  (enkan-simple-3pane--send-to-eat
    'enkan-repl-send-1 "1"))

(defun enkan-simple-3pane-send-2 ()
  "Send choice 2 to eat buffer from any window."
  (interactive)
  (enkan-simple-3pane--send-to-eat
    'enkan-repl-send-2 "2"))

(defun enkan-simple-3pane-send-3 ()
  "Send choice 3 to eat buffer from any window."
  (interactive)
  (enkan-simple-3pane--send-to-eat
    'enkan-repl-send-3 "3"))

(defun enkan-simple-3pane--send-to-eat (func msg)
  "Helper function to send commands to eat buffer.
FUNC is the function to call, MSG is the message to display."
  (if (and enkan-simple-3pane-eat-right-full-window
        (window-live-p enkan-simple-3pane-eat-right-full-window))
    (save-window-excursion
      (select-window enkan-simple-3pane-eat-right-full-window)
      (funcall func)
      (message "Sent %s to eat buffer" msg))
    (message "No eat window found. Run M-x enkan-simple-3pane-setup first.")))

;;; ========================================
;;; Display Buffer Management
;;; ========================================

(defun enkan-simple-3pane-setup-display-buffer-rules ()
  "Setup display-buffer rules to force misc buffers to left-down window."
  (advice-add 'display-buffer :around #'enkan-simple-3pane-display-buffer-advice)
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
        (enkan-simple-3pane--track-buffer-history buffer)
        (set-window-buffer enkan-simple-3pane-misc-left-down-window buffer)
        (select-window enkan-simple-3pane-misc-left-down-window)
        enkan-simple-3pane-misc-left-down-window)
      ;; Default behavior
      (t (apply orig-fun buffer args)))))

(defun enkan-simple-3pane--track-buffer-history (new-buffer)
  "Track buffer history for the misc window.
NEW-BUFFER is the buffer about to be displayed."
  (let ((current-buffer (window-buffer enkan-simple-3pane-misc-left-down-window)))
    (when (and current-buffer
            (not (eq current-buffer new-buffer))
            ;; Only track non-temporary buffers
            (not (string-match-p "^\\*magit" (buffer-name current-buffer)))
            (not (string-match-p "^COMMIT_EDITMSG" (buffer-name current-buffer)))
            ;; Track file buffers and persistent buffers
            (or (buffer-file-name current-buffer)
              (string-match-p "^\\*scratch\\*$" (buffer-name current-buffer))
              (string-match-p "^\\*Messages\\*$" (buffer-name current-buffer))))
      (push current-buffer enkan-simple-3pane-misc-buffer-history)
      ;; Keep history limited
      (when (> (length enkan-simple-3pane-misc-buffer-history) 10)
        (setq enkan-simple-3pane-misc-buffer-history
          (butlast enkan-simple-3pane-misc-buffer-history))))))

(defun enkan-simple-3pane-protect-window-advice (orig-fun &optional window)
  "Prevent deletion of protected windows in 3-pane layout."
  (let ((win (or window (selected-window))))
    (cond
      ;; Never allow deletion of our 3 main windows
      ((or (eq win enkan-simple-3pane-input-left-up-window)
         (eq win enkan-simple-3pane-misc-left-down-window)
         (eq win enkan-simple-3pane-eat-right-full-window))
        ;; Switch to scratch buffer instead of deleting window
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
            (enkan-simple-3pane--restore-previous-buffer)
            ;; For other windows, use standard other-buffer
            (switch-to-buffer (other-buffer))))
        nil)
      ;; For other windows, use original behavior
      (apply orig-fun kill window nil))))

(defun enkan-simple-3pane--restore-previous-buffer ()
  "Restore the previous buffer from history in the misc window."
  (let ((prev-buffer (pop enkan-simple-3pane-misc-buffer-history)))
    (while (and prev-buffer
             (not (buffer-live-p prev-buffer))
             enkan-simple-3pane-misc-buffer-history)
      (setq prev-buffer (pop enkan-simple-3pane-misc-buffer-history)))
    (if (and prev-buffer (buffer-live-p prev-buffer))
      (switch-to-buffer prev-buffer)
      (switch-to-buffer (other-buffer)))))

;;; ========================================
;;; Cheat Sheet Integration
;;; ========================================

(defun enkan-simple-3pane-cheat-sheet-advice (orig-fun)
  "Advice to add 3-pane commands to enkan-repl cheat sheet."
  (if enkan-simple-3pane-mode
    ;; When 3pane mode is active, show combined cheat sheet
    (progn
      (unless (featurep 'enkan-repl-constants)
        (require 'enkan-repl-constants nil t))
      (let* ((original-candidates
               (if (boundp 'enkan-repl-cheat-sheet-candidates)
                 enkan-repl-cheat-sheet-candidates
                 '()))
              ;; Combine original and 3pane candidates
              (candidates (append original-candidates
                            enkan-simple-3pane-cheat-sheet-candidates)))
        (let ((completion-extra-properties
                `(:annotation-function
                   (lambda (candidate)
                     (let ((description (alist-get candidate ',candidates
                                          nil nil #'string=)))
                       (when description
                         (format " — %s" description)))))))
          (let ((selected-command
                  (completing-read "enkan-repl & 3pane commands: " candidates)))
            (when selected-command
              (call-interactively (intern selected-command)))))))
    ;; When 3pane mode is not active, use original function
    (funcall orig-fun)))

(defun enkan-simple-3pane-setup-cheat-sheet-advice ()
  "Setup advice for cheat sheet integration."
  (advice-add 'enkan-repl-cheat-sheet :around
    #'enkan-simple-3pane-cheat-sheet-advice))

(defun enkan-simple-3pane-remove-cheat-sheet-advice ()
  "Remove advice for cheat sheet integration."
  (advice-remove 'enkan-repl-cheat-sheet
    #'enkan-simple-3pane-cheat-sheet-advice))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'simple-3-pane-window-layout)

;;; simple-3-pane-window-layout.el ends here

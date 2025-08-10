;;; dual-task-window-layout.el --- Dual task window layout for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;; This file is an example configuration for enkan-repl.
;; It provides a dual-task window layout for managing two enkan-repl sessions simultaneously.

;;; Commentary:

;; This example provides a dual-task window layout configuration:
;; - Left: Two input file buffers vertically split (main 65%, sub 35%)
;; - Middle: Main enkan-eat buffer (40% width)
;; - Right: Sub enkan-eat buffer (30% width)
;;
;; Window width ratio: 3:4:3 (input:main-eat:sub-eat)
;;
;; Features:
;; - Quick switching between input files only (C-t)
;; - Automatically selects two enkan-eat sessions
;; - Main task for active work, sub task for background processing
;; - Cheat-sheet integration with enkan-repl
;;
;; Usage:
;; 1. Start at least 2 enkan-repl sessions:
;;    M-x enkan-repl-start-eat (repeat for each session)
;;
;; 2. Load this file:
;;    (load-file "/path/to/enkan-repl/examples/dual-task-window-layout.el")
;;
;; 3. Initialize layout:
;;    M-x enkan-dual-task-setup
;;
;; 4. Use keybindings:
;;    C-t - Switch between input windows

;;; Code:

(require 'enkan-repl)

;; Load keybinding definitions and base keymap
(let ((constants-file (expand-file-name "keybinding-constants.el"
                        (file-name-directory load-file-name)))
       (keybinding-file (expand-file-name "keybinding.el"
                          (file-name-directory load-file-name))))
  (when (file-exists-p constants-file)
    (load constants-file))
  (when (file-exists-p keybinding-file)
    (load keybinding-file)
    ;; Install base keybindings as default
    (when (fboundp 'enkan-install-base-keymap)
      (enkan-install-base-keymap))))

;;; ========================================
;;; Customization Variables
;;; ========================================

(defcustom enkan-dual-task-input-column-ratio 0.4
  "Width ratio for input column (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-dual-task-main-eat-ratio 0.4
  "Width ratio for main eat window (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

(defcustom enkan-dual-task-main-input-ratio 0.55
  "Height ratio for main input window (0.0 to 1.0)."
  :type 'float
  :group 'enkan-repl)

;;; ========================================
;;; Internal Variables
;;; ========================================

(defvar enkan-dual-task-main-input-buffer nil
  "Buffer for main task input file.")

(defvar enkan-dual-task-sub-input-buffer nil
  "Buffer for sub task input file.")

(defvar enkan-dual-task-main-eat-buffer nil
  "Buffer for main task eat session.")

(defvar enkan-dual-task-sub-eat-buffer nil
  "Buffer for sub task eat session.")

(defvar enkan-dual-task-main-input-window nil
  "Window for main task input file.")

(defvar enkan-dual-task-sub-input-window nil
  "Window for sub task input file.")

(defvar enkan-dual-task-main-eat-window nil
  "Window for main task eat session.")

(defvar enkan-dual-task-sub-eat-window nil
  "Window for sub task eat session.")

(defvar enkan-dual-task-mode-map nil
  "Keymap for dual task layout commands.")

(defvar enkan-dual-task-cheat-sheet-candidates
  (if (boundp 'enkan-dual-task-command-definitions)
    ;; Use centralized definitions with keybinding hints
    (mapcar (lambda (def)
              (let* ((command (nth 0 def))
                      (description (nth 1 def))
                      (command-name (symbol-name command))
                      ;; Find keybinding if exists
                      (key (when (boundp 'enkan-dual-task-keybinding-overrides)
                             (car (seq-find (lambda (binding)
                                              (eq (nth 1 binding) command))
                                    enkan-dual-task-keybinding-overrides)))))
                (cons command-name
                  (if key
                    (format "%s (%s)" description key)
                    description))))
      enkan-dual-task-command-definitions)
    ;; Fallback definitions
    '(("enkan-dual-task-setup" . "Setup dual task window layout")
       ("enkan-dual-task-reset" . "Reset dual task layout")
       ("enkan-dual-task-describe-layout" . "Show dual task layout status")
       ("enkan-dual-task-switch-input" . "Switch between input windows (C-t)")))
  "Additional commands for dual task layout to add to cheat sheet.")

;;; ========================================
;;; Keymap Definition
;;; ========================================

(setq enkan-dual-task-mode-map
  (if (boundp 'enkan-dual-task-keybinding-overrides)
    ;; Use centralized definitions if available
    (enkan-keybinding-make-keymap enkan-dual-task-keybinding-overrides)
    ;; Fallback to manual definition
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-t") 'enkan-dual-task-switch-input)
      map)))

;;; ========================================
;;; Minor Mode Definition
;;; ========================================

(define-minor-mode enkan-dual-task-mode
  "Minor mode for dual task window layout with enkan-repl."
  :lighter " DualTask"
  :keymap enkan-dual-task-mode-map
  :global t
  (if enkan-dual-task-mode
    (progn
      (enkan-dual-task-setup-cheat-sheet-advice)
      (message "enkan-dual-task-mode enabled. C-t to switch between input files."))
    (progn
      (enkan-dual-task-remove-cheat-sheet-advice)
      (message "enkan-dual-task-mode disabled."))))

;;; ========================================
;;; Main Setup Functions
;;; ========================================

(defun enkan-dual-task-setup (&optional force-reselect)
  "Setup dual task window layout.
If FORCE-RESELECT is non-nil or no sessions are remembered,
prompt for session selection. Otherwise, reuse remembered sessions."
  (interactive "P")

  ;; Check if we have remembered sessions and they're still valid
  (if (and (not force-reselect)
        enkan-dual-task-main-eat-buffer
        enkan-dual-task-sub-eat-buffer
        (buffer-live-p enkan-dual-task-main-eat-buffer)
        (buffer-live-p enkan-dual-task-sub-eat-buffer))
    ;; Reuse existing sessions
    (progn
      (message "Reusing remembered sessions: Main: %s, Sub: %s"
        (buffer-name enkan-dual-task-main-eat-buffer)
        (buffer-name enkan-dual-task-sub-eat-buffer))

      ;; Get corresponding input files
      (setq enkan-dual-task-main-input-buffer
        (enkan-dual-task-get-input-buffer-for-eat enkan-dual-task-main-eat-buffer))
      (setq enkan-dual-task-sub-input-buffer
        (enkan-dual-task-get-input-buffer-for-eat enkan-dual-task-sub-eat-buffer)))

    ;; Select new sessions
    (let ((sessions (enkan-dual-task-select-sessions)))
      (unless sessions
        (error "Need at least 2 enkan-repl sessions. Please start them first"))

      (setq enkan-dual-task-main-eat-buffer (car sessions))
      (setq enkan-dual-task-sub-eat-buffer (cadr sessions))

      ;; Get corresponding input files
      (setq enkan-dual-task-main-input-buffer
        (enkan-dual-task-get-input-buffer-for-eat enkan-dual-task-main-eat-buffer))
      (setq enkan-dual-task-sub-input-buffer
        (enkan-dual-task-get-input-buffer-for-eat enkan-dual-task-sub-eat-buffer))))

  ;; Setup window layout
  (enkan-dual-task-window-setup)

  ;; Setup display-buffer rules (currently minimal, can be extended)
  (enkan-dual-task-setup-display-buffer-rules)

  ;; Enable mode
  (enkan-dual-task-mode 1)

  (message "Dual task layout initialized. Main: %s, Sub: %s"
    (buffer-name enkan-dual-task-main-eat-buffer)
    (buffer-name enkan-dual-task-sub-eat-buffer)))

(defun enkan-dual-task-reset ()
  "Reset all dual task related variables and remove advices."
  (interactive)
  ;; Clear variables
  (setq enkan-dual-task-main-input-buffer nil
    enkan-dual-task-sub-input-buffer nil
    enkan-dual-task-main-eat-buffer nil
    enkan-dual-task-sub-eat-buffer nil
    enkan-dual-task-main-input-window nil
    enkan-dual-task-sub-input-window nil
    enkan-dual-task-main-eat-window nil
    enkan-dual-task-sub-eat-window nil)

  ;; Remove any advices if added
  (advice-remove 'display-buffer #'enkan-dual-task-display-buffer-advice)

  ;; Disable the mode
  (enkan-dual-task-mode -1)

  (message "Dual task layout reset."))

;;; ========================================
;;; Window Layout Functions
;;; ========================================

(defun enkan-dual-task-window-setup ()
  "Set up fundamental dual task window layout."
  ;; Start fresh
  (delete-other-windows)

  ;; Switch to main input buffer
  (switch-to-buffer enkan-dual-task-main-input-buffer)
  (setq enkan-dual-task-main-input-window (selected-window))

  ;; Create three vertical columns (3:4:3 ratio)
  ;; Total width = 10 units, left=3, middle=4, right=3
  (let* ((total-width (window-width))
          (left-width (floor (* total-width enkan-dual-task-input-column-ratio)))
          (middle-width (floor (* total-width enkan-dual-task-main-eat-ratio))))

    ;; Split for middle column
    (split-window-horizontally left-width)
    (other-window 1)
    (setq enkan-dual-task-main-eat-window (selected-window))

    ;; Split for right column
    (split-window-horizontally middle-width)
    (other-window 1)
    (setq enkan-dual-task-sub-eat-window (selected-window))

    ;; Go back to left column and split vertically (65:35)
    (select-window enkan-dual-task-main-input-window)
    (let ((main-height (floor (* (window-height) enkan-dual-task-main-input-ratio))))
      (split-window-vertically main-height))
    (other-window 1)
    (setq enkan-dual-task-sub-input-window (selected-window))

    ;; Place buffers in windows
    (select-window enkan-dual-task-main-input-window)
    (switch-to-buffer enkan-dual-task-main-input-buffer)

    (select-window enkan-dual-task-sub-input-window)
    (switch-to-buffer enkan-dual-task-sub-input-buffer)

    (select-window enkan-dual-task-main-eat-window)
    (switch-to-buffer enkan-dual-task-main-eat-buffer)

    (select-window enkan-dual-task-sub-eat-window)
    (switch-to-buffer enkan-dual-task-sub-eat-buffer)

    ;; Return to main input window
    (select-window enkan-dual-task-main-input-window)))

;; Note: Window locking (window-dedicated-p) disabled to prevent eat scrolling issues
;; Note: Face remapping removed to prevent minibuffer interaction issues

;;; ========================================
;;; Keybinding Help Functions
;;; ========================================

(defun enkan-dual-task-describe-keybindings ()
  "Display keybindings active in dual task mode."
  (interactive)
  (with-output-to-temp-buffer "*Enkan Dual Task Keybindings*"
    (princ "Enkan Dual Task Mode Keybindings\n")
    (princ "=================================\n\n")

    ;; Show base keybindings
    (princ "Base Keybindings (inherited):\n")
    (princ "------------------------------\n")
    (when (boundp 'enkan-keybinding-definitions)
      ;; Show non-overridden base bindings
      (let ((overridden-keys (when (boundp 'enkan-dual-task-keybinding-overrides)
                               (mapcar #'car enkan-dual-task-keybinding-overrides))))
        (dolist (def enkan-keybinding-definitions)
          (unless (member (car def) overridden-keys)
            (princ (format "  %-15s - %s\n" (nth 0 def) (nth 2 def)))))))
    (princ "\n")

    ;; Show dual-task-specific overrides
    (princ "Dual Task Mode Overrides:\n")
    (princ "-------------------------\n")
    (if (boundp 'enkan-dual-task-keybinding-overrides)
      (princ (enkan-keybinding-format-description enkan-dual-task-keybinding-overrides))
      ;; Fallback if constants not loaded
      (princ "  C-t         - Switch between input windows\n"))

    (princ "\nNote: Dual task mode overrides take precedence over base keybindings.\n")))

;;; ========================================
;;; Window Navigation Functions
;;; ========================================

(defun enkan-dual-task-switch-input ()
  "Switch between main and sub input windows only.
Avoids switching to eat windows."
  (interactive)
  (cond
    ;; From main input -> go to sub input
    ((and enkan-dual-task-main-input-window
       (eq (selected-window) enkan-dual-task-main-input-window))
      (when (window-live-p enkan-dual-task-sub-input-window)
        (select-window enkan-dual-task-sub-input-window)))
    ;; From sub input -> go to main input
    ((and enkan-dual-task-sub-input-window
       (eq (selected-window) enkan-dual-task-sub-input-window))
      (when (window-live-p enkan-dual-task-main-input-window)
        (select-window enkan-dual-task-main-input-window)))
    ;; From eat windows -> go to main input
    (t
      (when (window-live-p enkan-dual-task-main-input-window)
        (select-window enkan-dual-task-main-input-window)))))

;;; ========================================
;;; Window Lock/Unlock Functions (placeholder)
;;; ========================================

;; Note: Window locking disabled to prevent eat scrolling issues
;; These functions are kept as placeholders for consistency

(defun enkan-dual-task-lock-windows ()
  "Lock windows to prevent opening other files in them.
Currently disabled to prevent eat scrolling issues."
  (interactive)
  (message "Window locking is disabled to prevent eat scrolling issues."))

(defun enkan-dual-task-unlock-windows ()
  "Unlock windows to allow opening other files.
Currently disabled to prevent eat scrolling issues."
  (interactive)
  (message "Window unlocking is disabled (windows are not locked)."))

;;; ========================================
;;; Remote eat Control Functions
;;; ========================================

(defun enkan-dual-task-send-escape ()
  "Send ESC to the appropriate eat buffer from any window."
  (interactive)
  (enkan-dual-task--send-to-active-eat
    'enkan-repl-send-escape "ESC"))

(defun enkan-dual-task-send-1 ()
  "Send choice 1 to the appropriate eat buffer from any window."
  (interactive)
  (enkan-dual-task--send-to-active-eat
    'enkan-repl-send-1 "1"))

(defun enkan-dual-task-send-2 ()
  "Send choice 2 to the appropriate eat buffer from any window."
  (interactive)
  (enkan-dual-task--send-to-active-eat
    'enkan-repl-send-2 "2"))

(defun enkan-dual-task-send-3 ()
  "Send choice 3 to the appropriate eat buffer from any window."
  (interactive)
  (enkan-dual-task--send-to-active-eat
    'enkan-repl-send-3 "3"))

(defun enkan-dual-task--send-to-active-eat (func msg)
  "Helper function to send commands to the active eat buffer.
FUNC is the function to call, MSG is the message to display.
Sends to the eat buffer corresponding to the current input window."
  (let ((target-eat-window
          (cond
            ;; If in main input window, send to main eat
            ((and enkan-dual-task-main-input-window
               (eq (selected-window) enkan-dual-task-main-input-window))
              enkan-dual-task-main-eat-window)
            ;; If in sub input window, send to sub eat
            ((and enkan-dual-task-sub-input-window
               (eq (selected-window) enkan-dual-task-sub-input-window))
              enkan-dual-task-sub-eat-window)
            ;; Default to main eat window
            (t enkan-dual-task-main-eat-window))))
    (if (and target-eat-window
          (window-live-p target-eat-window))
      (save-window-excursion
        (select-window target-eat-window)
        (funcall func)
        (message "Sent %s to %s"
          msg
          (if (eq target-eat-window enkan-dual-task-main-eat-window)
            "main eat buffer"
            "sub eat buffer")))
      (message "No eat window found. Run M-x enkan-dual-task-setup first."))))

(defun enkan-dual-task-send-to-main ()
  "Send text to main eat buffer explicitly."
  (interactive)
  (if (and enkan-dual-task-main-eat-window
        (window-live-p enkan-dual-task-main-eat-window))
    (save-window-excursion
      (select-window enkan-dual-task-main-eat-window)
      (message "Switched to main eat buffer for input"))
    (message "Main eat window not found.")))

(defun enkan-dual-task-send-to-sub ()
  "Send text to sub eat buffer explicitly."
  (interactive)
  (if (and enkan-dual-task-sub-eat-window
        (window-live-p enkan-dual-task-sub-eat-window))
    (save-window-excursion
      (select-window enkan-dual-task-sub-eat-window)
      (message "Switched to sub eat buffer for input"))
    (message "Sub eat window not found.")))

;;; ========================================
;;; Display Buffer Management
;;; ========================================

(defun enkan-dual-task-setup-display-buffer-rules ()
  "Setup display-buffer rules for dual task layout.
Currently minimal to avoid eat scrolling issues."
  ;; Placeholder for future display-buffer rules
  ;; Currently not adding advices to avoid interfering with eat
  )

(defun enkan-dual-task-display-buffer-advice (orig-fun buffer &rest args)
  "Advice to handle buffer display in dual task layout.
Currently minimal implementation to avoid interfering with dual task workflow."
  (let ((buffer-name (buffer-name (get-buffer buffer))))
    (cond
      ;; Don't redirect eat buffers
      ((string-match-p "^\\*enkan:" buffer-name)
        (apply orig-fun buffer args))
      ;; Don't redirect buffers that are already displayed
      ((get-buffer-window buffer)
        (apply orig-fun buffer args))
      ;; Default behavior for dual task layout
      (t (apply orig-fun buffer args)))))

(defun enkan-dual-task-protect-window-advice (orig-fun &optional window)
  "Prevent deletion of protected windows in dual task layout.
Placeholder for consistency with 3pane."
  ;; Currently no window protection needed for dual task
  (apply orig-fun window))

(defun enkan-dual-task-quit-window-advice (orig-fun &optional kill window)
  "Handle quit-window in dual task layout.
Placeholder for consistency with 3pane."
  ;; Currently no special quit handling needed for dual task
  (apply orig-fun kill window))

;;; ========================================
;;; Session Selection Functions
;;; ========================================

(defun enkan-dual-task-select-sessions ()
  "Select two enkan-eat sessions interactively."
  (let ((eat-buffers (enkan-dual-task-get-all-eat-buffers)))
    (when (< (length eat-buffers) 2)
      (error "Found only %d enkan session(s). Need at least 2" (length eat-buffers)))

    ;; Select main session
    (let* ((main-name (completing-read "Select MAIN enkan session: "
                        (mapcar #'buffer-name eat-buffers)
                        nil t))
            (main-buffer (get-buffer main-name))
            ;; Filter out selected main for sub selection
            (remaining (remove main-buffer eat-buffers))
            (sub-name (completing-read "Select SUB enkan session: "
                        (mapcar #'buffer-name remaining)
                        nil t))
            (sub-buffer (get-buffer sub-name)))
      (list main-buffer sub-buffer))))

(defun enkan-dual-task-get-all-eat-buffers ()
  "Get all enkan-eat buffers."
  (seq-filter (lambda (buffer)
                (string-match-p "^\\*enkan:" (buffer-name buffer)))
    (buffer-list)))

(defun enkan-dual-task-get-input-buffer-for-eat (eat-buffer)
  "Get the input file buffer corresponding to EAT-BUFFER."
  (when eat-buffer
    (let* ((buffer-name (buffer-name eat-buffer))
            ;; Extract directory from buffer name like "*enkan:/path/to/dir/*"
            (dir (when (string-match "^\\*enkan:\\(.+\\)\\*$" buffer-name)
                   (match-string 1 buffer-name)))
            (input-file (when dir
                          (enkan-repl--get-project-file-path dir))))
      (when input-file
        (if (file-exists-p input-file)
          (find-file-noselect input-file)
          ;; Create if doesn't exist
          (enkan-repl--create-project-input-file dir)
          (find-file-noselect input-file))))))

;;; ========================================
;;; Helper Functions
;;; ========================================

(defun enkan-dual-task-describe-layout ()
  "Describe current dual task layout."
  (interactive)
  (with-output-to-temp-buffer "*Dual Task Layout*"
    (princ "Dual Task Window Layout\n")
    (princ "========================\n\n")

    (princ "Main Task:\n")
    (princ (format "  Input: %s\n"
             (if enkan-dual-task-main-input-buffer
               (buffer-name enkan-dual-task-main-input-buffer)
               "Not set")))
    (princ (format "  Eat:   %s\n\n"
             (if enkan-dual-task-main-eat-buffer
               (buffer-name enkan-dual-task-main-eat-buffer)
               "Not set")))

    (princ "Sub Task:\n")
    (princ (format "  Input: %s\n"
             (if enkan-dual-task-sub-input-buffer
               (buffer-name enkan-dual-task-sub-input-buffer)
               "Not set")))
    (princ (format "  Eat:   %s\n\n"
             (if enkan-dual-task-sub-eat-buffer
               (buffer-name enkan-dual-task-sub-eat-buffer)
               "Not set")))

    (princ "Window Layout:\n")
    (princ "  [Input Main] [Main Eat ] [Sub Eat  ]\n")
    (princ "  [Input Sub ] [         ] [         ]\n")
    (princ "     30%          40%         30%    \n\n")

    (princ "Keybindings:\n")
    (princ "  C-t - Switch between input windows\n")))

(defun enkan-dual-task--track-buffer-history (new-buffer)
  "Track buffer history for dual task windows.
NEW-BUFFER is the buffer about to be displayed.
Placeholder for consistency with 3pane - minimal implementation."
  ;; Dual task doesn't need complex buffer history like 3pane
  ;; This is just for structural consistency
  nil)

(defun enkan-dual-task--restore-previous-buffer ()
  "Restore the previous buffer in dual task windows.
Placeholder for consistency with 3pane - minimal implementation."
  ;; Dual task doesn't need buffer restoration like 3pane
  ;; This is just for structural consistency
  (switch-to-buffer (other-buffer)))

;;; ========================================
;;; Cheat Sheet Integration
;;; ========================================

(defun enkan-dual-task-cheat-sheet-advice (orig-fun)
  "Advice to add dual task commands to enkan-repl cheat sheet."
  (if enkan-dual-task-mode
    ;; When dual task mode is active, show combined cheat sheet
    (progn
      (unless (featurep 'enkan-repl-constants)
        (require 'enkan-repl-constants nil t))
      (let* ((original-candidates
               (if (boundp 'enkan-repl-cheat-sheet-candidates)
                 enkan-repl-cheat-sheet-candidates
                 '()))
              ;; Combine original and dual task candidates
              (candidates (append original-candidates
                            enkan-dual-task-cheat-sheet-candidates)))
        (let ((completion-extra-properties
                `(:annotation-function
                   (lambda (candidate)
                     (let ((description (alist-get candidate ',candidates
                                          nil nil #'string=)))
                       (when description
                         (format " — %s" description)))))))
          (let ((selected-command
                  (completing-read "enkan-repl & dual task commands: " candidates)))
            (when selected-command
              (call-interactively (intern selected-command)))))))
    ;; When dual task mode is not active, use original function
    (funcall orig-fun)))

(defun enkan-dual-task-setup-cheat-sheet-advice ()
  "Setup advice for cheat sheet integration."
  (advice-add 'enkan-repl-cheat-sheet :around
    #'enkan-dual-task-cheat-sheet-advice))

(defun enkan-dual-task-remove-cheat-sheet-advice ()
  "Remove advice for cheat sheet integration."
  (advice-remove 'enkan-repl-cheat-sheet
    #'enkan-dual-task-cheat-sheet-advice))

;;; ========================================
;;; Provide
;;; ========================================

(provide 'dual-task-window-layout)

;;; dual-task-window-layout.el ends here

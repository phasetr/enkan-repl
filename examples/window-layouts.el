;;; window-layouts.el --- Window layout examples for enkan-repl center file mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Window layout configuration examples for enkan-repl center file mode.
;; Shows different layout patterns for managing multiple sessions.

;;; Code:

(require 'enkan-repl)

;;;; Pure Functions for Project Path Resolution

(defun enkan-repl--get-session-project-path-pure (session-number session-list project-registry)
  "Get project path for SESSION-NUMBER from SESSION-LIST and PROJECT-REGISTRY.
Pure function that returns project path string or nil.
SESSION-LIST entry value may be a string (legacy) or
\(project-name . instance) cons; both are accepted via the entry accessors.
PROJECT-REGISTRY format: ((alias . (project-name . project-path)) ...)"
  (let* ((entry-value (cdr (assoc session-number session-list)))
         (session-project (enkan-repl--session-entry-project entry-value))
         (project-info (when session-project
                         (cl-find-if (lambda (entry)
                                       (string= (car (cdr entry)) session-project))
                                     project-registry)))
         (project-path (when project-info (cdr (cdr project-info)))))
    project-path))

(defun enkan-repl--layout-session-project-info (session-project project-registry)
  "Return project info for SESSION-PROJECT from PROJECT-REGISTRY.
SESSION-PROJECT may be either a real project name or a configured alias."
  (when session-project
    (or (cdr (assoc session-project project-registry))
        (cdr
         (cl-find-if (lambda (entry)
                       (let ((value (cdr entry)))
                         (and (consp value)
                              (string= (car value) session-project))))
                     project-registry)))))

(defun enkan-repl--setup-window-terminal-buffer-pure (window session-number session-list project-registry)
  "Pure function to determine terminal buffer setup for WINDOW and SESSION-NUMBER.
Returns cons (window . buffer-name) or nil if session not registered.
Honors the multi-instance index stored in SESSION-LIST so different
instances of the same project map to distinct buffer names with <N>
suffix."
  (let* ((entry-value (cdr (assoc session-number session-list)))
         (project-name (enkan-repl--session-entry-project entry-value))
         (instance (enkan-repl--session-entry-instance entry-value)))
    (when project-name
      (let* ((project-info
              (enkan-repl--layout-session-project-info
               project-name project-registry))
             (project-path (cdr project-info)))
        (when project-path
          (let* ((expanded-path (expand-file-name project-path))
                 ;; Ensure path ends with / for consistency with terminal buffer names.
                 (normalized-path (if (string-suffix-p "/" expanded-path)
                                      expanded-path
                                    (concat expanded-path "/")))
                 ;; Use current workspace ID from global variable
                 (ws-id (if (and (boundp 'enkan-repl--current-workspace)
                                 enkan-repl--current-workspace
                                 (stringp enkan-repl--current-workspace))
                            enkan-repl--current-workspace
                          "01"))
                 (base (format "*ws:%s enkan:%s*" ws-id normalized-path))
                 (buffer-name (if (and instance (> instance 1))
                                  (format "%s<%d>" base instance)
                                base)))
            (cons window buffer-name)))))))

(defun enkan-repl--registered-session-buffer-for-entry (entry)
  "Return the live terminal buffer corresponding to session ENTRY.
ENTRY is one element of `enkan-repl-session-list'.  Prefer the exact path
registered through `enkan-repl-target-directories'.  When no project path is
configured, fall back to matching the project name encoded in existing buffer
names.  Stray buffers whose paths do not correspond to registered sessions are
not returned."
  (let* ((session-number (car entry))
         (entry-value (cdr entry))
         (project-name (enkan-repl--session-entry-project entry-value))
         (instance (enkan-repl--session-entry-instance entry-value))
         (project-info
          (enkan-repl--layout-session-project-info
           project-name enkan-repl-target-directories))
         (resolved-project-name (or (car project-info) project-name))
         (setup (enkan-repl--setup-window-terminal-buffer-pure
                 nil session-number (enkan-repl--ws-session-list)
                 enkan-repl-target-directories))
         (exact-buffer (and setup (get-buffer (cdr setup)))))
    (cond
     ((and exact-buffer
           (enkan-repl--buffer-alive-as-terminal-p exact-buffer))
      exact-buffer)
     (project-name
      (cl-find-if
       (lambda (buffer)
         (let ((name (buffer-name buffer)))
           (and name
                (enkan-repl--buffer-name-matches-workspace
                 name enkan-repl--current-workspace)
                (member (or (enkan-repl--extract-project-name name) "")
                        (delete-dups
                         (list project-name resolved-project-name)))
                (= (enkan-repl--buffer-name->instance name) instance)
                (enkan-repl--buffer-alive-as-terminal-p buffer))))
       (buffer-list))))))

(defun enkan-repl--registered-session-buffers ()
  "Return live terminal buffers registered for the current workspace.
The returned list follows session slot order and excludes stray tmux mirror
buffers that merely share the same workspace prefix.  Duplicate session-list
entries that resolve to the same buffer are collapsed."
  (let (buffers)
    (dolist (entry (sort (copy-sequence (enkan-repl--ws-session-list))
                         (lambda (a b) (< (car a) (car b)))))
      (let ((buffer (enkan-repl--registered-session-buffer-for-entry entry)))
        (when (and buffer (not (memq buffer buffers)))
          (push buffer buffers))))
    (nreverse buffers)))

;;;; Window Layout Variables

(defvar enkan-repl--window-1 nil
  "Window 1 (center file) for enkan-repl multi-buffer layout.")
(defvar enkan-repl--window-2 nil
  "Window 2 (work area) for enkan-repl multi-buffer layout.")
(defvar enkan-repl--window-3 nil
  "Window 3 (reserve area) for enkan-repl multi-buffer layout.")
(defvar enkan-repl--window-4 nil
  "Window 4 (session 1) for enkan-repl multi-buffer layout.")
(defvar enkan-repl--window-5 nil
  "Window 5 (session 2) for enkan-repl multi-buffer layout.")

;;;; Window Layout Functions

;;;###autoload
(defun other-window-or-split ()
  "Move to the next window, splitting the frame when only one window exists."
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (other-window 1))

;; Helper function for setting up terminal buffer in window.
(defun enkan-repl--setup-session-terminal-buffer (window session-number)
  "Setup terminal buffer for SESSION-NUMBER in WINDOW."
  (let ((terminal-setup (enkan-repl--setup-window-terminal-buffer-pure
                         window session-number (enkan-repl--ws-session-list) enkan-repl-target-directories)))
    (if terminal-setup
      (let ((buffer (get-buffer (cdr terminal-setup))))
        (if buffer
          (progn
            (select-window (car terminal-setup))
            (switch-to-buffer buffer)
            (message "✅ Window %d: Opened terminal buffer %s"
                    session-number (cdr terminal-setup)))
          (message "❌ Window %d: Terminal buffer %s not found. Run C-M-s to start sessions."
            session-number (cdr terminal-setup))))
      (message "❌ Window %d: No session registered for slot %d (internal number %d)."
        session-number session-number session-number))))

;;;###autoload
(defun enkan-repl-setup-1session-layout ()
  "Setup window layout for 1-session management.
  +--------+---+
  |    1   | 2 |
  | center |   |
  |  file  |   |
  +-----+--+---+

  1: Center file (command source)
  2: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup terminal buffers in session windows - use actual living buffers.
  (let ((available-buffers (enkan-repl--registered-session-buffers)))
    (when (and available-buffers (> (length available-buffers) 0))
      ;; Use first available buffer in window 2
      (when enkan-repl--window-2
        (select-window enkan-repl--window-2)
        (switch-to-buffer (car available-buffers))
        (message "✅ Window 2: Opened terminal buffer %s" (buffer-name (car available-buffers))))))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-2session-layout ()
  "Setup window layout for 2-session management.
  +--------+---+---+
  |    1   | 2 | 3 |
  | center |   |   |
  |  file  |   |   |
  +-----+--+---+---+

  1: Center file (command source)
  2,3: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.35)))
  ;; Move to right section and split into 3 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.55)))
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup terminal buffers in session windows - use actual living buffers.
  (let ((available-buffers (enkan-repl--registered-session-buffers)))
    (when (and available-buffers (> (length available-buffers) 0))
      ;; Place first buffer in window 2
      (when enkan-repl--window-2
        (select-window enkan-repl--window-2)
        (switch-to-buffer (car available-buffers))
        (message "✅ Window 2: Opened terminal buffer %s" (buffer-name (car available-buffers))))
      ;; Place second buffer in window 3 if exists
      (when (and (> (length available-buffers) 1) enkan-repl--window-3)
        (select-window enkan-repl--window-3)
        (switch-to-buffer (cadr available-buffers))
        (message "✅ Window 3: Opened terminal buffer %s" (buffer-name (cadr available-buffers))))))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-3session-layout ()
  "Setup window layout for 3-session management.
  +--------+---+---+---+
  |    1   | 2 | 3 | 4 |
  | center |   |   |   |
  |  file  |   |   |   |
  +--------+---+---+---+

  2,3,4: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 3 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.5)))
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup terminal buffers in session windows - use actual living buffers.
  (let ((available-buffers (enkan-repl--registered-session-buffers)))
    (when (and available-buffers (> (length available-buffers) 0))
      ;; Place first buffer in window 2
      (when enkan-repl--window-2
        (select-window enkan-repl--window-2)
        (switch-to-buffer (car available-buffers))
        (message "✅ Window 2: Opened terminal buffer %s" (buffer-name (car available-buffers))))
      ;; Place second buffer in window 3 if exists
      (when (and (> (length available-buffers) 1) enkan-repl--window-3)
        (select-window enkan-repl--window-3)
        (switch-to-buffer (cadr available-buffers))
        (message "✅ Window 3: Opened terminal buffer %s" (buffer-name (cadr available-buffers))))
      ;; Place third buffer in window 4 if exists
      (when (and (> (length available-buffers) 2) enkan-repl--window-4)
        (select-window enkan-repl--window-4)
        (switch-to-buffer (caddr available-buffers))
        (message "✅ Window 4: Opened terminal buffer %s" (buffer-name (caddr available-buffers))))))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-4session-layout ()
  "Setup window layout for 4-session management.
  +--------+---+---+---+---+
  |    1   | 2 | 3 | 4 | 5 |
  | center |   |   |   |   |
  |  file  |   |   |   |   |
  +--------+---+---+---+---+

  2,3,4,5: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 4 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.3)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right)
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-5 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup terminal buffers in session windows - use actual living buffers.
  (let ((available-buffers (enkan-repl--registered-session-buffers)))
    (when (and available-buffers (> (length available-buffers) 0))
      ;; Place first buffer in window 2
      (when enkan-repl--window-2
        (select-window enkan-repl--window-2)
        (switch-to-buffer (car available-buffers))
        (message "✅ Window 2: Opened terminal buffer %s" (buffer-name (car available-buffers))))
      ;; Place second buffer in window 3 if exists
      (when (and (> (length available-buffers) 1) enkan-repl--window-3)
        (select-window enkan-repl--window-3)
        (switch-to-buffer (cadr available-buffers))
        (message "✅ Window 3: Opened terminal buffer %s" (buffer-name (cadr available-buffers))))
      ;; Place third buffer in window 4 if exists
      (when (and (> (length available-buffers) 2) enkan-repl--window-4)
        (select-window enkan-repl--window-4)
        (switch-to-buffer (caddr available-buffers))
        (message "✅ Window 4: Opened terminal buffer %s" (buffer-name (caddr available-buffers))))
      ;; Place fourth buffer in window 5 if exists
      (when (and (> (length available-buffers) 3) enkan-repl--window-5)
        (select-window enkan-repl--window-5)
        (switch-to-buffer (cadddr available-buffers))
        (message "✅ Window 5: Opened terminal buffer %s" (buffer-name (cadddr available-buffers))))))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;; Dynamic Window Layout Selection

;;;###autoload
(defun enkan-repl-setup-current-project-layout ()
  "Setup window layout for the currently active project.
Uses actual living buffers in current workspace to determine layout.

Category: Utilities"
  (interactive)
  (unless (enkan-repl--ws-current-project)
    (error "No current project active. Run enkan-repl-setup first"))
  (unless enkan-repl--current-workspace
    (error "No current workspace active"))
  ;; Count registered live buffers for current workspace.  Do not use every
  ;; mirror buffer with the workspace prefix: stale tmux windows can recreate
  ;; stray mirrors asynchronously, and those must not drive the layout.
  (let* ((session-buffers (enkan-repl--registered-session-buffers))
         (session-count (length session-buffers)))
    (message "Setting up layout for %d actual sessions in workspace '%s' for project '%s'..."
      session-count enkan-repl--current-workspace (enkan-repl--ws-current-project))
    (cond
      ((= session-count 0)
        (message "⚠️  No active sessions found in workspace '%s'" enkan-repl--current-workspace))
      ((= session-count 1)
        (enkan-repl-setup-1session-layout)
        (message "✅ Applied 1-session layout for workspace '%s'" enkan-repl--current-workspace))
      ((= session-count 2)
        (enkan-repl-setup-2session-layout)
        (message "✅ Applied 2-session layout for workspace '%s'" enkan-repl--current-workspace))
      ((= session-count 3)
        (enkan-repl-setup-3session-layout)
        (message "✅ Applied 3-session layout for workspace '%s'" enkan-repl--current-workspace))
      ((= session-count 4)
        (enkan-repl-setup-4session-layout)
        (message "✅ Applied 4-session layout for workspace '%s'" enkan-repl--current-workspace))
      (t
        (error "Unsupported session count: %d (supported: 1-4 sessions)" session-count)))))

(provide 'window-layouts)
;;; window-layouts.el ends here

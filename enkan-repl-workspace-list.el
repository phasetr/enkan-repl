;;; enkan-repl-workspace-list.el --- Workspace list display for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: enkan tools convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides workspace list display functionality for enkan-repl.
;; It displays all workspaces with their associated project information
;; in a temporary buffer with keybindings for quick workspace switching.

;;; Code:

(require 'cl-lib)

;; Declare external functions to avoid byte-compile warnings
(declare-function enkan-repl--list-workspace-ids "enkan-repl-workspace" (workspaces))
(declare-function enkan-repl--get-workspace-state "enkan-repl-workspace" (workspaces workspace-id))
(declare-function enkan-repl--save-workspace-state "enkan-repl" ())
(declare-function enkan-repl--load-workspace-state "enkan-repl" (workspace-id))
(declare-function enkan-repl--get-project-info-from-directories "enkan-repl-utils" (alias target-directories))

;; Declare external variables
(defvar enkan-repl--current-workspace)

(defvar enkan-repl-workspace-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'enkan-repl-workspace-list-switch-to-workspace)
    (define-key map (kbd "g") 'enkan-repl-workspace-list-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `enkan-repl-workspace-list-mode'.")

(define-derived-mode enkan-repl-workspace-list-mode special-mode "Enkan-Workspace-List"
  "Major mode for displaying and managing enkan-repl workspaces.

\\{enkan-repl-workspace-list-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun enkan-repl-workspace-list--format-workspace-info (workspace-id workspaces current-workspace target-directories)
  "Format workspace information for display.
WORKSPACE-ID is the workspace identifier.
WORKSPACES is the global workspace data structure.
CURRENT-WORKSPACE is the currently active workspace ID.
TARGET-DIRECTORIES is the list of target directories."
  (let* ((state (enkan-repl--get-workspace-state workspaces workspace-id))
          (current-project (plist-get state :current-project))
          (project-aliases (plist-get state :project-aliases))
          (is-current (string= workspace-id current-workspace))
          (project-info (when current-project
                          (enkan-repl--get-project-info-from-directories
                            current-project target-directories)))
          (project-dir (cond
                         ;; project-info is (project-name . project-path)
                         ((and (consp project-info) (consp (cdr project-info)))
                          (cdr project-info))
                         ;; project-info is just a string (path or domain)
                         ((stringp project-info)
                          project-info)
                         ;; project-info is some other format
                         (t nil))))
    (propertize
      (format "%s%s - %s: %s"
        (if is-current "▶ " "  ")
        workspace-id
        (or current-project "<none>")
        (or project-dir "<no directory>"))
      'workspace-id workspace-id)))

(defun enkan-repl-workspace-list--get-workspace-at-point ()
  "Get the workspace ID at the current point."
  (get-text-property (point) 'workspace-id))

(defun enkan-repl-workspace-list-switch-to-workspace ()
  "Switch to the workspace at point."
  (interactive)
  (let ((workspace-id (enkan-repl-workspace-list--get-workspace-at-point)))
    (when workspace-id
      (let ((current-workspace (bound-and-true-p enkan-repl--current-workspace)))
        (if (string= workspace-id current-workspace)
          (message "Already in workspace %s" workspace-id)
          ;; Close the list buffer first
          (quit-window)
          ;; Save current workspace state
          (enkan-repl--save-workspace-state)
          ;; Switch to the new workspace
          (setq enkan-repl--current-workspace workspace-id)
          ;; Load the new workspace state
          (enkan-repl--load-workspace-state workspace-id)
          (message "Switched to workspace %s" workspace-id))))))

(defun enkan-repl-workspace-list-refresh ()
  "Refresh the workspace list display."
  (interactive)
  (enkan-repl-workspace-list))

;;;###autoload
(defun enkan-repl-workspace-list ()
  "Display a list of all workspaces with their information.
Shows workspace ID, current/inactive status, associated project,
aliases, session counts, and target directories."
  (interactive)
  (let* ((workspaces (bound-and-true-p enkan-repl--workspaces))
          (current-workspace (bound-and-true-p enkan-repl--current-workspace))
          (target-directories (bound-and-true-p enkan-repl-target-directories))
          (workspace-ids (when workspaces
                           (enkan-repl--list-workspace-ids workspaces)))
          (buffer-name "*Enkan Workspace List*"))
    (if (not workspace-ids)
      (message "No workspaces found")
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Enkan REPL Workspaces\n")
          (insert "=====================\n\n")
          (insert "Keys: RET - switch to workspace, g - refresh, q - quit\n\n")
          ;; Sort workspace IDs to ensure consistent display
          (setq workspace-ids (sort workspace-ids #'string<))
          ;; Display each workspace
          (dolist (ws-id workspace-ids)
            (insert (enkan-repl-workspace-list--format-workspace-info
                      ws-id workspaces current-workspace target-directories))
            (insert "\n"))
          (goto-char (point-min))
          ;; Move to first workspace line
          (forward-line 5)
          (enkan-repl-workspace-list-mode))
        (pop-to-buffer buffer-name)))))

;; Helper variable for workspace switching
(defvar enkan-repl--workspace-switch-target nil
  "Target workspace ID for switching.
Used to communicate between list buffer and switch function.")

(provide 'enkan-repl-workspace-list)

;;; enkan-repl-workspace-list.el ends here

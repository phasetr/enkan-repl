;;; enkan-repl-workspace.el --- Workspace management utilities for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: phasetr <phasetr@gmail.com>
;; URL: https://github.com/phasetr/enkan-repl
;; Keywords: tools
;; Package-Requires: ((emacs "28.2") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Workspace management utilities for enkan-repl package.
;; This file provides pure functions for workspace state management,
;; ID generation, and workspace manipulation.

;;; Code:

(require 'cl-lib)

;;;; Workspace Management Pure Functions

(defun enkan-repl--generate-next-workspace-id (workspaces)
  "Generate next workspace ID from WORKSPACES alist.
Returns zero-padded numeric string (e.g., \"01\", \"02\")."
  (if (null workspaces)
      "01"
    (let ((ids (mapcar (lambda (ws)
                         (string-to-number (car ws)))
                       workspaces)))
      (format "%02d" (1+ (apply #'max ids))))))

(defun enkan-repl--create-workspace-state (workspaces)
  "Create new workspace and return its ID.
WORKSPACES is the current workspace alist.
Returns the new workspace ID string."
  (enkan-repl--generate-next-workspace-id workspaces))

(defun enkan-repl--add-workspace (workspaces new-id)
  "Add new workspace with NEW-ID to WORKSPACES alist.
Returns updated alist with new workspace."
  (cons (cons new-id
              (list :current-project nil
                    :session-list nil
                    :session-counter 0
                    :project-aliases nil))
        workspaces))

(defun enkan-repl--get-workspace-state (workspaces workspace-id)
  "Get state plist for WORKSPACE-ID from WORKSPACES alist.
Returns plist or nil if not found."
  (cdr (assoc workspace-id workspaces #'string=)))

(defun enkan-repl--can-delete-workspace (workspace-id workspaces)
  "Check if WORKSPACE-ID can be deleted from WORKSPACES.
Cannot delete if it's the only workspace or doesn't exist."
  (and (> (length workspaces) 1)
       (assoc workspace-id workspaces #'string=)
       t))

(defun enkan-repl--delete-workspace (workspaces workspace-id)
  "Remove WORKSPACE-ID from WORKSPACES alist.
Returns updated alist without the specified workspace."
  (cl-remove-if (lambda (ws)
                  (string= (car ws) workspace-id))
                workspaces))

(defun enkan-repl--list-workspace-ids (workspaces)
  "List all workspace IDs from WORKSPACES alist.
Returns sorted list of unique ID strings."
  (when workspaces
    (let ((ids (mapcar #'car workspaces)))
      (sort (cl-remove-duplicates ids :test #'string=) #'string<))))

(provide 'enkan-repl-workspace)
;;; enkan-repl-workspace.el ends here
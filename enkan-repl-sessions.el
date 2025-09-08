;;; enkan-repl-sessions.el --- Session management functions for enkan-repl -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Enkan Contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains session management functions for enkan-repl.
;; Functions are pure and handle session state, formatting, and actions.

;;; Code:

(require 'cl-lib)

;; Forward declarations to avoid circular dependencies
(declare-function enkan-repl--is-enkan-buffer-name "enkan-repl-utils")
(declare-function enkan-repl--buffer-name->path "enkan-repl-utils")

;;; Session Information Extraction

(defun enkan-repl--extract-session-info (buffer-name buffer-live-p has-eat-process process-live-p)
  "Pure function to extract session info from buffer properties.
BUFFER-NAME is the name of the buffer.
BUFFER-LIVE-P indicates if buffer is live.
HAS-EAT-PROCESS indicates if buffer has eat--process variable.
PROCESS-LIVE-P indicates if the process is alive.
Returns a plist with :name, :directory, and :status, or nil if not a session."
  (when (and buffer-name
             buffer-live-p
             (enkan-repl--is-enkan-buffer-name buffer-name))
    (let ((dir (enkan-repl--buffer-name->path buffer-name))
          (status (if (and has-eat-process process-live-p)
                      'alive
                    'dead)))
      (list :name buffer-name
            :directory dir
            :status status))))

(defun enkan-repl--collect-sessions (buffer-info-list)
  "Pure function to collect session info from buffer info list.
BUFFER-INFO-LIST is a list of plists with buffer properties.
Each plist should have :name, :live-p, :has-eat-process, and :process-live-p.
Returns a list of session info plists."
  (let ((sessions '()))
    (dolist (buffer-info buffer-info-list)
      (let ((session-info
             (enkan-repl--extract-session-info
              (plist-get buffer-info :name)
              (plist-get buffer-info :live-p)
              (plist-get buffer-info :has-eat-process)
              (plist-get buffer-info :process-live-p))))
        (when session-info
          (push session-info sessions))))
    (nreverse sessions)))

;;; Session Formatting Functions

(defun enkan-repl--format-sessions (sessions)
  "Pure function to format sessions for display.
SESSIONS is a list of session info plists.
Returns a formatted string for display."
  (if (null sessions)
      "No active sessions found\n"
    (concat "Active enkan-repl sessions:\n"
            "Press 'd' to delete a session, 'q' to quit\n"
            "─────────────────────────────────────────\n\n"
            (mapconcat
             (lambda (session)
               (format "  %s\n    Directory: %s\n    Status: %s\n\n"
                       (plist-get session :name)
                       (plist-get session :directory)
                       (plist-get session :status)))
             sessions
             ""))))

(defun enkan-repl--format-numbered-sessions (sessions)
  "Pure function to format numbered session list.
SESSIONS is a list of session info plists.
Returns a formatted string with numbered sessions."
  (let ((result "")
        (index 1))
    (dolist (session sessions)
      (setq result
            (concat result
                    (format "%d. %s — Directory: %s, Status: %s\n"
                            index
                            (plist-get session :name)
                            (plist-get session :directory)
                            (plist-get session :status))))
      (setq index (1+ index)))
    result))

(defun enkan-repl--format-minibuffer-sessions (sessions)
  "Pure function to format sessions for minibuffer display.
SESSIONS is a list of session info plists.
Returns a list of formatted strings, one per session."
  (let ((index 1)
        (result '()))
    (dolist (session sessions)
      (push (format "%d. %s — Directory: %s, Status: %s"
                    index
                    (plist-get session :name)
                    (plist-get session :directory)
                    (plist-get session :status))
            result)
      (setq index (1+ index)))
    (nreverse result)))

(defun enkan-repl--prepare-session-candidates (sessions)
  "Pure function to prepare candidates for completing-read.
SESSIONS is a list of session info plists.
Returns an alist of (NAME . DESCRIPTION)."
  (mapcar
   (lambda (session)
     (cons (plist-get session :name)
           (format "Directory: %s, Status: %s"
                   (plist-get session :directory)
                   (plist-get session :status))))
   sessions))

;;; Session Selection and Actions

(defun enkan-repl--find-session-buffer (selected-name buffer-info-list)
  "Pure function to find buffer for selected session.
SELECTED-NAME is the selected session name.
BUFFER-INFO-LIST is the list of buffer info plists.
Returns the buffer object or nil."
  (let ((buf-info (cl-find-if
                   (lambda (info)
                     (equal (plist-get info :name) selected-name))
                   buffer-info-list)))
    (and buf-info (plist-get buf-info :buffer))))

(defun enkan-repl--session-action (action selected-name)
  "Pure function to determine session action result.
ACTION is the action character (?s, ?d, ?c).
SELECTED-NAME is the selected session name.
Returns a plist with :type and :message."
  (cl-case action
    (?s (list :type 'switch
              :message (format "Switching to session: %s" selected-name)))
    (?d (list :type 'delete
              :message (format "Deleting session: %s" selected-name)))
    (?c (list :type 'copy
              :message (format "Copying session path: %s" selected-name)))
    (t (list :type 'unknown
             :message "Unknown action"))))

(defun enkan-repl--validate-session-selection (selection candidates)
  "Pure function to validate session selection.
SELECTION is the user's selection.
CANDIDATES is the list of valid candidates.
Returns the valid selection or nil."
  (when (and selection
             (member selection candidates))
    selection))

(defun enkan-repl--format-session-list (sessions)
  "Pure function to format sessions for display.
SESSIONS is a list of session info plists.
Returns a formatted string for display."
  (if (null sessions)
      "No sessions found\n"
    (concat "Sessions:\n"
            (mapconcat
             (lambda (session)
               (format "  %s (%s) - %s"
                       (plist-get session :name)
                       (plist-get session :directory)
                       (plist-get session :status)))
             sessions
             "\n"))))

;;; Session State Information

(defun enkan-repl--get-current-session-state-info (current-project session-list session-counter project-aliases)
  "Pure function to get formatted session state information.
CURRENT-PROJECT is the current project name or nil.
SESSION-LIST is an alist of (session-number . project-name).
SESSION-COUNTER is the next session number.
PROJECT-ALIASES is a list of project aliases.
Returns a plist with formatted information."
  (list :current-project (or current-project "none")
        :session-count (length session-list)
        :next-session-number session-counter
        :sessions (if session-list
                      (mapconcat (lambda (s)
                                   (format "%d -> %s" (car s) (cdr s)))
                                 session-list ", ")
                    "none")
        :aliases (if project-aliases
                     (mapconcat #'identity project-aliases ", ")
                   "none")))

(defun enkan-repl--format-session-state-display (state-info &optional prefix)
  "Pure function to format session state for display.
STATE-INFO is a plist from `enkan-repl--get-current-session-state-info'.
PREFIX is an optional string prefix for each line.
Returns a formatted string."
  (let ((prefix (or prefix "")))
    (format "%sCurrent project: %s
%sActive sessions: %d
%sNext session #: %d
%sSessions: %s
%sAliases: %s"
            prefix (plist-get state-info :current-project)
            prefix (plist-get state-info :session-count)
            prefix (plist-get state-info :next-session-number)
            prefix (plist-get state-info :sessions)
            prefix (plist-get state-info :aliases))))

;;; Session Deletion Support

(defun enkan-repl--find-deletion-bounds (lines current-line)
  "Pure function to find bounds for deletion.
LINES is a list of strings (buffer lines).
CURRENT-LINE is the current line number (0-indexed).
Returns a plist with :start-line and :end-line for deletion bounds,
or nil if not on a session entry."
  (when (and (>= current-line 0)
             (< current-line (length lines)))
    (let ((line (nth current-line lines)))
      ;; Check if we're on or near a session entry
      (cond
       ;; On session name line
       ((and (>= (length line) 2)
             (enkan-repl--is-enkan-buffer-name (substring line 2)))
        (list :start-line current-line
              :end-line (min (+ current-line 4) (length lines))))
       ;; On directory line
       ((and (> current-line 0)
             (string-match "^    Directory:" line)
             (let ((prev-line (nth (1- current-line) lines)))
               (and (>= (length prev-line) 2)
                    (enkan-repl--is-enkan-buffer-name (substring prev-line 2)))))
        (list :start-line (1- current-line)
              :end-line (min (+ current-line 3) (length lines))))
       ;; On status line
       ((and (> current-line 1)
             (string-match "^    Status:" line)
             (let ((prev-prev-line (nth (- current-line 2) lines)))
               (and (>= (length prev-prev-line) 2)
                    (enkan-repl--is-enkan-buffer-name (substring prev-prev-line 2)))))
        (list :start-line (- current-line 2)
              :end-line (min (+ current-line 2) (length lines))))
       (t nil)))))

(provide 'enkan-repl-sessions)
;;; enkan-repl-sessions.el ends here
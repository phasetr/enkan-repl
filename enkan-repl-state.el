;;; enkan-repl-state.el --- Persistent workspace state for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: phasetr <phasetr@gmail.com>
;; URL: https://github.com/phasetr/enkan-repl
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Disk persistence for enkan-repl's workspace state.
;;
;; The primary motivation is the tmux backend: tmux sessions survive Emacs
;; crashes / restarts, so on restart we want to reconcile the in-Emacs
;; workspace bookkeeping (`enkan-repl--workspaces') with the live tmux
;; server.  For the eat backend the persisted state is informational only
;; (eat sessions die with Emacs).
;;
;; Format (Emacs Lisp Data, written via `prin1' / read via `read'):
;;
;;   ((:schema-version 1)
;;    (:saved-at        "2026-05-09T11:23:45+0900")
;;    (:current         "01")
;;    (:workspaces      ("01" . PLIST) ("02" . PLIST) ...))
;;
;; PLIST is whatever `enkan-repl--workspaces' stores (currently a plist with
;; :current-project / :session-list / :session-counter / :project-aliases).
;; We dump it verbatim so future schema changes inside the workspace plist
;; require no changes here.
;;
;; Atomic write semantics: write to STATE-FILE.tmp, fsync via close, then
;; rename(2) over STATE-FILE so a kill -9 mid-write cannot truncate the
;; live state file.
;;
;; Reconcile (tmux backend): see `enkan-repl-state-tmux-reconcile' --
;; cross-references each persisted workspace against `tmux has-session' and
;; classifies as A (state+tmux), B (state only), C (tmux only).  Behavior
;; per class is governed by `enkan-repl-state-recovery-policy'.

;;; Code:

(require 'cl-lib)

(defgroup enkan-repl-state nil
  "Persistent state for enkan-repl across Emacs sessions."
  :group 'enkan-repl)

(defcustom enkan-repl-state-file
  (expand-file-name "enkan-repl-state.eld" user-emacs-directory)
  "Path to the file used to persist enkan-repl workspace state.
Atomic writes are used so partial writes from a crash cannot corrupt the
file."
  :type 'file
  :group 'enkan-repl-state)

(defcustom enkan-repl-state-recovery-policy 'reattach
  "How to behave when persisted state and live tmux server disagree.

`reattach' (default) -- restore only workspaces that exist in both state
                        and tmux; drop the rest from state.  Standard
                        Emacs-crash-and-restart case.
`recreate'           -- restore all workspaces from state, recreating any
                        tmux sessions that are missing.
`prompt'             -- ask the user per-workspace what to do.
`ignore'             -- do not load state at all (persistence effectively
                        disabled for this session)."
  :type '(choice (const reattach)
                 (const recreate)
                 (const prompt)
                 (const ignore))
  :group 'enkan-repl-state)

(defconst enkan-repl-state--schema-version 1
  "On-disk schema version for `enkan-repl-state-file'.")

;;;; Forward declarations

(declare-function enkan-repl--terminal-tmux--has-session
                  "enkan-repl-terminal" (session))
(declare-function enkan-repl--terminal-tmux--workspace-session
                  "enkan-repl-terminal" ())
(defvar enkan-repl-tmux-session-prefix)
(defvar enkan-repl--workspaces)
(defvar enkan-repl--current-workspace)

;;;; Pure helpers

(defun enkan-repl-state--build-payload (workspaces current)
  "Build the Lisp object to be persisted from WORKSPACES and CURRENT.
Returns a plist of (:schema-version :saved-at :current :workspaces)."
  (list :schema-version enkan-repl-state--schema-version
        :saved-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
        :current current
        :workspaces (copy-tree workspaces)))

(defun enkan-repl-state--validate-payload (payload)
  "Return PAYLOAD if it look like a valid persisted state plist, else nil.
Required keys: :schema-version (integer), :workspaces (alist)."
  (and (listp payload)
       (integerp (plist-get payload :schema-version))
       (listp (plist-get payload :workspaces))
       payload))

;;;; Atomic file IO

(defun enkan-repl-state-save (&optional file)
  "Write current `enkan-repl--workspaces' state to FILE atomically.
FILE defaults to `enkan-repl-state-file'.
Returns the file path on success, nil on failure (file IO error)."
  (let* ((target (or file enkan-repl-state-file))
         (tmpfile (concat target ".tmp"))
         (payload (enkan-repl-state--build-payload
                   (and (boundp 'enkan-repl--workspaces) enkan-repl--workspaces)
                   (and (boundp 'enkan-repl--current-workspace)
                        enkan-repl--current-workspace))))
    (condition-case err
        (progn
          (with-temp-file tmpfile
            (let ((print-length nil)
                  (print-level nil)
                  (print-circle nil))
              (prin1 payload (current-buffer))
              (insert "\n")))
          (rename-file tmpfile target t)
          target)
      (error
       (when (file-exists-p tmpfile)
         (ignore-errors (delete-file tmpfile)))
       (message "enkan-repl-state-save failed: %s" (error-message-string err))
       nil))))

(defun enkan-repl-state-load (&optional file)
  "Read persisted state from FILE.
FILE defaults to `enkan-repl-state-file'.
Returns the validated payload plist, or nil when the file does not exist
or is malformed."
  (let ((target (or file enkan-repl-state-file)))
    (when (file-readable-p target)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents target)
            (goto-char (point-min))
            (let ((payload (read (current-buffer))))
              (or (enkan-repl-state--validate-payload payload)
                  (progn
                    (message "enkan-repl-state-load: malformed state in %s"
                             target)
                    nil))))
        (error
         (message "enkan-repl-state-load failed: %s" (error-message-string err))
         nil)))))

;;;; Reconcile (tmux backend)

(defun enkan-repl-state--classify (state-workspaces)
  "Classify STATE-WORKSPACES against live tmux sessions.
Returns a plist with three lists:
  :both        workspace IDs present in state AND in tmux (reattach candidates)
  :state-only  workspace IDs present only in state (tmux session missing)
  :tmux-only   tmux session names present only on the server"
  (let* ((prefix (or (and (boundp 'enkan-repl-tmux-session-prefix)
                          enkan-repl-tmux-session-prefix)
                     "enkan-"))
         (state-ids (mapcar #'car state-workspaces))
         (state-sessions (mapcar (lambda (id) (concat prefix id)) state-ids))
         (live-sessions (enkan-repl-state--list-live-tmux-sessions prefix))
         both state-only tmux-only)
    (dolist (id state-ids)
      (let ((session (concat prefix id)))
        (if (member session live-sessions)
            (push id both)
          (push id state-only))))
    (dolist (session live-sessions)
      (unless (member session state-sessions)
        (push session tmux-only)))
    (list :both (nreverse both)
          :state-only (nreverse state-only)
          :tmux-only (nreverse tmux-only))))

(defun enkan-repl-state--list-live-tmux-sessions (prefix)
  "Return list of tmux session names beginning with PREFIX.
Returns nil when tmux is unavailable or no server is running."
  (when (and (fboundp 'enkan-repl--terminal-tmux--call))
    (let ((out (ignore-errors
                 (funcall (intern "enkan-repl--terminal-tmux--call")
                          (list "list-sessions" "-F" "#{session_name}")
                          t))))
      (when (stringp out)
        (cl-remove-if-not
         (lambda (s) (string-prefix-p prefix s))
         (split-string out "\n" t))))))

(defun enkan-repl-state-tmux-reconcile (&optional file)
  "Load persisted state from FILE and reconcile with the live tmux server.

Behavior is governed by `enkan-repl-state-recovery-policy':
- `reattach': restore only workspaces present in both state and tmux.
- `recreate': restore all workspaces from state (caller may choose to
              recreate missing tmux sessions).
- `prompt'  : query the user per-workspace.
- `ignore'  : do nothing.

Returns a plist describing the reconciliation result:
  (:loaded-workspaces ALIST :restored IDS :dropped IDS :orphan-tmux SESSIONS
   :current CURRENT)."
  (interactive)
  (let ((payload (enkan-repl-state-load file)))
    (when (and payload (not (eq enkan-repl-state-recovery-policy 'ignore)))
      (let* ((state-ws (plist-get payload :workspaces))
             (cls (enkan-repl-state--classify state-ws))
             (both (plist-get cls :both))
             (tmux-only (plist-get cls :tmux-only))
             (keep-ids
              (pcase enkan-repl-state-recovery-policy
                ('reattach both)
                ('recreate (mapcar #'car state-ws))
                ('prompt
                 (cl-remove-if-not
                  (lambda (id)
                    (yes-or-no-p
                     (format "Restore workspace %s (tmux session %smissing)? "
                             id (if (member id both) "" "is "))))
                  (mapcar #'car state-ws))))))
        (let ((restored
               (cl-remove-if-not (lambda (kv) (member (car kv) keep-ids))
                                 state-ws))
              (dropped
               (cl-set-difference (mapcar #'car state-ws) keep-ids
                                  :test #'string=)))
          (list :loaded-workspaces restored
                :restored keep-ids
                :dropped dropped
                :orphan-tmux tmux-only
                :current (plist-get payload :current)))))))

(provide 'enkan-repl-state)
;;; enkan-repl-state.el ends here

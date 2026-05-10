;;; enkan-repl-terminal.el --- Terminal backend abstraction for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: phasetr <phasetr@gmail.com>
;; URL: https://github.com/phasetr/enkan-repl
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Terminal backend dispatch layer for enkan-repl.
;;
;; enkan-repl historically coupled directly to the `eat' in-Emacs terminal
;; emulator.  Heavy TUIs (claude code CLI, codex, etc.) overwhelm eat's
;; pure-elisp output processing and freeze Emacs.  To allow swapping the
;; terminal layer for an external multiplexer (tmux) while keeping eat as a
;; rollback path, this file defines a generic dispatch layer:
;;
;;   `enkan-repl--terminal-start'         -- create a new session
;;   `enkan-repl--terminal-send'          -- send text (optionally with newline)
;;   `enkan-repl--terminal-send-key'      -- send a special key (escape/enter/digit)
;;   `enkan-repl--terminal-alive-p'       -- liveness check
;;   `enkan-repl--terminal-list'          -- enumerate sessions of current ws
;;   `enkan-repl--terminal-kill'          -- terminate a session
;;   `enkan-repl--terminal-display'       -- present a session inside Emacs
;;   `enkan-repl--terminal-find-by-name'  -- resolve a session by project alias
;;
;; The active backend is selected via `enkan-repl-terminal-backend'.  Phase 1
;; ships only the eat backend, which wraps the existing in-tree behavior
;; without changing semantics.  tmux backend functions are stubs that raise
;; `user-error' until implemented in a later phase.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup enkan-repl-terminal nil
  "Terminal backend selection for enkan-repl."
  :group 'enkan-repl)

(defcustom enkan-repl-terminal-backend 'tmux
  "Terminal backend used by enkan-repl.

`tmux' (default) -- external tmux server.  Heavy TUIs (claude code CLI,
                    codex, etc.) run outside Emacs and cannot block
                    Emacs's main loop with repaint storms.  Sessions
                    survive Emacs restart and can be attached from any
                    terminal application via `enkan-repl-tmux-attach'.
`eat'            -- in-Emacs terminal emulator (legacy / rollback path).
                    Available for users who prefer eat or whose workflow
                    does not involve heavy TUIs.

The value is read at session-creation time; existing sessions are not
migrated when the value changes."
  :type '(choice (const :tag "tmux (external; default)" tmux)
                 (const :tag "eat (in-Emacs; legacy)" eat))
  :group 'enkan-repl-terminal)

;;;; Forward declarations

(declare-function eat "eat" (&optional program arg))
(declare-function eat--send-string "eat" (process string))
(declare-function eat--send-input "eat" (terminal input))
(declare-function enkan-repl--path->buffer-name "enkan-repl-utils" (path &optional instance))
(declare-function enkan-repl--buffer-name->instance "enkan-repl-utils" (name))
(declare-function enkan-repl--buffer-name-matches-workspace
                  "enkan-repl-utils" (name workspace-id))
(declare-function enkan-repl-state--list-live-tmux-sessions
                  "enkan-repl-state" (prefix))
(declare-function enkan-repl-notify-task-complete
                  "enkan-repl-mac-notify" (&optional message))
(defvar eat--process)
(defvar enkan-repl--current-workspace)

;;;; Backend dispatch helpers

(defun enkan-repl--terminal--coerce-id (id)
  "Normalize ID to the form expected by the active backend.

When the active backend is tmux but ID is an Emacs buffer (typically a
mirror buffer found by `buffer-list' lookups), reach through to the
buffer-local `enkan-repl--tmux-mirror-id' so callers can pass either a
buffer or a tmux target string interchangeably.

When the active backend is eat but ID is a string, no conversion is
attempted (eat operates on buffer objects); the original ID is returned
unchanged for the dispatch to either succeed or fail loudly."
  (cond
   ((and (eq enkan-repl-terminal-backend 'tmux)
         (bufferp id))
    (or (buffer-local-value 'enkan-repl--tmux-mirror-id id) id))
   (t id)))

(defun enkan-repl--terminal--dispatch (op-name eat-fn tmux-fn &rest args)
  "Internal helper: dispatch OP-NAME to EAT-FN or TMUX-FN with ARGS.
Selects implementation by `enkan-repl-terminal-backend'.
The first positional argument in ARGS, when non-nil, is coerced via
`enkan-repl--terminal--coerce-id' so callers can pass either a buffer
or a tmux target string regardless of backend."
  (let ((coerced (when args
                   (cons (enkan-repl--terminal--coerce-id (car args))
                         (cdr args)))))
    (pcase enkan-repl-terminal-backend
      ('eat  (apply eat-fn (or coerced args)))
      ('tmux (apply tmux-fn (or coerced args)))
      (other (user-error "Unknown enkan-repl-terminal-backend: %S (op=%s)"
                         other op-name)))))

;;;; Public API (backend-agnostic)

(defun enkan-repl--terminal-start (dir)
  "Create a new terminal session running in DIR.
Return a backend-specific identifier (eat: buffer object;
tmux: target string like \"enkan-01:lat\")."
  (enkan-repl--terminal--dispatch
   'start
   #'enkan-repl--terminal-eat-start
   #'enkan-repl--terminal-tmux-start
   dir))

(defun enkan-repl--terminal-send (id text &optional newline)
  "Send TEXT to terminal ID.  When NEWLINE is non-nil, append CR.
Return non-nil on success."
  (enkan-repl--terminal--dispatch
   'send
   #'enkan-repl--terminal-eat-send
   #'enkan-repl--terminal-tmux-send
   id text newline))

(defun enkan-repl--terminal-send-key (id key)
  "Send special KEY to terminal ID.
KEY is one of: `escape', `enter', or an integer 1..9.
Return non-nil on success."
  (enkan-repl--terminal--dispatch
   'send-key
   #'enkan-repl--terminal-eat-send-key
   #'enkan-repl--terminal-tmux-send-key
   id key))

(defun enkan-repl--terminal-alive-p (id)
  "Return non-nil if terminal ID is alive."
  (if (and (eq enkan-repl-terminal-backend 'tmux)
           (bufferp id))
      (enkan-repl--terminal-tmux-mirror-buffer-alive-p id)
    (enkan-repl--terminal--dispatch
     'alive-p
     #'enkan-repl--terminal-eat-alive-p
     #'enkan-repl--terminal-tmux-alive-p
     id)))

(defun enkan-repl--terminal-list ()
  "Return list of identifiers for sessions in current workspace."
  (enkan-repl--terminal--dispatch
   'list
   #'enkan-repl--terminal-eat-list
   #'enkan-repl--terminal-tmux-list))

(defun enkan-repl--terminal-kill (id)
  "Terminate terminal ID."
  (enkan-repl--terminal--dispatch
   'kill
   #'enkan-repl--terminal-eat-kill
   #'enkan-repl--terminal-tmux-kill
   id))

(defun enkan-repl--terminal-display (id)
  "Present terminal ID inside Emacs in the current window."
  (enkan-repl--terminal--dispatch
   'display
   #'enkan-repl--terminal-eat-display
   #'enkan-repl--terminal-tmux-display
   id))

(defun enkan-repl--terminal-id-instance (id)
  "Return the multi-instance index (integer, 1-based) of terminal ID.
Returns 1 when ID has no explicit instance suffix."
  (enkan-repl--terminal--dispatch
   'id-instance
   #'enkan-repl--terminal-eat-id-instance
   #'enkan-repl--terminal-tmux-id-instance
   id))

;;;; eat backend

(defun enkan-repl--terminal-eat--live-process (buffer)
  "Return BUFFER's live eat process, or nil."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((proc (or (and (boundp 'eat--process) eat--process)
                      (get-buffer-process buffer))))
        (and proc (process-live-p proc) proc)))))

(defun enkan-repl--terminal-eat-start (dir)
  "Eat backend: create eat session in DIR and rename to enkan buffer name.
Return the eat buffer."
  (require 'eat)
  (let* ((default-directory (file-name-as-directory dir))
         (target-name (enkan-repl--path->buffer-name dir))
         (eat-buffer (eat)))
    (when eat-buffer
      (with-current-buffer eat-buffer
        ;; UNIQUE flag (t) appends <2>, <3>... if target-name exists.
        ;; Multi-instance handling for the same path lives in the parser
        ;; functions in enkan-repl-utils (see docs/terminal-backend-abstraction.md).
        (rename-buffer target-name t)))
    eat-buffer))

(defun enkan-repl--terminal-eat-send (id text &optional newline)
  "Eat backend: send TEXT to buffer ID, optionally appending CR.
NEWLINE non-nil appends a CR after TEXT."
  (when (and id (buffer-live-p id))
    (with-current-buffer id
      (let ((proc (or (and (boundp 'eat--process) eat--process)
                      (get-buffer-process id))))
        (when (and proc (process-live-p proc))
          (cond
           ((fboundp 'eat--send-input)
            (eat--send-input nil text)
            (when newline (eat--send-input nil "\r")))
           ((fboundp 'eat--send-string)
            (eat--send-string proc text)
            (when newline (eat--send-string proc "\r")))
           (t
            (process-send-string proc text)
            (when newline (process-send-string proc "\r"))))
          t)))))

(defun enkan-repl--terminal-eat-send-key (id key)
  "Eat backend: send special KEY to buffer ID."
  (let ((s (pcase key
             ('escape "\e")
             ('enter  "\r")
             ((pred integerp) (number-to-string key))
             (_ (user-error "Unsupported terminal key: %S" key)))))
    (enkan-repl--terminal-eat-send id s nil)))

(defun enkan-repl--terminal-eat-alive-p (id)
  "Eat backend: t if buffer ID has a live process."
  (and (enkan-repl--terminal-eat--live-process id) t))

(defun enkan-repl--terminal-eat-list ()
  "Eat backend: list of live enkan buffers in current workspace."
  (let ((current-ws (and (boundp 'enkan-repl--current-workspace)
                         enkan-repl--current-workspace)))
    (when current-ws
      (cl-remove-if-not
       (lambda (buf)
         (and (buffer-live-p buf)
              (buffer-name buf)
              (enkan-repl--buffer-name-matches-workspace
               (buffer-name buf) current-ws)
              (enkan-repl--terminal-eat--live-process buf)))
       (buffer-list)))))

(defun enkan-repl--terminal-eat-kill (id)
  "Eat backend: kill buffer ID."
  (when (and id (buffer-live-p id))
    (kill-buffer id)
    t))

(defun enkan-repl--terminal-eat-display (id)
  "Eat backend: pop to buffer ID in same window (no split)."
  (when (and id (buffer-live-p id))
    (pop-to-buffer-same-window id)))

(defun enkan-repl--terminal-eat-id-instance (id)
  "Eat backend: derive instance index from buffer ID's name (1 if no <N>)."
  (when (and id (buffer-live-p id))
    (or (enkan-repl--buffer-name->instance (buffer-name id)) 1)))

;;;; tmux backend

(defcustom enkan-repl-tmux-executable "tmux"
  "Path to tmux executable used by the tmux backend."
  :type 'string
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-session-prefix "enkan-"
  "Prefix for tmux session names.
The workspace ID (e.g. \"01\") is appended, yielding session names
like \"enkan-01\"."
  :type 'string
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-command-timeout 2.0
  "Maximum seconds to wait for synchronous tmux control commands.
This applies to lightweight control operations such as session/window
enumeration, send-keys, and kill commands.  Pane capture uses the separate
`enkan-repl-tmux-mirror-capture-timeout'."
  :type 'number
  :group 'enkan-repl-terminal)

(defun enkan-repl--terminal-tmux--workspace-session ()
  "Return the tmux session name corresponding to current workspace, or nil."
  (when (and (boundp 'enkan-repl--current-workspace)
             enkan-repl--current-workspace)
    (concat enkan-repl-tmux-session-prefix enkan-repl--current-workspace)))

(defun enkan-repl--terminal-tmux--call (args &optional capture)
  "Run tmux with ARGS (list of strings).
When CAPTURE is non-nil, return stdout as a string (trailing newline stripped),
or nil on non-zero exit.  Otherwise return t on zero exit, nil on non-zero."
  (unless (executable-find enkan-repl-tmux-executable)
    (user-error "Tmux executable not found: %s" enkan-repl-tmux-executable))
  (let* ((timeout (and (numberp enkan-repl-tmux-command-timeout)
                       (> enkan-repl-tmux-command-timeout 0)
                       enkan-repl-tmux-command-timeout))
         (deadline (and timeout
                        (+ (float-time) timeout)))
         (output-buffer (and capture
                             (generate-new-buffer " *enkan-repl tmux call*")))
         (done nil)
         (status nil)
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "enkan-tmux-call"
                 :buffer output-buffer
                 :command (cons enkan-repl-tmux-executable args)
                 :connection-type 'pipe
                 :noquery t
                 :sentinel (lambda (proc _event)
                             (when (memq (process-status proc) '(exit signal))
                               (setq status (process-exit-status proc))
                               (setq done t)))))
          (while (and (not done)
                      (process-live-p process)
                      (or (not deadline) (< (float-time) deadline)))
            (accept-process-output process 0.05)
            (when (and (not done)
                       (memq (process-status process) '(exit signal)))
              (setq status (process-exit-status process))
              (setq done t)))
          (unless done
            (when (process-live-p process)
              (delete-process process))
            (setq status 'timeout))
          (cond
           ((not (and (integerp status) (zerop status))) nil)
           (capture
            (with-current-buffer output-buffer
              (let ((s (buffer-substring-no-properties
                        (point-min) (point-max))))
                (if (string-suffix-p "\n" s)
                    (substring s 0 -1)
                  s))))
           (t t)))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun enkan-repl--terminal-tmux--has-session (session)
  "Return non-nil if tmux SESSION exists."
  (enkan-repl--terminal-tmux--call (list "has-session" "-t" session)))

(defun enkan-repl--terminal-tmux--list-windows (session)
  "Return list of window-name strings in SESSION (empty list if no session)."
  (let ((out (enkan-repl--terminal-tmux--call
              (list "list-windows" "-t" session "-F" "#{window_name}")
              t)))
    (if (and out (not (string-empty-p out)))
        (split-string out "\n" t)
      nil)))

(defun enkan-repl--terminal-tmux--list-window-cwds (session)
  "Return list of (WINDOW . CWD) pairs in tmux SESSION."
  (let ((out (enkan-repl--terminal-tmux--call
              (list "list-windows" "-t" session "-F"
                    "#{window_name}\t#{pane_current_path}")
              t)))
    (when (and out (not (string-empty-p out)))
      (delq
       nil
       (mapcar
        (lambda (line)
          (pcase-let ((`(,window ,cwd) (split-string line "\t")))
            (when (and window cwd (not (string-empty-p window)))
              (cons window (unless (string-empty-p cwd) cwd)))))
        (split-string out "\n" t))))))

(defun enkan-repl--terminal-tmux--derive-base-name (dir)
  "Derive a base window name from DIR (file-name-nondirectory of cleaned dir)."
  (let* ((clean (directory-file-name (expand-file-name dir)))
         (base (file-name-nondirectory clean)))
    (if (string-empty-p base) "root" base)))

(defun enkan-repl--terminal-tmux--next-instance-name (session base)
  "Return a window name for SESSION based on BASE that does not collide.
Tries BASE, then BASE-2, BASE-3, ... until an unused name is found."
  (let ((existing (enkan-repl--terminal-tmux--list-windows session))
        (n 1)
        (candidate base))
    (while (member candidate existing)
      (setq n (1+ n))
      (setq candidate (format "%s-%d" base n)))
    candidate))

(defun enkan-repl--terminal-tmux--make-id (session window)
  "Return tmux target identifier for SESSION:WINDOW."
  (format "%s:%s" session window))

(defun enkan-repl--terminal-tmux--id-window (id)
  "Return the window component of tmux ID (after the colon)."
  (when (and (stringp id) (string-match ":\\(.+\\)$" id))
    (match-string 1 id)))

(defun enkan-repl--terminal-tmux--id-session (id)
  "Return the session component of tmux ID (before the colon)."
  (when (and (stringp id) (string-match "^\\([^:]+\\):" id))
    (match-string 1 id)))

(defun enkan-repl--terminal-tmux-start (dir)
  "Tmux backend: start a new session/window in DIR for current workspace.
Ensures the workspace's tmux session exists (creating it on demand) and
then adds a window for DIR with a non-colliding name.  Returns the tmux
target identifier (e.g. \"enkan-01:lat\")."
  (let* ((session (or (enkan-repl--terminal-tmux--workspace-session)
                      (user-error "No current workspace; cannot start tmux session")))
         (base (enkan-repl--terminal-tmux--derive-base-name dir))
         (cdir (expand-file-name dir)))
    (enkan-repl--terminal-tmux--ensure-bell-monitor)
    (cond
     ;; Session does not exist: create it with the first window in DIR.
     ((not (enkan-repl--terminal-tmux--has-session session))
      (unless (enkan-repl--terminal-tmux--call
               (list "new-session" "-d" "-s" session "-c" cdir "-n" base))
        (user-error "Tmux new-session failed for %s" session))
      (enkan-repl--terminal-tmux--make-id session base))
     ;; Session exists: add a new window with a non-colliding name.
     (t
      (let ((win (enkan-repl--terminal-tmux--next-instance-name session base)))
        (unless (enkan-repl--terminal-tmux--call
                 (list "new-window" "-t" session "-n" win "-c" cdir))
          (user-error "Tmux new-window failed for %s:%s" session win))
        (enkan-repl--terminal-tmux--make-id session win))))))

(defun enkan-repl--terminal-tmux-send (id text &optional newline)
  "Tmux backend: send TEXT to ID via send-keys -l.
When NEWLINE is non-nil, follow with an Enter key.  Returns t on success."
  (when (and id text)
    (and (enkan-repl--terminal-tmux--call
          (list "send-keys" "-t" id "-l" text))
         (or (not newline)
             (enkan-repl--terminal-tmux--call
              (list "send-keys" "-t" id "Enter"))))))

(defun enkan-repl--terminal-tmux-send-key (id key)
  "Tmux backend: send special KEY (`escape', `enter', integer 1..9) to ID."
  (let ((arg (pcase key
               ('escape "Escape")
               ('enter  "Enter")
               ((pred integerp) (number-to-string key))
               (_ (user-error "Unsupported terminal key: %S" key)))))
    (enkan-repl--terminal-tmux--call (list "send-keys" "-t" id arg))))

(defun enkan-repl--terminal-tmux-alive-p (id)
  "Tmux backend: t if SESSION:WINDOW described by ID still exists."
  (when id
    (let ((session (enkan-repl--terminal-tmux--id-session id))
          (window  (enkan-repl--terminal-tmux--id-window id)))
      (and session window
           (enkan-repl--terminal-tmux--has-session session)
           (member window (enkan-repl--terminal-tmux--list-windows session))
           t))))

(defun enkan-repl--terminal-tmux-list ()
  "Tmux backend: list all window identifiers in the current workspace's session."
  (let ((session (enkan-repl--terminal-tmux--workspace-session)))
    (when (and session (enkan-repl--terminal-tmux--has-session session))
      (mapcar (lambda (w) (enkan-repl--terminal-tmux--make-id session w))
              (enkan-repl--terminal-tmux--list-windows session)))))

(defun enkan-repl--terminal-tmux-kill (id)
  "Tmux backend: kill the window described by ID."
  (when id
    (enkan-repl--terminal-tmux--call (list "kill-window" "-t" id))))

;;;;; tmux mirror buffer

(defcustom enkan-repl-tmux-mirror t
  "Whether tmux backend mirrors pane content to a read-only Emacs buffer.
When non-nil, a periodic `tmux capture-pane' refresh is scheduled per
mirror buffer."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-auto-refresh nil
  "When non-nil, tmux mirror buffers refresh themselves on an idle timer.
The default is nil because mirror refresh can still involve main-thread buffer
updates.  Use `enkan-repl-tmux-refresh-current' or
`enkan-repl-tmux-refresh-workspace' for explicit refreshes."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-interval 5.0
  "Idle interval (seconds) used when tmux mirror auto-refresh is enabled.
This is ignored unless `enkan-repl-tmux-mirror-auto-refresh' is non-nil."
  :type 'number
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-bell-notify t
  "When non-nil, watch tmux alerts and notify task completion or prompts.
This is independent from mirror auto-refresh.  Permission prompt detection uses
a small bounded capture and refreshes only the target that triggered a
notification."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-bell-notify-interval 2.0
  "Seconds between lightweight tmux bell alert checks."
  :type 'number
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-bell-notify-timeout 0.2
  "Maximum seconds to wait for one tmux bell alert check."
  :type 'number
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-prompt-notify t
  "When non-nil, notify for Claude Code/Codex CLI permission prompts."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-prompt-notify-lines 40
  "Recent tmux lines inspected for permission prompts."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-prompt-notify-max-chars (* 16 1024)
  "Maximum characters inspected per tmux pane for prompt notifications."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-prompt-notify-patterns
  '("permission[[:space:]-]+\\(required\\|requested\\|needed\\)"
    "\\(requires\\|needs\\)[[:space:]]+\\(approval\\|permission\\)"
    "\\(approval\\|permission\\)[[:space:]-]+\\(required\\|requested\\|needed\\)"
    "waiting[[:space:]]+for[[:space:]]+\\(approval\\|permission\\)"
    "\\(allow\\|approve\\|deny\\|reject\\)[[:space:]]+\\(command\\|execution\\|tool\\)"
    "\\(allow\\|approve\\)[^?\n]*\\?"
    "do you want to \\(continue\\|proceed\\|run\\|execute\\|allow\\)"
    "\\b\\(y/n\\|yes/no\\)\\b")
  "Case-insensitive regexps that identify CLI permission prompts."
  :type '(repeat regexp)
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-activity-notify t
  "When non-nil, notify after tmux pane output changes and then settles.
This covers CLIs that do not emit a terminal bell when an answer finishes.
The monitor uses the same small bounded capture as prompt notifications; it
does not re-enable periodic full mirror refresh."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-activity-notify-idle-seconds 1.5
  "Seconds of unchanged alert-capture content before notifying completion."
  :type 'number
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-alert-capture-targets-per-poll 4
  "Maximum number of tmux targets inspected with capture-pane per poll.
Bell flags remain checked for all sessions every poll.  Prompt and activity
notifications use bounded pane captures, so this limit caps worst-case UI
latency when many background panes exist."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-history-lines 160
  "Number of recent tmux lines to capture per refresh.
The mirror is a lightweight status view, not a full transcript.  Keep this
small enough that manual refresh remains cheap after a long idle period."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-display-lines 80
  "Maximum number of lines kept in a tmux mirror buffer."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-max-chars (* 64 1024)
  "Maximum characters to apply to a tmux mirror buffer per refresh.
When captured content is larger than this value, keep the tail of the
capture.  This bounds main-thread work after the asynchronous tmux
process returns."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-compact-noisy-blocks t
  "When non-nil, collapse large diff-like or very long output blocks."
  :type 'boolean
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-noisy-block-threshold 6
  "Minimum consecutive noisy lines collapsed into one mirror summary line."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-max-line-length 240
  "Maximum line length shown verbatim in tmux mirror buffers."
  :type 'integer
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-capture-timeout 3.0
  "Seconds before an in-flight tmux mirror capture is cancelled.
This bounds how long `enkan-repl-tmux-refresh-current' can wait on a stuck
tmux capture process."
  :type 'number
  :group 'enkan-repl-terminal)

(defvar-local enkan-repl--tmux-mirror-id nil
  "Buffer-local: tmux target id this mirror buffer is bound to.")

(defvar-local enkan-repl--tmux-mirror-timer nil
  "Buffer-local: idle timer driving capture-pane refreshes.")

(defvar-local enkan-repl--tmux-mirror-refresh-process nil
  "Buffer-local asynchronous `tmux capture-pane' process.")

(defvar-local enkan-repl--tmux-mirror-cwd-process nil
  "Buffer-local asynchronous tmux pane cwd lookup process.")

(defvar-local enkan-repl--tmux-mirror-state 'idle
  "Buffer-local refresh state for the tmux mirror buffer.")

(defvar-local enkan-repl--tmux-mirror-last-refresh-time nil
  "Buffer-local time of the last successful tmux mirror refresh.")

(defvar-local enkan-repl--tmux-mirror-last-refresh-duration nil
  "Buffer-local duration in seconds of the last tmux mirror refresh.")

(defvar-local enkan-repl--tmux-mirror-last-refresh-bytes nil
  "Buffer-local byte count captured during the last tmux mirror refresh.")

(defvar-local enkan-repl--tmux-mirror-last-content-hash nil
  "Buffer-local hash of the last captured tmux mirror content.")

(defvar enkan-repl--tmux-bell-monitor-timer nil
  "Timer used to poll tmux bell alerts for completion notifications.")

(defvar enkan-repl--tmux-bell-monitor-seen (make-hash-table :test #'equal)
  "Tmux alert keys already notified while their condition remains set.")

(defvar enkan-repl--tmux-bell-monitor-content-state
  (make-hash-table :test #'equal)
  "Per-target small-capture state used for tmux activity notifications.")

(defvar enkan-repl--tmux-bell-monitor-target-cursor 0
  "Round-robin cursor for bounded tmux alert capture polling.")

(defvar enkan-repl-tmux-mirror-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'enkan-repl-tmux-refresh-current)
    map)
  "Keymap for `enkan-repl-tmux-mirror-mode'.")

(define-derived-mode enkan-repl-tmux-mirror-mode special-mode "Enkan-tmux"
  "Major mode for tmux mirror buffers.

\\{enkan-repl-tmux-mirror-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun enkan-repl--terminal-tmux--mirror-visible-p (buffer)
  "Return non-nil when BUFFER is visible in at least one live window."
  (and (buffer-live-p buffer)
       (get-buffer-window-list buffer nil t)
       t))

(defun enkan-repl--terminal-tmux--mirror-status-string ()
  "Return a compact status string for the current tmux mirror buffer."
  (let ((time-str (if enkan-repl--tmux-mirror-last-refresh-time
                      (format-time-string
                       "%H:%M:%S" enkan-repl--tmux-mirror-last-refresh-time)
                    "never"))
        (duration-str (if enkan-repl--tmux-mirror-last-refresh-duration
                          (format "%.0fms"
                                  (* 1000 enkan-repl--tmux-mirror-last-refresh-duration))
                        "-"))
        (bytes-str (if enkan-repl--tmux-mirror-last-refresh-bytes
                       (format "%dB" enkan-repl--tmux-mirror-last-refresh-bytes)
                     "-")))
    (format " tmux %s | %s | last %s | %s | %s "
            (or enkan-repl--tmux-mirror-id "<none>")
            enkan-repl--tmux-mirror-state
            time-str
            bytes-str
            duration-str)))

(defun enkan-repl--terminal-tmux--mirror-update-status ()
  "Update header line status for the current tmux mirror buffer."
  (setq header-line-format
        '(:eval (enkan-repl--terminal-tmux--mirror-status-string)))
  (force-mode-line-update))

(defun enkan-repl--terminal-tmux--mirror-set-state (state)
  "Set tmux mirror refresh STATE and refresh visible status immediately."
  (setq enkan-repl--tmux-mirror-state state)
  (enkan-repl--terminal-tmux--mirror-update-status))

(defun enkan-repl--terminal-tmux--mirror-auto-refresh-enabled-p ()
  "Return non-nil when tmux mirror auto-refresh should run."
  (and enkan-repl-tmux-mirror-auto-refresh
       (numberp enkan-repl-tmux-mirror-interval)
       (> enkan-repl-tmux-mirror-interval 0)))

(defun enkan-repl--terminal-tmux--bell-monitor-enabled-p ()
  "Return non-nil when tmux bell notification monitoring should run."
  (and enkan-repl-tmux-bell-notify
       (numberp enkan-repl-tmux-bell-notify-interval)
       (> enkan-repl-tmux-bell-notify-interval 0)))

(defun enkan-repl--terminal-tmux--bell-monitor-sessions ()
  "Return tmux sessions that should be checked for bell alerts."
  (let ((enkan-repl-tmux-command-timeout
         (if (and (numberp enkan-repl-tmux-bell-notify-timeout)
                  (> enkan-repl-tmux-bell-notify-timeout 0))
             enkan-repl-tmux-bell-notify-timeout
           0.2)))
    (cl-remove-duplicates
     (delq nil
           (if (fboundp 'enkan-repl-state--list-live-tmux-sessions)
               (enkan-repl-state--list-live-tmux-sessions
                enkan-repl-tmux-session-prefix)
             (list (enkan-repl--terminal-tmux--workspace-session))))
     :test #'equal)))

(defun enkan-repl--terminal-tmux--bell-alert-targets (session)
  "Return tmux target ids with a bell alert flag in SESSION."
  (let* ((enkan-repl-tmux-command-timeout
          (if (and (numberp enkan-repl-tmux-bell-notify-timeout)
                   (> enkan-repl-tmux-bell-notify-timeout 0))
              enkan-repl-tmux-bell-notify-timeout
            0.2))
         (out (enkan-repl--terminal-tmux--call
               (list "list-windows" "-t" session "-F"
                     "#{window_name}\t#{window_bell_flag}")
               t)))
    (when (stringp out)
      (delq
       nil
       (mapcar
        (lambda (line)
          (pcase-let ((`(,window ,flag) (split-string line "\t")))
            (when (and window (string= flag "1"))
              (enkan-repl--terminal-tmux--make-id session window))))
        (split-string out "\n" t))))))

(defun enkan-repl--terminal-tmux--all-targets (session)
  "Return all tmux target ids in SESSION."
  (let ((windows (enkan-repl--terminal-tmux--list-windows session)))
    (mapcar (lambda (window)
              (enkan-repl--terminal-tmux--make-id session window))
            windows)))

(defun enkan-repl--terminal-tmux--alert-capture (target)
  "Return a small bounded capture from TARGET for alert detection."
  (let* ((lines (if (and (integerp enkan-repl-tmux-prompt-notify-lines)
                         (> enkan-repl-tmux-prompt-notify-lines 0))
                    enkan-repl-tmux-prompt-notify-lines
                  40))
         (max-chars (and (integerp enkan-repl-tmux-prompt-notify-max-chars)
                         (> enkan-repl-tmux-prompt-notify-max-chars 0)
                         enkan-repl-tmux-prompt-notify-max-chars))
         (enkan-repl-tmux-command-timeout
          (if (and (numberp enkan-repl-tmux-bell-notify-timeout)
                   (> enkan-repl-tmux-bell-notify-timeout 0))
              enkan-repl-tmux-bell-notify-timeout
            0.2))
         (out (enkan-repl--terminal-tmux--call
               (list "capture-pane" "-p" "-J" "-S"
                     (format "-%d" lines)
                     "-t" target)
               t)))
    (when (stringp out)
      (if (and max-chars (> (length out) max-chars))
          (substring out (- max-chars))
        out))))

(defun enkan-repl--terminal-tmux--prompt-content-p (content)
  "Return non-nil when CONTENT looks like a CLI permission prompt."
  (let ((case-fold-search t)
        (patterns enkan-repl-tmux-prompt-notify-patterns)
        matched)
    (while (and patterns (not matched))
      (setq matched (string-match-p (car patterns) content))
      (setq patterns (cdr patterns)))
    matched))

(defun enkan-repl--terminal-tmux--prompt-alert-targets (session)
  "Return tmux target ids in SESSION that look like permission prompts."
  (when enkan-repl-tmux-prompt-notify
    (delq
     nil
     (mapcar
      (lambda (target)
        (let ((content (enkan-repl--terminal-tmux--alert-capture target)))
          (when (and content
                     (enkan-repl--terminal-tmux--prompt-content-p content))
            target)))
      (enkan-repl--terminal-tmux--all-targets session)))))

(defun enkan-repl--terminal-tmux--activity-notify-enabled-p ()
  "Return non-nil when tmux content-settled notifications should run."
  (and enkan-repl-tmux-activity-notify
       (numberp enkan-repl-tmux-activity-notify-idle-seconds)
       (>= enkan-repl-tmux-activity-notify-idle-seconds 0)))

(defun enkan-repl--terminal-tmux--activity-alert-p (target content now)
  "Return non-nil when TARGET CONTENT has changed and then settled by NOW.
The first observation only establishes a baseline.  Later content changes mark
the target active, and a notification is emitted once the small capture remains
unchanged for `enkan-repl-tmux-activity-notify-idle-seconds'."
  (when (and (enkan-repl--terminal-tmux--activity-notify-enabled-p)
             (stringp content))
    (let* ((hash (secure-hash 'sha1 content))
           (state (gethash target
                           enkan-repl--tmux-bell-monitor-content-state))
           (previous-hash (plist-get state :hash))
           (changed-at (or (plist-get state :changed-at) now))
           (notified (plist-get state :notified)))
      (cond
       ((null state)
        (puthash target
                 (list :hash hash :changed-at now :notified t)
                 enkan-repl--tmux-bell-monitor-content-state)
        nil)
       ((not (equal hash previous-hash))
        (puthash target
                 (list :hash hash :changed-at now :notified nil)
                 enkan-repl--tmux-bell-monitor-content-state)
        nil)
       ((not notified)
        (let ((settled-p
               (>= (float-time (time-subtract now changed-at))
                   enkan-repl-tmux-activity-notify-idle-seconds)))
          (when settled-p
            (puthash target
                     (list :hash hash :changed-at changed-at :notified t)
                     enkan-repl--tmux-bell-monitor-content-state))
          settled-p))
       (t nil)))))

(defun enkan-repl--terminal-tmux--alert-target-slice (targets)
  "Return the bounded round-robin subset of TARGETS to capture this poll."
  (let* ((targets (cl-remove-duplicates targets :test #'equal))
         (len (length targets))
         (limit (if (and (integerp enkan-repl-tmux-alert-capture-targets-per-poll)
                         (> enkan-repl-tmux-alert-capture-targets-per-poll 0))
                    enkan-repl-tmux-alert-capture-targets-per-poll
                  len)))
    (cond
     ((zerop len) nil)
     ((>= limit len)
      (setq enkan-repl--tmux-bell-monitor-target-cursor 0)
      targets)
     (t
      (let ((start (mod enkan-repl--tmux-bell-monitor-target-cursor len))
            selected)
        (dotimes (i limit)
          (push (nth (mod (+ start i) len) targets) selected))
        (setq enkan-repl--tmux-bell-monitor-target-cursor
              (mod (+ start limit) len))
        (nreverse selected))))))

(defun enkan-repl--terminal-tmux--alert-key (kind target)
  "Return a stable de-duplication key for alert KIND and TARGET."
  (format "%s\t%s" kind target))

(defun enkan-repl--terminal-tmux--refresh-alert-target (target)
  "Force-refresh the mirror buffer for alert TARGET."
  (when enkan-repl-tmux-mirror
    (let ((buf (enkan-repl--terminal-tmux--mirror-make target t)))
      (when buf
        (enkan-repl--terminal-tmux--mirror-refresh buf t)))))

(defun enkan-repl--terminal-tmux--bell-notify (target &optional kind)
  "Refresh and notify that tmux TARGET emitted alert KIND."
  (enkan-repl--terminal-tmux--refresh-alert-target target)
  (if (fboundp 'enkan-repl-notify-task-complete)
      (enkan-repl-notify-task-complete
       (pcase kind
         ('prompt (format "tmux permission requested: %s" target))
         ('activity (format "tmux task completed: %s" target))
         (_ (format "tmux task completed: %s" target))))
    (message "%s"
             (pcase kind
               ('prompt (format "tmux permission requested: %s" target))
               ('activity (format "tmux task completed: %s" target))
               (_ (format "tmux task completed: %s" target))))))

(defun enkan-repl--terminal-tmux--bell-monitor-poll ()
  "Poll tmux alert flags/prompts and notify newly observed alerts."
  (when (enkan-repl--terminal-tmux--bell-monitor-enabled-p)
    (let ((active (make-hash-table :test #'equal))
          (seen-targets (make-hash-table :test #'equal))
          targets-to-capture
          (now (current-time)))
      (dolist (session (enkan-repl--terminal-tmux--bell-monitor-sessions))
        (dolist (target (enkan-repl--terminal-tmux--bell-alert-targets session))
          (let ((key (enkan-repl--terminal-tmux--alert-key 'bell target)))
            (puthash key t active)
            (unless (gethash key enkan-repl--tmux-bell-monitor-seen)
              (puthash key t enkan-repl--tmux-bell-monitor-seen)
              (enkan-repl--terminal-tmux--bell-notify target 'bell))))
        (dolist (target (enkan-repl--terminal-tmux--all-targets session))
          (puthash target t seen-targets)
          (push target targets-to-capture)))
      (dolist (target (enkan-repl--terminal-tmux--alert-target-slice
                       (nreverse targets-to-capture)))
        (when (gethash target seen-targets)
          (let* ((content (enkan-repl--terminal-tmux--alert-capture target))
                 (prompt-p (and content
                                enkan-repl-tmux-prompt-notify
                                (enkan-repl--terminal-tmux--prompt-content-p
                                 content)))
                 (activity-p
                  (enkan-repl--terminal-tmux--activity-alert-p
                   target content now)))
            (when prompt-p
              (let ((key (enkan-repl--terminal-tmux--alert-key 'prompt target)))
                (puthash key t active)
                (unless (gethash key enkan-repl--tmux-bell-monitor-seen)
                  (puthash key t enkan-repl--tmux-bell-monitor-seen)
                  (enkan-repl--terminal-tmux--bell-notify target 'prompt))))
            (when activity-p
              (enkan-repl--terminal-tmux--bell-notify target 'activity)))))
      (maphash
       (lambda (key _)
         (unless (gethash key active)
           (remhash key enkan-repl--tmux-bell-monitor-seen)))
       enkan-repl--tmux-bell-monitor-seen)
      (maphash
       (lambda (target _)
         (unless (gethash target seen-targets)
           (remhash target enkan-repl--tmux-bell-monitor-content-state)))
       enkan-repl--tmux-bell-monitor-content-state))))

(defun enkan-repl--terminal-tmux--ensure-bell-monitor ()
  "Start tmux bell notification monitoring when enabled."
  (when (and (enkan-repl--terminal-tmux--bell-monitor-enabled-p)
             (not enkan-repl--tmux-bell-monitor-timer))
    (setq enkan-repl--tmux-bell-monitor-timer
          (run-with-timer
           enkan-repl-tmux-bell-notify-interval
           enkan-repl-tmux-bell-notify-interval
           #'enkan-repl--terminal-tmux--bell-monitor-poll))))

(defun enkan-repl-tmux-stop-bell-monitor ()
  "Stop tmux bell notification monitoring."
  (interactive)
  (when enkan-repl--tmux-bell-monitor-timer
    (cancel-timer enkan-repl--tmux-bell-monitor-timer)
    (setq enkan-repl--tmux-bell-monitor-timer nil))
  (clrhash enkan-repl--tmux-bell-monitor-seen)
  (clrhash enkan-repl--tmux-bell-monitor-content-state)
  (setq enkan-repl--tmux-bell-monitor-target-cursor 0))

(defun enkan-repl--terminal-tmux-mirror-buffer-alive-p (buffer)
  "Return non-nil when BUFFER is an open tmux mirror.
This is intentionally an Emacs-local check.  UI paths call this while scanning
`buffer-list', so it must not synchronously query tmux."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (buffer-local-boundp 'enkan-repl--tmux-mirror-id buffer)
       (buffer-local-value 'enkan-repl--tmux-mirror-id buffer)
       (not (and (buffer-local-boundp 'enkan-repl--tmux-mirror-state buffer)
                 (eq (buffer-local-value 'enkan-repl--tmux-mirror-state buffer)
                     'closed)))))

(defun enkan-repl--terminal-tmux-kill-workspace (workspace-id)
  "Kill the tmux session backing WORKSPACE-ID.
Returns non-nil when a live tmux session was killed."
  (let ((session (and workspace-id
                      (concat enkan-repl-tmux-session-prefix workspace-id))))
    (when (and session (enkan-repl--terminal-tmux--has-session session))
      (enkan-repl--terminal-tmux--call (list "kill-session" "-t" session)))))

(defun enkan-repl--terminal-tmux--window-at-bottom-p (window)
  "Return non-nil when WINDOW is displaying the current buffer bottom."
  (or (= (window-point window) (point-max))
      (pos-visible-in-window-p (point-max) window t)))

(defun enkan-repl--terminal-tmux--mirror-window-state (buffer)
  "Capture scroll state for windows currently displaying BUFFER."
  (with-current-buffer buffer
    (mapcar
     (lambda (window)
       (list :window window
             :at-bottom (enkan-repl--terminal-tmux--window-at-bottom-p window)
             :start (window-start window)
             :point (window-point window)))
     (get-buffer-window-list buffer nil t))))

(defun enkan-repl--terminal-tmux--restore-window-state (buffer states)
  "Restore BUFFER window scroll STATES after a mirror refresh.
Windows that were already showing the bottom continue following the
bottom.  Other windows keep their previous line and column."
  (save-selected-window
    (with-current-buffer buffer
      (dolist (state states)
        (let ((window (plist-get state :window)))
          (when (and (window-live-p window)
                     (eq (window-buffer window) buffer))
            (if (plist-get state :at-bottom)
                (progn
                  (set-window-point window (point-max)))
              (let ((start (min (point-max) (plist-get state :start)))
                    (point (min (point-max) (plist-get state :point))))
                (set-window-point window point)
                (set-window-start window start)))))))))

(defun enkan-repl--terminal-tmux--replace-mirror-content (content)
  "Replace the current tmux mirror buffer with CONTENT.
This intentionally avoids `replace-buffer-contents'.  Mirror buffers are a
bounded recent-status view, and diffing a stale huge buffer is unnecessary
main-thread work."
  (erase-buffer)
  (insert content))

(defun enkan-repl--terminal-tmux--mirror-fallback-buffer-name (id)
  "Return a non-blocking fallback mirror buffer name for tmux ID."
  (format "*tmux %s*" id))

(defun enkan-repl--terminal-tmux--id-workspace (id)
  "Return workspace id encoded in tmux target ID, or nil.
For example, with `enkan-repl-tmux-session-prefix' set to \"enkan-\",
\"enkan-02:lat\" maps to \"02\"."
  (let ((session (enkan-repl--terminal-tmux--id-session id))
        (prefix enkan-repl-tmux-session-prefix))
    (when (and (stringp session)
               (stringp prefix)
               (string-prefix-p prefix session)
               (> (length session) (length prefix)))
      (substring session (length prefix)))))

(defun enkan-repl--terminal-tmux--mirror-buffer-name-for-path (id path)
  "Return path-based mirror buffer name for tmux ID at PATH.
The name uses the same `*ws:NN enkan:/path/*' form as the eat backend so
buffer-list-based layout and send-target lookup can find tmux mirrors."
  (let ((enkan-repl--current-workspace
         (or (enkan-repl--terminal-tmux--id-workspace id)
             enkan-repl--current-workspace)))
    (enkan-repl--path->buffer-name
     (file-name-as-directory path)
     (enkan-repl--terminal-tmux-id-instance-for-path id path))))

(defun enkan-repl--terminal-tmux--mirror-buffer-name (id &optional path)
  "Return mirror buffer name for tmux ID without calling tmux.
When PATH is non-nil, return the path-based eat-compatible name.
Otherwise return a tmux-id fallback name.  Pane cwd discovery is done
asynchronously by `enkan-repl--terminal-tmux--mirror-make'."
  (if (and path (not (string-empty-p path)))
      (enkan-repl--terminal-tmux--mirror-buffer-name-for-path id path)
    (enkan-repl--terminal-tmux--mirror-fallback-buffer-name id)))

(defun enkan-repl--terminal-tmux--find-mirror-buffer (id)
  "Return an existing tmux mirror buffer for ID, or nil."
  (cl-find-if
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (buffer-local-boundp 'enkan-repl--tmux-mirror-id buffer)
          (equal (buffer-local-value 'enkan-repl--tmux-mirror-id buffer)
                 id)))
   (buffer-list)))

(defun enkan-repl--terminal-tmux--repair-mirror-buffer-name (buffer)
  "Repair BUFFER's workspace prefix from its tmux mirror id.
Return non-nil when BUFFER was renamed.  This uses only Emacs-local metadata
and the existing buffer path; it does not call tmux."
  (when (and (buffer-live-p buffer)
             (buffer-local-boundp 'enkan-repl--tmux-mirror-id buffer))
    (let* ((id (buffer-local-value 'enkan-repl--tmux-mirror-id buffer))
           (workspace (and id
                           (enkan-repl--terminal-tmux--id-workspace id)))
           (path (enkan-repl--buffer-name->path (buffer-name buffer))))
      (when (and workspace path)
        (let* ((enkan-repl--current-workspace workspace)
               (expected-name
                (enkan-repl--path->buffer-name
                 path
                 (enkan-repl--terminal-tmux-id-instance id))))
          (unless (string= (buffer-name buffer) expected-name)
            (with-current-buffer buffer
              (rename-buffer expected-name t))
            t))))))

;;;###autoload
(defun enkan-repl-tmux-repair-mirror-buffer-names ()
  "Repair tmux mirror buffer workspace prefixes from their tmux target ids.
This fixes already-created mirror buffers whose `*ws:NN enkan:...*' name was
derived from the current Emacs workspace instead of the tmux target session."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (enkan-repl--terminal-tmux--repair-mirror-buffer-name buffer)
        (setq count (1+ count))))
    (message "Repaired %d tmux mirror buffer name(s)" count)
    count))

(defun enkan-repl--terminal-tmux--pane-cwd-async (id callback)
  "Resolve tmux pane cwd for ID asynchronously, then call CALLBACK.
CALLBACK receives the cwd string, or nil on failure."
  (unless (executable-find enkan-repl-tmux-executable)
    (user-error "Tmux executable not found: %s" enkan-repl-tmux-executable))
  (let* ((output-buffer (generate-new-buffer " *enkan-repl tmux cwd*"))
         (command (list enkan-repl-tmux-executable
                        "display-message" "-p" "-t" id
                        "#{pane_current_path}")))
    (make-process
     :name (format "enkan-tmux-cwd %s" id)
     :buffer output-buffer
     :command command
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let* ((status (process-exit-status process))
                (content (when (buffer-live-p output-buffer)
                           (with-current-buffer output-buffer
                             (string-trim
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))
           (funcall callback
                    (and (zerop status)
                         content
                         (not (string-empty-p content))
                         content))))))))

(defun enkan-repl--terminal-tmux--mirror-rename-for-cwd (buffer id cwd)
  "Rename mirror BUFFER for ID using CWD when it is still current."
  (when (and cwd (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (equal enkan-repl--tmux-mirror-id id)
        (let ((name (enkan-repl--terminal-tmux--mirror-buffer-name id cwd)))
          (unless (string= (buffer-name buffer) name)
            (rename-buffer name t)))))))

(defun enkan-repl--terminal-tmux--mirror-start-cwd-lookup (buffer id)
  "Start asynchronous cwd lookup for mirror BUFFER bound to ID."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (and enkan-repl--tmux-mirror-cwd-process
                   (process-live-p enkan-repl--tmux-mirror-cwd-process))
        (let ((target-buffer buffer)
              (target-id id))
          (setq enkan-repl--tmux-mirror-cwd-process
                (enkan-repl--terminal-tmux--pane-cwd-async
                 id
                 (lambda (cwd)
                   (when (buffer-live-p target-buffer)
                     (with-current-buffer target-buffer
                       (setq enkan-repl--tmux-mirror-cwd-process nil)))
                   (enkan-repl--terminal-tmux--mirror-rename-for-cwd
                    target-buffer target-id cwd)))))))))

(defun enkan-repl--terminal-tmux--tail-append (content chunk max-chars)
  "Append CHUNK to CONTENT, keeping at most MAX-CHARS characters.
When MAX-CHARS is nil or non-positive, return the full concatenation."
  (if (and (integerp max-chars) (> max-chars 0))
      (cond
       ((>= (length chunk) max-chars)
        (substring chunk (- max-chars)))
       (t
        (let ((combined (concat content chunk)))
          (if (> (length combined) max-chars)
              (substring combined (- max-chars))
            combined))))
    (concat content chunk)))

(defun enkan-repl--terminal-tmux--tail-lines (content max-lines)
  "Return CONTENT limited to its final MAX-LINES lines."
  (if (and (integerp max-lines) (> max-lines 0))
      (let* ((lines (split-string content "\n"))
             (count (length lines)))
        (string-join
         (if (> count max-lines)
             (last lines max-lines)
           lines)
         "\n"))
    content))

(defun enkan-repl--terminal-tmux--max-line-length ()
  "Return the configured mirror line length limit, or nil."
  (and (integerp enkan-repl-tmux-mirror-max-line-length)
       (> enkan-repl-tmux-mirror-max-line-length 0)
       enkan-repl-tmux-mirror-max-line-length))

(defun enkan-repl--terminal-tmux--truncate-line (line max-length)
  "Return LINE shortened to MAX-LENGTH characters when needed."
  (if (and max-length (> (length line) max-length))
      (format "%s [enkan-repl: omitted %d chars]"
              (substring line 0 max-length)
              (- (length line) max-length))
    line))

(defun enkan-repl--terminal-tmux--noisy-line-p (line)
  "Return non-nil when LINE looks like generated diff/code noise."
  (let ((max-length (enkan-repl--terminal-tmux--max-line-length)))
    (or (and max-length
             (> (length line) max-length))
        (string-match-p
         (rx string-start (* space)
             (or "diff --git" "index " "@@ " "+++ " "--- "
                 "```" "~~~"))
         line)
        (string-match-p
         (rx string-start (* space) (or "+" "-")
             (not (any "\n")))
         line))))

(defun enkan-repl--terminal-tmux--noisy-block-threshold ()
  "Return the configured noisy block threshold."
  (if (and (integerp enkan-repl-tmux-mirror-noisy-block-threshold)
           (> enkan-repl-tmux-mirror-noisy-block-threshold 0))
      enkan-repl-tmux-mirror-noisy-block-threshold
    6))

(defun enkan-repl--terminal-tmux--flush-noisy-lines (lines output)
  "Append compacted or verbatim noisy LINES to OUTPUT."
  (let ((threshold (enkan-repl--terminal-tmux--noisy-block-threshold)))
    (if (>= (length lines) threshold)
        (cons (format "[enkan-repl: omitted %d noisy/diff line(s)]"
                      (length lines))
              output)
      (append lines output))))

(defun enkan-repl--terminal-tmux--compact-noisy-content (content)
  "Collapse large noisy blocks in CONTENT."
  (if (not enkan-repl-tmux-mirror-compact-noisy-blocks)
      content
    (let ((output nil)
          (noisy nil)
          (max-length (enkan-repl--terminal-tmux--max-line-length)))
      (dolist (line (split-string content "\n"))
        (if (enkan-repl--terminal-tmux--noisy-line-p line)
            (push (enkan-repl--terminal-tmux--truncate-line line max-length)
                  noisy)
          (let ((line (enkan-repl--terminal-tmux--truncate-line
                       line max-length)))
            (when noisy
              (setq output
                    (enkan-repl--terminal-tmux--flush-noisy-lines
                     noisy output))
              (setq noisy nil))
            (push line output))))
      (when noisy
        (setq output
              (enkan-repl--terminal-tmux--flush-noisy-lines noisy output)))
      (string-join (reverse output) "\n"))))

(defun enkan-repl--terminal-tmux--prepare-mirror-content (content)
  "Return bounded, display-ready tmux mirror CONTENT."
  (let* ((max-chars (and (integerp enkan-repl-tmux-mirror-max-chars)
                         (> enkan-repl-tmux-mirror-max-chars 0)
                         enkan-repl-tmux-mirror-max-chars))
         (content (if (and max-chars
                           (> (length content) max-chars))
                      (substring content (- max-chars))
                    content)))
    (setq content (enkan-repl--terminal-tmux--compact-noisy-content content))
    (setq content
          (enkan-repl--terminal-tmux--tail-lines
           content enkan-repl-tmux-mirror-display-lines))
    (if (and max-chars (> (length content) max-chars))
        (substring content (- max-chars))
      content)))

(defun enkan-repl--terminal-tmux--capture-pane-async (id lines callback)
  "Capture tmux ID asynchronously and call CALLBACK with content and status.
LINES is the maximum scrollback to include.  CALLBACK receives two
arguments: CONTENT, or nil on failure, and the tmux process exit status."
  (unless (executable-find enkan-repl-tmux-executable)
    (user-error "Tmux executable not found: %s" enkan-repl-tmux-executable))
  (let* ((command (list enkan-repl-tmux-executable
                        "capture-pane" "-p" "-J" "-S"
                        (format "-%d" (max 0 lines))
                        "-t" id))
         (max-chars (and (integerp enkan-repl-tmux-mirror-max-chars)
                         (> enkan-repl-tmux-mirror-max-chars 0)
                         enkan-repl-tmux-mirror-max-chars))
         (content "")
         (done nil)
         timer
         process)
    (cl-labels
        ((finish
          (status)
          (unless done
            (setq done t)
            (when timer
              (cancel-timer timer)
              (setq timer nil))
            (funcall callback
                     (and (integerp status) (zerop status) content)
                     status))))
      (setq process
            (make-process
             :name (format "enkan-tmux-capture %s" id)
             :buffer nil
             :command command
             :connection-type 'pipe
             :noquery t
             :filter (lambda (_process chunk)
                       (setq content
                             (enkan-repl--terminal-tmux--tail-append
                              content chunk max-chars)))
             :sentinel (lambda (proc _event)
                         (when (memq (process-status proc) '(exit signal))
                           (finish (process-exit-status proc))))))
      (when (and (numberp enkan-repl-tmux-mirror-capture-timeout)
                 (> enkan-repl-tmux-mirror-capture-timeout 0))
        (setq timer
              (run-at-time
               enkan-repl-tmux-mirror-capture-timeout nil
               (lambda ()
                 (unless done
                   (finish 'timeout)
                   (when (process-live-p process)
                     (delete-process process)))))))
      process)))

(defun enkan-repl--terminal-tmux--mirror-mark-closed ()
  "Mark current tmux mirror buffer as closed."
  (enkan-repl--terminal-tmux--mirror-stop (current-buffer))
  (enkan-repl--terminal-tmux--mirror-set-state 'closed)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert "\n[tmux pane closed]\n")))

(defun enkan-repl--terminal-tmux--mirror-apply-content
    (buffer id started content status)
  "Apply async tmux capture CONTENT to BUFFER for ID.
STARTED is the capture start time.  STATUS is the tmux process exit status."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (equal enkan-repl--tmux-mirror-id id)
        (setq enkan-repl--tmux-mirror-refresh-process nil)
        (cond
         ((eq status 'timeout)
          (enkan-repl--terminal-tmux--mirror-set-state 'timed-out))
         ((null content)
          (enkan-repl--terminal-tmux--mirror-mark-closed))
         (t
          (let* ((content
                  (enkan-repl--terminal-tmux--prepare-mirror-content content))
                 (content-hash (secure-hash 'sha1 content))
                 (window-state (enkan-repl--terminal-tmux--mirror-window-state
                                buffer))
                 (inhibit-read-only t))
            (setq enkan-repl--tmux-mirror-last-refresh-time (current-time))
            (setq enkan-repl--tmux-mirror-last-refresh-duration
                  (float-time
                   (time-subtract enkan-repl--tmux-mirror-last-refresh-time
                                  started)))
            (setq enkan-repl--tmux-mirror-last-refresh-bytes
                  (string-bytes content))
            (if (equal content-hash enkan-repl--tmux-mirror-last-content-hash)
                (enkan-repl--terminal-tmux--mirror-set-state 'unchanged)
              (setq enkan-repl--tmux-mirror-last-content-hash content-hash)
              (enkan-repl--terminal-tmux--replace-mirror-content content)
              (enkan-repl--terminal-tmux--restore-window-state
               buffer window-state)
              (enkan-repl--terminal-tmux--mirror-set-state 'fresh)))))))))

(defun enkan-repl--terminal-tmux--mirror-refresh (buffer &optional force)
  "Refresh mirror BUFFER by capturing current tmux pane content.
Skips when BUFFER is dead, hidden, has no bound id, or the tmux pane is gone.
When FORCE is non-nil, refresh even when BUFFER is hidden."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (enkan-repl--terminal-tmux--mirror-update-status)
      (let ((id enkan-repl--tmux-mirror-id))
        (cond
         ((null id))
         ((and (not force)
               (active-minibuffer-window))
          (enkan-repl--terminal-tmux--mirror-set-state 'deferred)
          'deferred)
         ((and (not force)
               (not (enkan-repl--terminal-tmux--mirror-visible-p buffer)))
          (enkan-repl--terminal-tmux--mirror-set-state 'hidden)
          'hidden)
         ((and enkan-repl--tmux-mirror-refresh-process
               (process-live-p enkan-repl--tmux-mirror-refresh-process))
          (enkan-repl--terminal-tmux--mirror-set-state 'refreshing)
          'refreshing)
         (t
          (let ((started (current-time))
                (target-buffer buffer)
                (target-id id))
            (enkan-repl--terminal-tmux--mirror-set-state 'refreshing)
            (setq enkan-repl--tmux-mirror-refresh-process
                  (enkan-repl--terminal-tmux--capture-pane-async
                   id enkan-repl-tmux-mirror-history-lines
                   (lambda (content status)
                     (enkan-repl--terminal-tmux--mirror-apply-content
                      target-buffer target-id started content status))))
            'refreshing)))))))

(defun enkan-repl--terminal-tmux--mirror-stop (buffer)
  "Cancel mirror refresh timer for BUFFER (if any)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when enkan-repl--tmux-mirror-timer
        (cancel-timer enkan-repl--tmux-mirror-timer)
        (setq enkan-repl--tmux-mirror-timer nil)))))

;;;###autoload
(defun enkan-repl-tmux-stop-all-mirrors (&optional kill-buffers)
  "Stop all tmux mirror timers and in-flight mirror processes.
With prefix KILL-BUFFERS, also kill the mirror buffers.  This command does not
call tmux; it only quiets Emacs-side timers and subprocess sentinels."
  (interactive "P")
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-local-boundp 'enkan-repl--tmux-mirror-id buffer)
                 (buffer-local-value 'enkan-repl--tmux-mirror-id buffer))
        (setq count (1+ count))
        (with-current-buffer buffer
          (enkan-repl--terminal-tmux--mirror-stop buffer)
          (when (and enkan-repl--tmux-mirror-refresh-process
                     (process-live-p enkan-repl--tmux-mirror-refresh-process))
            (delete-process enkan-repl--tmux-mirror-refresh-process))
          (setq enkan-repl--tmux-mirror-refresh-process nil)
          (when (and enkan-repl--tmux-mirror-cwd-process
                     (process-live-p enkan-repl--tmux-mirror-cwd-process))
            (delete-process enkan-repl--tmux-mirror-cwd-process))
          (setq enkan-repl--tmux-mirror-cwd-process nil)
          (enkan-repl--terminal-tmux--mirror-set-state 'stopped))
        (when kill-buffers
          (kill-buffer buffer))))
    (message "Stopped %d tmux mirror buffer(s)" count)
    count))

(defun enkan-repl--terminal-tmux--mirror-make (id &optional defer-refresh path)
  "Get or create the mirror buffer for tmux ID and return it.
When tmux mirror auto-refresh is enabled and DEFER-REFRESH is nil, start a
timer and immediately refresh the mirror content.  With the default settings,
mirror buffers never refresh unless explicitly requested.
When PATH is non-nil, use it for the eat-compatible buffer name.  Otherwise
create the buffer without blocking on tmux cwd lookup and rename it later when
the asynchronous lookup returns."
  (let ((buf (or (enkan-repl--terminal-tmux--find-mirror-buffer id)
                 (get-buffer-create
                  (enkan-repl--terminal-tmux--mirror-buffer-name id path)))))
    (enkan-repl--terminal-tmux--ensure-bell-monitor)
    (with-current-buffer buf
      (unless (derived-mode-p 'enkan-repl-tmux-mirror-mode)
        (enkan-repl-tmux-mirror-mode))
      (setq enkan-repl--tmux-mirror-id id)
      (when path
        (enkan-repl--terminal-tmux--mirror-rename-for-cwd buf id path))
      (enkan-repl--terminal-tmux--mirror-update-status)
      (unless path
        (enkan-repl--terminal-tmux--mirror-start-cwd-lookup buf id))
      (when (and (enkan-repl--terminal-tmux--mirror-auto-refresh-enabled-p)
                 (not enkan-repl--tmux-mirror-timer))
        (setq enkan-repl--tmux-mirror-timer
              (run-with-idle-timer
               enkan-repl-tmux-mirror-interval t
               #'enkan-repl--terminal-tmux--mirror-refresh buf)))
      (when (and (enkan-repl--terminal-tmux--mirror-auto-refresh-enabled-p)
                 (not defer-refresh))
        (enkan-repl--terminal-tmux--mirror-refresh buf))
      ;; Stop the timer when buffer is killed.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (enkan-repl--terminal-tmux--mirror-stop buf))
                nil t))
    buf))

(defun enkan-repl--terminal-tmux-display (id)
  "Tmux backend: present ID as a read-only mirror buffer in the current window.
When `enkan-repl-tmux-mirror' is nil, instead message a hint to attach
externally via `enkan-repl-tmux-attach'."
  (cond
   ((not enkan-repl-tmux-mirror)
    (message (concat "tmux mirror is disabled. "
                     "Run M-x enkan-repl-tmux-attach to attach in a terminal,"
                     " or set `enkan-repl-tmux-mirror' to t."))
    nil)
   (t
    (let ((buf (enkan-repl--terminal-tmux--mirror-make id t)))
      (pop-to-buffer-same-window buf)
      (message "tmux mirror displayed; press g or run M-x enkan-repl-tmux-refresh-current to refresh")
      buf))))

;;;###autoload
(defun enkan-repl-tmux-refresh-workspace (&optional quiet)
  "Refresh tmux mirror buffers for the current workspace.
Mirror buffers are created for live tmux windows that do not yet have one.
When QUIET is non-nil, suppress the status message.
Returns the number of refreshed mirror buffers."
  (interactive)
  (unless (eq enkan-repl-terminal-backend 'tmux)
    (user-error "Current terminal backend is not tmux: %S"
                enkan-repl-terminal-backend))
  (unless enkan-repl-tmux-mirror
    (user-error "Tmux mirror is disabled"))
  (let ((ids (enkan-repl--terminal-tmux-list))
        (count 0))
    (dolist (id ids)
      (let ((buf (enkan-repl--terminal-tmux--mirror-make id t)))
        (when buf
          (enkan-repl--terminal-tmux--mirror-refresh buf t)
          (setq count (1+ count)))))
    (unless quiet
      (message "Refreshed %d tmux mirror buffer(s) for workspace %s"
               count
               (or (and (boundp 'enkan-repl--current-workspace)
                        enkan-repl--current-workspace)
                   "<none>")))
    count))

;;;###autoload
(defun enkan-repl-tmux-refresh-current ()
  "Force-refresh the current tmux mirror buffer."
  (interactive)
  (unless enkan-repl--tmux-mirror-id
    (user-error "Current buffer is not a tmux mirror"))
  (let ((state (enkan-repl--terminal-tmux--mirror-refresh (current-buffer) t)))
    (message "%s" (enkan-repl--terminal-tmux--mirror-status-string))
    state))

;;;;; tmux attach helper

(defcustom enkan-repl-tmux-attach-command nil
  "Command used to attach to a tmux session in an external terminal.

When nil, a sensible default is chosen per system-type:
  darwin -> Terminal.app
  other  -> the value of `shell-file-name' as a fallback (no real attach)

When non-nil, must be either:
- a string: a shell command in which \\='%s\\=' is replaced by the
  target tmux session name (e.g. \"open -a iTerm \\='tmux attach -t %s\\='\")
- a function of one argument (the session name)."
  :type '(choice (const :tag "Auto" nil) string function)
  :group 'enkan-repl-terminal)

(defun enkan-repl--tmux-attach--default-command (session)
  "Return a shell command string to attach SESSION using a sensible default."
  (pcase system-type
    ('darwin
     (format "osascript -e 'tell application \"Terminal\" to do script \"tmux attach -t %s\"'"
             session))
    (_ (format "%s -ic 'tmux attach -t %s'" shell-file-name session))))

(defun enkan-repl--terminal-tmux-id-instance (id)
  "Tmux backend: derive instance index from ID's window name suffix.
Returns 1 when no \"-N\" suffix is present."
  (let ((win (enkan-repl--terminal-tmux--id-window id)))
    (cond
     ((null win) 1)
     ((string-match "-\\([0-9]+\\)$" win)
      (string-to-number (match-string 1 win)))
     (t 1))))

(defun enkan-repl--terminal-tmux-id-instance-for-path (id path)
  "Return tmux ID's instance index relative to PATH.
A numeric suffix is treated as an instance only when the tmux window name is
the PATH basename followed by -N.  This keeps projects whose real names end in
digits, such as foo-2, at instance 1."
  (let* ((win (enkan-repl--terminal-tmux--id-window id))
         (base (and (stringp path)
                    (enkan-repl--terminal-tmux--derive-base-name path))))
    (cond
     ((and win base (string= win base)) 1)
     ((and win base
           (string-match
            (format "\\`%s-\\([0-9]+\\)\\'" (regexp-quote base))
            win))
      (string-to-number (match-string 1 win)))
     (t 1))))

;;;###autoload
(defun enkan-repl-tmux-kill-session (&optional session)
  "Kill a tmux SESSION (defaults to the current workspace's enkan session).
When called interactively without arg, prompts to pick from all
sessions whose name starts with `enkan-repl-tmux-session-prefix'."
  (interactive
   (list
    (let* ((prefix enkan-repl-tmux-session-prefix)
           (live (enkan-repl-state--list-live-tmux-sessions prefix)))
      (cond
       ((null live)
        (user-error "No tmux sessions matching prefix %s" prefix))
       ((= 1 (length live)) (car live))
       (t (completing-read "Kill tmux session: " live nil t))))))
  (unless (enkan-repl--terminal-tmux--has-session session)
    (user-error "Tmux session not found: %s" session))
  (when (enkan-repl--terminal-tmux--call (list "kill-session" "-t" session))
    (message "Killed tmux session: %s" session)
    t))

;;;###autoload
(defun enkan-repl-tmux-kill-all-enkan ()
  "Kill ALL tmux sessions starting with `enkan-repl-tmux-session-prefix'.
Asks for confirmation."
  (interactive)
  (let ((live (enkan-repl-state--list-live-tmux-sessions
               enkan-repl-tmux-session-prefix)))
    (cond
     ((null live)
      (message "No enkan tmux sessions to kill")
      nil)
     ((y-or-n-p (format "Kill %d enkan tmux session(s): %s ? "
                        (length live) (mapconcat #'identity live ", ")))
      (dolist (s live)
        (enkan-repl--terminal-tmux--call (list "kill-session" "-t" s)))
      (message "Killed: %s" (mapconcat #'identity live ", "))
      t))))

;;;###autoload
(defun enkan-repl-tmux-attach (&optional ws-id)
  "Attach to enkan tmux session for WS-ID in an external terminal.

WS-ID defaults to `enkan-repl--current-workspace'.  Uses
`enkan-repl-tmux-attach-command' to construct the attach invocation;
falls back to a system-appropriate default when nil."
  (interactive)
  (let* ((ws (or ws-id
                 (and (boundp 'enkan-repl--current-workspace)
                      enkan-repl--current-workspace)
                 (user-error "No current workspace")))
         (session (concat enkan-repl-tmux-session-prefix ws)))
    (unless (enkan-repl--terminal-tmux--has-session session)
      (user-error "Tmux session %s does not exist" session))
    (cond
     ((functionp enkan-repl-tmux-attach-command)
      (funcall enkan-repl-tmux-attach-command session))
     ((stringp enkan-repl-tmux-attach-command)
      (start-process-shell-command
       "enkan-tmux-attach" nil
       (format-spec enkan-repl-tmux-attach-command (list (cons ?s session)))))
     (t
      (start-process-shell-command
       "enkan-tmux-attach" nil
       (enkan-repl--tmux-attach--default-command session))))
    (message "Launched terminal to attach tmux session: %s" session)))

(provide 'enkan-repl-terminal)
;;; enkan-repl-terminal.el ends here

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
  (enkan-repl--terminal--dispatch
   'alive-p
   #'enkan-repl--terminal-eat-alive-p
   #'enkan-repl--terminal-tmux-alive-p
   id))

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
  (and id
       (buffer-live-p id)
       (let ((proc (get-buffer-process id)))
         (and proc (process-live-p proc)))))

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
              (let ((proc (get-buffer-process buf)))
                (and proc (process-live-p proc)))))
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
  (with-temp-buffer
    (let ((status (apply #'call-process
                         enkan-repl-tmux-executable
                         nil (current-buffer) nil args)))
      (cond
       ((not (zerop status)) nil)
       (capture (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                  (if (string-suffix-p "\n" s)
                      (substring s 0 -1)
                    s)))
       (t t)))))

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

(defcustom enkan-repl-tmux-mirror-interval 5.0
  "Idle interval (seconds) between `tmux capture-pane' refreshes.
Applies to every active tmux mirror buffer."
  :type 'number
  :group 'enkan-repl-terminal)

(defcustom enkan-repl-tmux-mirror-history-lines 500
  "Number of scrollback lines to capture per refresh tick."
  :type 'integer
  :group 'enkan-repl-terminal)

(defvar-local enkan-repl--tmux-mirror-id nil
  "Buffer-local: tmux target id this mirror buffer is bound to.")

(defvar-local enkan-repl--tmux-mirror-timer nil
  "Buffer-local: idle timer driving capture-pane refreshes.")

(defvar-local enkan-repl--tmux-mirror-refresh-process nil
  "Buffer-local asynchronous `tmux capture-pane' process.")

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
  (enkan-repl--terminal-tmux--mirror-update-status)
  ;; A visible "refreshing" marker only helps if redisplay happens before the
  ;; synchronous tmux process call starts.
  (when (eq state 'refreshing)
    (redisplay t)))

(defun enkan-repl--terminal-tmux-kill-workspace (workspace-id)
  "Kill the tmux session backing WORKSPACE-ID.
Returns non-nil when a live tmux session was killed."
  (let ((session (and workspace-id
                      (concat enkan-repl-tmux-session-prefix workspace-id))))
    (when (and session (enkan-repl--terminal-tmux--has-session session))
      (enkan-repl--terminal-tmux--call (list "kill-session" "-t" session)))))

(defun enkan-repl--terminal-tmux--line-column-at (position)
  "Return a cons cell of line and column at POSITION in the current buffer."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos nil t) (current-column))))

(defun enkan-repl--terminal-tmux--position-at-line-column (line-column)
  "Return buffer position for LINE-COLUMN in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car line-column)))
    (move-to-column (cdr line-column))
    (point)))

(defun enkan-repl--terminal-tmux--window-at-bottom-p (window)
  "Return non-nil when WINDOW is displaying the current buffer bottom."
  (or (= (window-point window) (point-max))
      (let ((start-line (line-number-at-pos (window-start window) t))
            (end-line (line-number-at-pos (point-max) t))
            (height (max 1 (window-body-height window))))
        (<= end-line (+ start-line height)))))

(defun enkan-repl--terminal-tmux--mirror-window-state (buffer)
  "Capture scroll state for windows currently displaying BUFFER."
  (with-current-buffer buffer
    (mapcar
     (lambda (window)
       (list :window window
             :at-bottom (enkan-repl--terminal-tmux--window-at-bottom-p window)
             :start (enkan-repl--terminal-tmux--line-column-at
                     (window-start window))
             :point (enkan-repl--terminal-tmux--line-column-at
                     (window-point window))))
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
                (let* ((height (max 1 (window-body-height window)))
                       (end-line (line-number-at-pos (point-max) t))
                       (start-line (max 1 (- end-line height -1)))
                       (start (enkan-repl--terminal-tmux--position-at-line-column
                               (cons start-line 0))))
                  (set-window-point window (point-max))
                  (set-window-start window start))
              (let ((start (enkan-repl--terminal-tmux--position-at-line-column
                            (plist-get state :start)))
                    (point (enkan-repl--terminal-tmux--position-at-line-column
                            (plist-get state :point))))
                (set-window-point window point)
                (set-window-start window start)))))))))

(defun enkan-repl--terminal-tmux--replace-mirror-content (content)
  "Replace the current tmux mirror buffer with CONTENT while preserving markers."
  (let ((source (generate-new-buffer " *enkan-repl tmux capture*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert content))
          (replace-buffer-contents source))
      (kill-buffer source))))

(defun enkan-repl--terminal-tmux--pane-cwd (id)
  "Return current working directory of tmux pane ID, or nil."
  (enkan-repl--terminal-tmux--call
   (list "display-message" "-p" "-t" id "#{pane_current_path}")
   t))

(defun enkan-repl--terminal-tmux--mirror-buffer-name (id)
  "Return mirror buffer name for tmux ID.
Uses the same `*ws:NN enkan:/path/*' form as the eat backend so that
existing buffer-list-based layout / lookup code finds tmux mirror
buffers transparently.  The path is normalized via
`file-name-as-directory' so the trailing slash matches what
`enkan-repl--path->buffer-name' produces from `enkan-repl-target-directories'
entries (which include the trailing slash).  Falls back to
`*tmux <id>*' when the path cannot be determined."
  (let ((path (enkan-repl--terminal-tmux--pane-cwd id))
        (instance (enkan-repl--terminal-tmux-id-instance id)))
    (cond
     ((and path (not (string-empty-p path)))
      (enkan-repl--path->buffer-name (file-name-as-directory path) instance))
     (t (format "*tmux %s*" id)))))

(defun enkan-repl--terminal-tmux--capture-pane (id lines)
  "Return current pane content of tmux ID as a string (best effort).
LINES is the maximum scrollback to include (negative numbers per
`tmux capture-pane -S' semantics)."
  (or (enkan-repl--terminal-tmux--call
       (list "capture-pane" "-p" "-J" "-S" (format "-%d" (max 0 lines))
             "-t" id)
       t)
      ""))

(defun enkan-repl--terminal-tmux--capture-pane-async (id lines callback)
  "Capture tmux ID asynchronously and call CALLBACK with content and status.
LINES is the maximum scrollback to include.  CALLBACK receives two
arguments: CONTENT, or nil on failure, and the tmux process exit status."
  (unless (executable-find enkan-repl-tmux-executable)
    (user-error "Tmux executable not found: %s" enkan-repl-tmux-executable))
  (let* ((capture-buffer (generate-new-buffer " *enkan-repl tmux async capture*"))
         (command (list enkan-repl-tmux-executable
                        "capture-pane" "-p" "-J" "-S"
                        (format "-%d" (max 0 lines))
                        "-t" id)))
    (make-process
     :name (format "enkan-tmux-capture %s" id)
     :buffer capture-buffer
     :command command
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((status (process-exit-status process))
               (content (when (buffer-live-p capture-buffer)
                          (with-current-buffer capture-buffer
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))
           (when (buffer-live-p capture-buffer)
             (kill-buffer capture-buffer))
           (funcall callback
                    (and (zerop status) content)
                    status)))))))

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
  (ignore status)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (equal enkan-repl--tmux-mirror-id id)
        (setq enkan-repl--tmux-mirror-refresh-process nil)
        (if (null content)
            (enkan-repl--terminal-tmux--mirror-mark-closed)
          (let* ((content-hash (secure-hash 'sha1 content))
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
              (enkan-repl--terminal-tmux--mirror-set-state 'fresh))))))))

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

(defun enkan-repl--terminal-tmux--mirror-make (id &optional defer-refresh)
  "Get or create the mirror buffer for tmux ID, set up timer, return it.
When DEFER-REFRESH is non-nil, do not immediately refresh the mirror content."
  (let ((buf (get-buffer-create
              (enkan-repl--terminal-tmux--mirror-buffer-name id))))
    (with-current-buffer buf
      (unless (derived-mode-p 'enkan-repl-tmux-mirror-mode)
        (enkan-repl-tmux-mirror-mode))
      (setq enkan-repl--tmux-mirror-id id)
      (enkan-repl--terminal-tmux--mirror-update-status)
      (unless enkan-repl--tmux-mirror-timer
        (setq enkan-repl--tmux-mirror-timer
              (run-with-idle-timer
               enkan-repl-tmux-mirror-interval t
               #'enkan-repl--terminal-tmux--mirror-refresh buf)))
      ;; First refresh immediately for instant feedback when visible.  Hidden
      ;; mirrors stay cheap until manually refreshed or displayed.
      (unless defer-refresh
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
      (enkan-repl--terminal-tmux--mirror-refresh buf t)
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

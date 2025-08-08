;;; enkan-repl-backend.el --- Backend abstraction layer for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the backend abstraction layer for enkan-repl.
;; It allows switching between different backends (eat, claudemacs, tmux)
;; without changing the core functionality.

;;; Code:

(require 'cl-lib)

;;; Configuration

(defgroup enkan-repl-backend nil
  "Backend settings for enkan-repl."
  :group 'enkan-repl)

(defcustom enkan-repl-backend-type 'eat
  "Backend type for enkan-repl.
Available options: 'eat, 'claudemacs, 'tmux"
  :type '(choice (const :tag "Eat (default)" eat)
                 (const :tag "Claudemacs" claudemacs)
                 (const :tag "Tmux" tmux))
  :group 'enkan-repl-backend)

(defcustom enkan-repl-notify-on-completion t
  "Whether to show a system notification when a task completes.
When non-nil, display an OS notification popup when output is received.
When nil, no notification is shown (silent operation)."
  :type 'boolean
  :group 'enkan-repl-backend)

(defcustom enkan-repl-notification-sound-mac "Submarine"
  "The sound to use when displaying system notifications on macOS.
See `/System/Library/Sounds' for available sounds.
System sounds include: `Basso', `Blow', `Bottle', `Frog', `Funk',
`Glass', `Hero', `Morse', `Ping', `Pop', `Purr', `Sosumi', `Submarine',
`Tink'. Or put more sounds in the `/Library/Sound' folder and use those."
  :type 'string
  :group 'enkan-repl-backend)

(defcustom enkan-repl-notification-sound-linux "bell"
  "The sound to use when displaying system notifications on Linux.
Uses canberra-gtk-play if available.  Common sound IDs include:
`message-new-instant', `bell', `dialog-error', `dialog-warning'.
When empty string, no sound is played."
  :type 'string
  :group 'enkan-repl-backend)

;;; Session Management

(defvar enkan-repl-sessions-alist nil
  "Active sessions list.
Each element is (session-id . plist) where plist contains:
  :buffer     - The buffer object
  :backend    - Backend type (eat, claudemacs, tmux)
  :directory  - Working directory
  :short-name - Short display name
  :created    - Creation timestamp")

(defun enkan-repl-backend--generate-session-id (backend directory)
  "Generate a unique session ID from BACKEND and DIRECTORY."
  (format "%s:%s" backend (expand-file-name directory)))

(defun enkan-repl-backend--extract-short-name (directory)
  "Extract short name from DIRECTORY path."
  (file-name-nondirectory (directory-file-name directory)))

(defun enkan-repl-backend--find-session-by-short-name (short-name)
  "Find session by SHORT-NAME if it's unique."
  (let ((matches (cl-remove-if-not
                  (lambda (session)
                    (string= (plist-get (cdr session) :short-name) short-name))
                  enkan-repl-sessions-alist)))
    (when (= (length matches) 1)
      (car matches))))

(defun enkan-repl-backend--parse-target (text)
  "Parse @target notation from TEXT.
Returns (session-id . message) or (nil . text)."
  (if (string-match "^@\\([^ ]+\\) \\(.*\\)" text)
      (let ((target (match-string 1 text))
            (message (match-string 2 text)))
        ;; Try to find by full ID or short name
        (if-let ((session (or (assoc target enkan-repl-sessions-alist)
                             (enkan-repl-backend--find-session-by-short-name target))))
            (cons (car session) message)
          (cons nil text)))
    (cons nil text)))

;;; Backend Interface

(defun enkan-repl-backend-create-session (directory &optional new-session-p)
  "Create a new session for DIRECTORY.
If NEW-SESSION-P is non-nil, create a new session even if one exists."
  (let* ((normalized-dir (expand-file-name directory))
         (backend enkan-repl-backend-type)
         (session-id (enkan-repl-backend--generate-session-id backend normalized-dir))
         (short-name (enkan-repl-backend--extract-short-name normalized-dir)))

    ;; Check for existing session
    (when (and (not new-session-p)
               (assoc session-id enkan-repl-sessions-alist))
      (user-error "Session already exists for %s. Use prefix arg to create new session" normalized-dir))

    ;; Create new session using backend-specific function
    (let ((buffer (funcall (intern (format "enkan-repl-backend-%s-create" backend))
                          normalized-dir session-id)))
      ;; Register session
      (push (cons session-id
                  (list :buffer buffer
                        :backend backend
                        :directory normalized-dir
                        :short-name short-name
                        :created (current-time)))
            enkan-repl-sessions-alist)
      buffer)))

(defun enkan-repl-backend-send-text (text &optional session-id)
  "Send TEXT to SESSION-ID or current session."
  (let* ((parsed (enkan-repl-backend--parse-target text))
         (target-id (or (car parsed) session-id))
         (message (if (car parsed) (cdr parsed) text)))
    (if target-id
        (let* ((session (assoc target-id enkan-repl-sessions-alist))
               (backend (plist-get (cdr session) :backend))
               (buffer (plist-get (cdr session) :buffer)))
          (funcall (intern (format "enkan-repl-backend-%s-send" backend))
                   message buffer))
      (user-error "No target session specified"))))

(defun enkan-repl-backend-get-buffer (session-id)
  "Get buffer for SESSION-ID."
  (plist-get (cdr (assoc session-id enkan-repl-sessions-alist)) :buffer))

(defun enkan-repl-backend-alive-p (session-id)
  "Check if SESSION-ID is alive."
  (when-let* ((session (assoc session-id enkan-repl-sessions-alist))
              (backend (plist-get (cdr session) :backend))
              (buffer (plist-get (cdr session) :buffer)))
    (funcall (intern (format "enkan-repl-backend-%s-alive-p" backend)) buffer)))

(defun enkan-repl-backend-finish-session (session-id)
  "Finish SESSION-ID and clean up resources."
  (when-let* ((session (assoc session-id enkan-repl-sessions-alist))
              (backend (plist-get (cdr session) :backend))
              (buffer (plist-get (cdr session) :buffer)))
    ;; Call backend-specific finish function
    (funcall (intern (format "enkan-repl-backend-%s-finish" backend)) buffer)
    ;; Remove from session list
    (setq enkan-repl-sessions-alist
          (assoc-delete-all session-id enkan-repl-sessions-alist))
    ;; Kill buffer if it still exists
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    t))

(defun enkan-repl-backend-list-sessions ()
  "List all active sessions with display format."
  (mapcar (lambda (session)
            (let ((id (car session))
                  (props (cdr session)))
              (format "%s (%s)"
                      (plist-get props :short-name)
                      id)))
          enkan-repl-sessions-alist))

(defun enkan-repl-backend--find-session-for-buffer ()
  "Find the appropriate session for the current buffer.
Returns session-id if found, nil otherwise."
  (let ((current-dir (expand-file-name default-directory)))
    ;; Find a session whose directory matches current buffer's directory
    (cl-loop for (session-id . props) in enkan-repl-sessions-alist
             for session-dir = (plist-get props :directory)
             when (string-prefix-p session-dir current-dir)
             return session-id)))

(defun enkan-repl-backend-recenter-bottom (&optional session-id)
  "Recenter SESSION-ID buffer to bottom."
  (when-let ((buffer (if session-id
                         (enkan-repl-backend-get-buffer session-id)
                       (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (recenter -1))))

;;; Notification System

(defun enkan-repl-backend--system-notification (message &optional title)
  "Show a system notification with MESSAGE and optional TITLE.
Uses platform-specific notification commands based on the system."
  (let ((title (or title "enkan-repl")))
    (cond
     ;; macOS with osascript
     ((and (eq system-type 'darwin)
           (executable-find "osascript"))
      (call-process "osascript" nil nil nil
                    "-e" (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                                message title enkan-repl-notification-sound-mac)))
     ;; Linux with notify-send and canberra-gtk-play
     ((and (memq system-type '(gnu gnu/linux gnu/kfreebsd))
           (executable-find "notify-send"))
      (call-process "notify-send" nil nil nil title message)
      (when (and (not (string-empty-p enkan-repl-notification-sound-linux))
                 (executable-find "canberra-gtk-play"))
        (call-process "canberra-gtk-play" nil nil nil
                      "--id" enkan-repl-notification-sound-linux)))
     ;; Windows or fallback - just use message
     (t
      (message "%s: %s" title message)))))

(defun enkan-repl-backend--bell-handler (terminal)
  "Handle bell events from TERMINAL.
This function is called when a bell character is received."
  (ignore terminal)
  (when enkan-repl-notify-on-completion
    (enkan-repl-backend--system-notification 
     "Task completed and is awaiting your input")))

(defun enkan-repl-backend-setup-bell-handler (buffer)
  "Set up bell handler for BUFFER.
This enables notification on task completion."
  (with-current-buffer buffer
    (when (and (boundp 'eat-terminal)
               eat-terminal)
      (setf (eat-term-parameter eat-terminal 'ring-bell-function)
            #'enkan-repl-backend--bell-handler))))

(provide 'enkan-repl-backend)
;;; enkan-repl-backend.el ends here

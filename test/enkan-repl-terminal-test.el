;;; enkan-repl-terminal-test.el --- Tests for terminal backend abstraction -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure parts of `enkan-repl-terminal' (backend dispatch,
;; tmux identifier helpers).  Tests that would require an actual tmux
;; server are kept out — those are exercised manually.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-terminal.el" default-directory) nil t)

(defvar enkan-repl--current-workspace)

;;;; Default backend

(ert-deftest test-enkan-repl-terminal-default-backend ()
  "Default backend is `tmux' (heavy TUI safe).  `eat' remains as a
documented opt-in alternative; see README."
  (should (eq enkan-repl-terminal-backend 'tmux)))

;;;; tmux identifier helpers (pure)

(ert-deftest test-enkan-repl--terminal-tmux--make-id ()
  (should (string= "enkan-01:lat"
                   (enkan-repl--terminal-tmux--make-id "enkan-01" "lat")))
  (should (string= "enkan-99:my-proj-2"
                   (enkan-repl--terminal-tmux--make-id "enkan-99" "my-proj-2"))))

(ert-deftest test-enkan-repl--terminal-tmux--id-session ()
  (should (string= "enkan-01"
                   (enkan-repl--terminal-tmux--id-session "enkan-01:lat")))
  (should (string= "enkan-42"
                   (enkan-repl--terminal-tmux--id-session "enkan-42:proj-3")))
  (should (null (enkan-repl--terminal-tmux--id-session "no-colon")))
  (should (null (enkan-repl--terminal-tmux--id-session nil))))

(ert-deftest test-enkan-repl--terminal-tmux--id-window ()
  (should (string= "lat"
                   (enkan-repl--terminal-tmux--id-window "enkan-01:lat")))
  (should (string= "lat-2"
                   (enkan-repl--terminal-tmux--id-window "enkan-01:lat-2")))
  ;; Window names containing colons (pane refs etc.) are returned as-is
  ;; from the first colon onward.
  (should (string= "lat.0"
                   (enkan-repl--terminal-tmux--id-window "enkan-01:lat.0")))
  (should (null (enkan-repl--terminal-tmux--id-window "no-colon")))
  (should (null (enkan-repl--terminal-tmux--id-window nil))))

(ert-deftest test-enkan-repl--terminal-tmux--id-workspace ()
  "Workspace id is parsed from tmux target session names."
  (let ((enkan-repl-tmux-session-prefix "enkan-"))
    (should (string= "02"
                     (enkan-repl--terminal-tmux--id-workspace
                      "enkan-02:lat")))
    (should (null
             (enkan-repl--terminal-tmux--id-workspace
              "other-02:lat")))))

(ert-deftest test-enkan-repl--terminal-tmux-id-instance ()
  "Instance index parsed from -N suffix in window name."
  (should (= 1 (enkan-repl--terminal-tmux-id-instance "enkan-01:lat")))
  (should (= 2 (enkan-repl--terminal-tmux-id-instance "enkan-01:lat-2")))
  (should (= 7 (enkan-repl--terminal-tmux-id-instance "enkan-01:lat-7")))
  (should (= 1 (enkan-repl--terminal-tmux-id-instance "enkan-01:my-proj")))  ; "-proj" not a number
  (should (= 1 (enkan-repl--terminal-tmux-id-instance nil))))

(ert-deftest test-enkan-repl--terminal-tmux--derive-base-name ()
  (should (string= "lat"
                   (enkan-repl--terminal-tmux--derive-base-name "/path/to/lat")))
  (should (string= "lat"
                   (enkan-repl--terminal-tmux--derive-base-name "/path/to/lat/")))
  (should (string= ".emacs.d"
                   (enkan-repl--terminal-tmux--derive-base-name "~/.emacs.d/")))
  ;; Root collapses to a sentinel
  (should (string= "root"
                   (enkan-repl--terminal-tmux--derive-base-name "/"))))

;;;; bounded tmux control calls

(ert-deftest test-enkan-repl--terminal-tmux--call-captures-output ()
  "Synchronous tmux control wrapper should capture output."
  (let ((enkan-repl-tmux-executable "sh")
        (enkan-repl-tmux-command-timeout 1.0))
    (should (string= "ok"
                     (enkan-repl--terminal-tmux--call
                      '("-c" "printf ok") t)))))

(ert-deftest test-enkan-repl--terminal-tmux--call-times-out ()
  "Synchronous tmux control wrapper should be bounded by timeout."
  (let ((enkan-repl-tmux-executable "sh")
        (enkan-repl-tmux-command-timeout 0.05)
        (started (float-time)))
    (should-not (enkan-repl--terminal-tmux--call
                 '("-c" "sleep 1") nil))
    (should (< (- (float-time) started) 0.5))))

;;;; tmux session name from current workspace

(ert-deftest test-enkan-repl--terminal-tmux--workspace-session ()
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-tmux-session-prefix "enkan-"))
    (should (string= "enkan-01"
                     (enkan-repl--terminal-tmux--workspace-session))))
  (let ((enkan-repl--current-workspace "42")
        (enkan-repl-tmux-session-prefix "foo-"))
    (should (string= "foo-42"
                     (enkan-repl--terminal-tmux--workspace-session))))
  (let ((enkan-repl--current-workspace nil))
    (should (null (enkan-repl--terminal-tmux--workspace-session)))))

;;;; tmux bell notification monitor

(ert-deftest test-enkan-repl--terminal-tmux--bell-alert-targets ()
  "Bell alert parsing should return only windows with the tmux bell flag."
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--call)
             (lambda (_args _capture)
               "lat\t1\nother\t0\nwork\t1\n")))
    (should (equal '("enkan-01:lat" "enkan-01:work")
                   (enkan-repl--terminal-tmux--bell-alert-targets
                    "enkan-01")))))

(ert-deftest test-enkan-repl--terminal-tmux--bell-monitor-notifies-once ()
  "Bell monitor should notify only newly observed bell alert flags."
  (let ((enkan-repl-tmux-bell-notify t)
        (enkan-repl-tmux-bell-notify-interval 2.0)
        (enkan-repl--tmux-bell-monitor-seen (make-hash-table :test #'equal))
        notified)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--bell-monitor-sessions)
               (lambda () '("enkan-01")))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-alert-targets)
               (lambda (_session) '("enkan-01:lat")))
              ((symbol-function 'enkan-repl--terminal-tmux--all-targets)
               (lambda (_session) nil))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-notify)
               (lambda (target &optional _kind) (push target notified))))
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (should (equal '("enkan-01:lat") notified)))))

(ert-deftest test-enkan-repl--terminal-tmux--bell-monitor-resets-cleared-alerts ()
  "Bell monitor should allow a later alert after tmux clears the flag."
  (let ((enkan-repl-tmux-bell-notify t)
        (enkan-repl-tmux-bell-notify-interval 2.0)
        (enkan-repl--tmux-bell-monitor-seen (make-hash-table :test #'equal))
        (targets '(("enkan-01:lat") nil ("enkan-01:lat")))
        notified)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--bell-monitor-sessions)
               (lambda () '("enkan-01")))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-alert-targets)
               (lambda (_session) (pop targets)))
              ((symbol-function 'enkan-repl--terminal-tmux--all-targets)
               (lambda (_session) nil))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-notify)
               (lambda (target &optional _kind) (push target notified))))
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (should (equal '("enkan-01:lat" "enkan-01:lat") notified)))))

(ert-deftest test-enkan-repl--terminal-tmux--prompt-content-p ()
  "Permission prompt detection covers common Claude/Codex wording."
  (should (enkan-repl--terminal-tmux--prompt-content-p
           "Codex needs approval to run command. Allow command?"))
  (should (enkan-repl--terminal-tmux--prompt-content-p
           "Claude Code permission required\nApprove tool use?"))
  (should-not (enkan-repl--terminal-tmux--prompt-content-p
               "Build finished successfully.")))

(ert-deftest test-enkan-repl--terminal-tmux--prompt-alert-targets ()
  "Prompt alerts should be detected from bounded pane captures."
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--all-targets)
             (lambda (_session) '("enkan-01:lat" "enkan-01:codex")))
            ((symbol-function 'enkan-repl--terminal-tmux--alert-capture)
             (lambda (target)
               (if (string-suffix-p "codex" target)
                   "Allow command?\ny/n"
                 "idle output"))))
    (should (equal '("enkan-01:codex")
                   (enkan-repl--terminal-tmux--prompt-alert-targets
                    "enkan-01")))))

(ert-deftest test-enkan-repl--terminal-tmux--alert-notify-refreshes-target ()
  "Alert notification should force-refresh the alerting target's mirror."
  (let ((enkan-repl-tmux-mirror t)
        (buf (generate-new-buffer "*tmux alert refresh*"))
        refreshed
        message)
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--mirror-make)
                   (lambda (target &optional defer-refresh)
                     (should (string= target "enkan-01:codex"))
                     (should defer-refresh)
                     buf))
                  ((symbol-function 'enkan-repl--terminal-tmux--mirror-refresh)
                   (lambda (buffer &optional force)
                     (setq refreshed (list buffer force))))
                  ((symbol-function 'enkan-repl-notify-task-complete)
                   (lambda (msg) (setq message msg))))
          (enkan-repl--terminal-tmux--bell-notify "enkan-01:codex" 'prompt)
          (should (equal (list buf t) refreshed))
          (should (string-match-p "permission requested" message)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux--activity-alert-p ()
  "Activity notification should fire after changed content settles."
  (let ((enkan-repl-tmux-activity-notify t)
        (enkan-repl-tmux-activity-notify-idle-seconds 1.0)
        (enkan-repl--tmux-bell-monitor-content-state
         (make-hash-table :test #'equal))
        (t0 (seconds-to-time 100)))
    (should-not
     (enkan-repl--terminal-tmux--activity-alert-p
      "enkan-01:codex" "old" t0))
    (should-not
     (enkan-repl--terminal-tmux--activity-alert-p
      "enkan-01:codex" "new answer" (seconds-to-time 101)))
    (should-not
     (enkan-repl--terminal-tmux--activity-alert-p
      "enkan-01:codex" "new answer" (seconds-to-time 101.5)))
    (should
     (enkan-repl--terminal-tmux--activity-alert-p
      "enkan-01:codex" "new answer" (seconds-to-time 102.1)))
    (should-not
     (enkan-repl--terminal-tmux--activity-alert-p
      "enkan-01:codex" "new answer" (seconds-to-time 103)))))

(ert-deftest test-enkan-repl--terminal-tmux--bell-monitor-notifies-on-settled-output ()
  "Bell monitor should notify when bounded pane content changes and settles."
  (let ((enkan-repl-tmux-bell-notify t)
        (enkan-repl-tmux-bell-notify-interval 2.0)
        (enkan-repl-tmux-activity-notify t)
        (enkan-repl-tmux-activity-notify-idle-seconds 0)
        (enkan-repl-tmux-prompt-notify nil)
        (enkan-repl--tmux-bell-monitor-seen (make-hash-table :test #'equal))
        (enkan-repl--tmux-bell-monitor-content-state
         (make-hash-table :test #'equal))
        (captures '("old" "final answer" "final answer"))
        notified)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--bell-monitor-sessions)
               (lambda () '("enkan-01")))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-alert-targets)
               (lambda (_session) nil))
              ((symbol-function 'enkan-repl--terminal-tmux--all-targets)
               (lambda (_session) '("enkan-01:codex")))
              ((symbol-function 'enkan-repl--terminal-tmux--alert-capture)
               (lambda (_target) (pop captures)))
              ((symbol-function 'enkan-repl--terminal-tmux--bell-notify)
               (lambda (target &optional kind)
                 (push (list target kind) notified))))
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (enkan-repl--terminal-tmux--bell-monitor-poll)
      (should (equal '(("enkan-01:codex" activity)) notified)))))

;;;; next-instance-name with list-windows mocked

(ert-deftest test-enkan-repl--terminal-tmux--next-instance-name ()
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--list-windows)
             (lambda (_session) '())))
    (should (string= "lat"
                     (enkan-repl--terminal-tmux--next-instance-name
                      "enkan-01" "lat"))))
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--list-windows)
             (lambda (_session) '("lat"))))
    (should (string= "lat-2"
                     (enkan-repl--terminal-tmux--next-instance-name
                      "enkan-01" "lat"))))
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--list-windows)
             (lambda (_session) '("lat" "lat-2" "lat-3"))))
    (should (string= "lat-4"
                     (enkan-repl--terminal-tmux--next-instance-name
                      "enkan-01" "lat")))))

;;;; eat backend id-instance (existing parser via abstraction)

(ert-deftest test-enkan-repl--terminal-eat-id-instance ()
  (let ((buf (generate-new-buffer "*ws:01 enkan:/p/*")))
    (unwind-protect
        (should (= 1 (enkan-repl--terminal-eat-id-instance buf)))
      (kill-buffer buf)))
  (let ((buf (generate-new-buffer "*ws:01 enkan:/p/*<3>")))
    (unwind-protect
        (should (= 3 (enkan-repl--terminal-eat-id-instance buf)))
      (kill-buffer buf))))

;;;; tmux mirror buffer naming

(ert-deftest test-enkan-repl--terminal-tmux--mirror-buffer-name ()
  ;; PATH is supplied by callers that already know the startup directory, or
  ;; by the asynchronous cwd lookup after the fallback buffer is created.
  (let ((enkan-repl--current-workspace "01"))
    (should (string= "*ws:01 enkan:/path/to/lat/*"
                     (enkan-repl--terminal-tmux--mirror-buffer-name
                      "enkan-01:lat" "/path/to/lat")))
    (should (string= "*ws:01 enkan:/path/to/lat/*"
                     (enkan-repl--terminal-tmux--mirror-buffer-name
                      "enkan-01:lat" "/path/to/lat/")))
    ;; instance suffix is honored (lat-3 -> instance 3 -> *<3>)
    (should (string= "*ws:01 enkan:/path/to/lat/*<3>"
                     (enkan-repl--terminal-tmux--mirror-buffer-name
                      "enkan-01:lat-3" "/path/to/lat"))))
  ;; Without PATH, this function never calls tmux and immediately falls back.
  (should (string= "*tmux enkan-01:lat*"
                   (enkan-repl--terminal-tmux--mirror-buffer-name
                    "enkan-01:lat"))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-buffer-name-uses-target-workspace ()
  "Background tmux mirrors must use the workspace encoded in target ids."
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-tmux-session-prefix "enkan-"))
    (should (string= "*ws:02 enkan:/path/to/lat/*"
                     (enkan-repl--terminal-tmux--mirror-buffer-name
                      "enkan-02:lat" "/path/to/lat")))))

(ert-deftest test-enkan-repl-tmux-repair-mirror-buffer-names ()
  "Existing tmux mirrors with wrong workspace prefixes should be repairable."
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-tmux-session-prefix "enkan-")
        (buf (generate-new-buffer "*ws:01 enkan:/path/to/lat/*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-02:lat"))
          (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
            (should (= 1 (enkan-repl-tmux-repair-mirror-buffer-names))))
          (should (string= "*ws:02 enkan:/path/to/lat/*"
                           (buffer-name buf))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-make-does-not-block-on-cwd ()
  "Mirror creation should not synchronously query pane cwd."
  (let ((enkan-repl--current-workspace "01")
        (buf nil)
        cwd-lookup-started)
    (unwind-protect
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd-async)
                   (lambda (_id _callback)
                     (setq cwd-lookup-started t)
                     nil)))
          (setq buf (enkan-repl--terminal-tmux--mirror-make
                     "enkan-01:lat" t))
          (should (buffer-live-p buf))
          (should (string= "*tmux enkan-01:lat*" (buffer-name buf)))
          (should cwd-lookup-started))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-make-renames-from-async-cwd ()
  "Async cwd lookup renames fallback mirrors to path-based names."
  (let ((enkan-repl--current-workspace "01")
        (buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd-async)
                   (lambda (_id callback)
                     (funcall callback "/path/to/lat")
                     nil)))
          (setq buf (enkan-repl--terminal-tmux--mirror-make
                     "enkan-01:lat" t))
          (should (string= "*ws:01 enkan:/path/to/lat/*"
                           (buffer-name buf))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-make-default-is-manual ()
  "Mirror creation should not start timers or capture by default."
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-tmux-mirror-auto-refresh nil)
        (buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd-async)
                   (lambda (&rest _) nil))
                  ((symbol-function 'enkan-repl--terminal-tmux--mirror-refresh)
                   (lambda (&rest _)
                     (error "must not auto-refresh"))))
          (setq buf (enkan-repl--terminal-tmux--mirror-make
                     "enkan-01:lat" nil))
          (with-current-buffer buf
            (should-not enkan-repl--tmux-mirror-timer)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-make-auto-refresh-opt-in ()
  "Mirror auto-refresh is available only when explicitly enabled."
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-tmux-mirror-auto-refresh t)
        (enkan-repl-tmux-mirror-interval 10)
        (buf nil)
        refresh-count)
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd-async)
                   (lambda (&rest _) nil))
                  ((symbol-function 'enkan-repl--terminal-tmux--mirror-refresh)
                   (lambda (&rest _)
                     (setq refresh-count (1+ (or refresh-count 0))))))
          (setq buf (enkan-repl--terminal-tmux--mirror-make
                     "enkan-01:lat" nil))
          (with-current-buffer buf
            (should enkan-repl--tmux-mirror-timer))
          (should (= refresh-count 1)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl--terminal-tmux-display-does-not-refresh ()
  "Displaying a mirror should not capture pane content implicitly."
  (let ((enkan-repl-tmux-mirror t)
        (buf (generate-new-buffer "*tmux display manual*")))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--mirror-make)
                   (lambda (_id &rest _)
                     buf))
                  ((symbol-function 'enkan-repl--terminal-tmux--mirror-refresh)
                   (lambda (&rest _)
                     (error "must not refresh on display")))
                  ((symbol-function 'pop-to-buffer-same-window)
                   (lambda (buffer)
                     (should (eq buffer buf))))
                  ((symbol-function 'message)
                   (lambda (&rest _) nil)))
          (should (eq buf (enkan-repl--terminal-tmux-display "enkan-01:lat"))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest test-enkan-repl-tmux-refresh-workspace ()
  "Refresh command creates or updates mirrors for current tmux windows."
  (let ((enkan-repl-terminal-backend 'tmux)
        (enkan-repl-tmux-mirror t)
        (enkan-repl--current-workspace "01")
        refreshed)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-list)
               (lambda () '("enkan-01:a" "enkan-01:b")))
              ((symbol-function 'enkan-repl--terminal-tmux--mirror-make)
               (lambda (id &optional defer-refresh)
                 (should defer-refresh)
                 id))
              ((symbol-function 'enkan-repl--terminal-tmux--mirror-refresh)
               (lambda (buffer &optional force)
                 (push (list buffer force) refreshed))))
      (should (= 2 (enkan-repl-tmux-refresh-workspace t)))
      (should (equal '(("enkan-01:b" t) ("enkan-01:a" t))
                     refreshed)))))

;;;; tmux mirror buffer refresh scrolling

(defun test-enkan-repl-terminal--lines (from to)
  "Return a string containing numbered lines FROM to TO."
  (mapconcat (lambda (line) (format "line %02d" line))
             (number-sequence from to)
             "\n"))

(defun test-enkan-repl-terminal--goto-line (line)
  "Move point to LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(ert-deftest test-enkan-repl--terminal-tmux--tail-append-caps-content ()
  "Tmux capture accumulation should keep only the bounded tail."
  (should (string= "bcdef"
                   (enkan-repl--terminal-tmux--tail-append
                    "abc" "def" 5)))
  (should (string= "cdefg"
                   (enkan-repl--terminal-tmux--tail-append
                    "ab" "cdefg" 5)))
  (should (string= "abcdef"
                   (enkan-repl--terminal-tmux--tail-append
                    "abc" "def" nil))))

(ert-deftest test-enkan-repl--terminal-tmux--prepare-mirror-content-tail-lines ()
  "Prepared tmux mirror content should keep only recent display lines."
  (let ((enkan-repl-tmux-mirror-display-lines 3)
        (enkan-repl-tmux-mirror-max-chars nil)
        (enkan-repl-tmux-mirror-compact-noisy-blocks nil))
    (should (string= "line 03\nline 04\nline 05"
                     (enkan-repl--terminal-tmux--prepare-mirror-content
                      (test-enkan-repl-terminal--lines 1 5))))))

(ert-deftest test-enkan-repl--terminal-tmux--prepare-mirror-content-compacts-diff ()
  "Prepared tmux mirror content should collapse large diff-like blocks."
  (let ((enkan-repl-tmux-mirror-display-lines 20)
        (enkan-repl-tmux-mirror-max-chars nil)
        (enkan-repl-tmux-mirror-compact-noisy-blocks t)
        (enkan-repl-tmux-mirror-noisy-block-threshold 3))
    (should (string= "message before\n[enkan-repl: omitted 4 noisy/diff line(s)]\nmessage after"
                     (enkan-repl--terminal-tmux--prepare-mirror-content
                      (string-join
                       '("message before"
                         "+added line"
                         "-removed line"
                         "@@ hunk"
                         "diff --git a/file b/file"
                         "message after")
                       "\n"))))))

(ert-deftest test-enkan-repl--terminal-tmux--prepare-mirror-content-keeps-short-noisy-order ()
  "Short noisy blocks should stay readable and preserve line order."
  (let ((enkan-repl-tmux-mirror-display-lines 20)
        (enkan-repl-tmux-mirror-max-chars nil)
        (enkan-repl-tmux-mirror-compact-noisy-blocks t)
        (enkan-repl-tmux-mirror-noisy-block-threshold 3))
    (should (string= "message before\n+added\n-removed\nmessage after"
                     (enkan-repl--terminal-tmux--prepare-mirror-content
                      (string-join
                       '("message before" "+added" "-removed" "message after")
                       "\n"))))))

(ert-deftest test-enkan-repl--terminal-tmux--prepare-mirror-content-compacts-long-lines ()
  "Repeated very long lines should be treated as noisy output."
  (let ((enkan-repl-tmux-mirror-display-lines 20)
        (enkan-repl-tmux-mirror-max-chars nil)
        (enkan-repl-tmux-mirror-compact-noisy-blocks t)
        (enkan-repl-tmux-mirror-noisy-block-threshold 2)
        (enkan-repl-tmux-mirror-max-line-length 8))
    (should (string= "before\n[enkan-repl: omitted 2 noisy/diff line(s)]\nafter"
                     (enkan-repl--terminal-tmux--prepare-mirror-content
                      (string-join
                       (list "before"
                             "0123456789abcdef"
                             "abcdefghijklmnop"
                             "after")
                       "\n"))))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-sticks-to-bottom ()
  "A tmux mirror window at the bottom should remain at the bottom after refresh."
  (let ((buf (generate-new-buffer "*tmux mirror bottom*"))
        (initial-content (concat (test-enkan-repl-terminal--lines 1 80) "\n"))
        (updated-content (concat (test-enkan-repl-terminal--lines 1 81) "\n")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((window-min-height 1))
            (split-window (selected-window) 12 'below))
          (switch-to-buffer buf)
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
            (let ((inhibit-read-only t))
              (insert initial-content)))
          (goto-char (point-max))
          (recenter -1)
          (redisplay t)
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (id _lines callback)
                       (funcall callback updated-content 0)
                       nil)))
            (enkan-repl--terminal-tmux--mirror-refresh buf))
          (redisplay t)
          (should (= (window-point) (point-max)))
          (should (enkan-repl--terminal-tmux--window-at-bottom-p
                   (selected-window))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-preserves-scroll ()
  "A tmux mirror window away from the bottom should keep its scroll position."
  (let ((buf (generate-new-buffer "*tmux mirror scrolled*"))
        (initial-content (concat (test-enkan-repl-terminal--lines 1 200) "\n"))
        (updated-content (concat (test-enkan-repl-terminal--lines 1 201) "\n"))
        (enkan-repl-tmux-mirror-display-lines 300))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((window-min-height 1))
            (split-window (selected-window) 12 'below))
          (switch-to-buffer buf)
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
            (let ((inhibit-read-only t))
              (insert initial-content)))
          (test-enkan-repl-terminal--goto-line 30)
          (set-window-start nil (point))
          (test-enkan-repl-terminal--goto-line 35)
          (set-window-point nil (point))
          (redisplay t)
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (id _lines callback)
                       (funcall callback updated-content 0)
                       nil)))
            (enkan-repl--terminal-tmux--mirror-refresh buf))
          (should (= 30 (line-number-at-pos (window-start) t)))
          (should (= 35 (line-number-at-pos (window-point) t))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-skips-hidden ()
  "A hidden tmux mirror should not start tmux capture."
  (let ((buf (generate-new-buffer "*tmux mirror hidden*"))
        (capture-count 0))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p"))
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (_id _lines _callback)
                       (setq capture-count (1+ capture-count))
                       "hidden content")))
            (enkan-repl--terminal-tmux--mirror-refresh buf)
            (should (= 0 capture-count))
            (with-current-buffer buf
            (should (eq enkan-repl--tmux-mirror-state 'hidden)))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-skips-minibuffer ()
  "Timer refresh should not run while minibuffer completion is active."
  (let ((buf (generate-new-buffer "*tmux mirror minibuffer*"))
        (capture-called nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p"))
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     (lambda () 'mock-minibuffer-window))
                    ((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (&rest _args)
                       (setq capture-called t)
                       "")))
            (should (eq 'deferred
                        (enkan-repl--terminal-tmux--mirror-refresh buf)))
            (should-not capture-called)))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-shows-refreshing ()
  "A tmux mirror should enter `refreshing' before capture completes."
  (let ((buf (generate-new-buffer "*tmux mirror status*"))
        observed-state)
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (_id _lines callback)
                       (setq observed-state enkan-repl--tmux-mirror-state)
                       (funcall callback "status content" 0)
                       nil)))
            (enkan-repl--terminal-tmux--mirror-refresh buf t)
            (should (eq observed-state 'refreshing))
            (should (eq enkan-repl--tmux-mirror-state 'fresh))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-refresh-starts-async ()
  "Refresh should return after starting capture without waiting for content."
  (let ((buf (generate-new-buffer "*tmux mirror async*"))
        callback-started)
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (cl-letf (((symbol-function
                      'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (_id _lines callback)
                       (setq callback-started callback)
                       nil)))
            (should (eq 'refreshing
                        (enkan-repl--terminal-tmux--mirror-refresh buf t)))
            (should callback-started)
            (should (eq enkan-repl--tmux-mirror-state 'refreshing))
            (should (= (point-min) (point-max)))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux-refresh-current-forces-hidden ()
  "Manual refresh should update the current mirror even when it is hidden."
  (let ((buf (generate-new-buffer "*tmux mirror manual*"))
        (capture-count 0))
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--capture-pane-async)
                     (lambda (_id _lines callback)
                       (setq capture-count (1+ capture-count))
                       (funcall callback "manual content" 0)
                       nil))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (enkan-repl-tmux-refresh-current)
            (should (= 1 capture-count))
            (should (string= "manual content"
                             (buffer-substring-no-properties
                              (point-min) (point-max))))
            (should (eq enkan-repl--tmux-mirror-state 'fresh))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-apply-avoids-buffer-diff ()
  "Mirror application should not diff against a stale large buffer."
  (let ((buf (generate-new-buffer "*tmux mirror replace*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (insert (make-string 10000 ?x))
          (let ((enkan-repl-tmux-mirror-display-lines 80)
                (enkan-repl-tmux-mirror-max-chars (* 64 1024))
                (enkan-repl-tmux-mirror-compact-noisy-blocks t))
            (cl-letf (((symbol-function 'replace-buffer-contents)
                       (lambda (&rest _)
                         (error "must not diff mirror content"))))
              (enkan-repl--terminal-tmux--mirror-apply-content
               buf "enkan-01:p" (current-time) "fresh content" 0)))
          (should (string= "fresh content"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-tmux--mirror-timeout-keeps-buffer-open ()
  "A timed-out tmux capture should not mark the pane as closed."
  (let ((buf (generate-new-buffer "*tmux mirror timeout*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (setq-local enkan-repl--tmux-mirror-refresh-process 'mock-process)
          (insert "previous content")
          (enkan-repl--terminal-tmux--mirror-apply-content
           buf "enkan-01:p" (current-time) nil 'timeout)
          (should (eq enkan-repl--tmux-mirror-state 'timed-out))
          (should-not enkan-repl--tmux-mirror-refresh-process)
          (should (string= "previous content"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl-tmux-stop-all-mirrors ()
  "Stop command cancels Emacs-side mirror timers without tmux calls."
  (let ((buf (generate-new-buffer "*tmux mirror stop*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (setq-local enkan-repl--tmux-mirror-timer
                      (run-with-timer 1000 nil #'ignore))
          (cl-letf (((symbol-function 'message) (lambda (&rest _) nil))
                    ((symbol-function 'enkan-repl--terminal-tmux--call)
                     (lambda (&rest _)
                       (error "must not call tmux"))))
            (should (= 1 (enkan-repl-tmux-stop-all-mirrors)))
            (should-not enkan-repl--tmux-mirror-timer)
            (should (eq enkan-repl--tmux-mirror-state 'stopped))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;;; attach helper default command (per-system shape)

(ert-deftest test-enkan-repl--tmux-attach--default-command ()
  ;; macOS path: should embed an osascript invocation containing the session.
  (let ((system-type 'darwin))
    (let ((cmd (enkan-repl--tmux-attach--default-command "enkan-01")))
      (should (stringp cmd))
      (should (string-match-p "Terminal" cmd))
      (should (string-match-p "tmux attach -t enkan-01" cmd))))
  ;; Non-darwin fallback: shell command containing tmux attach.
  (let ((system-type 'gnu/linux)
        (shell-file-name "/bin/sh"))
    (let ((cmd (enkan-repl--tmux-attach--default-command "enkan-02")))
      (should (string-match-p "tmux attach -t enkan-02" cmd)))))

;;;; ID coercion (buffer <-> tmux target)

(ert-deftest test-enkan-repl--terminal--coerce-id-tmux-buffer ()
  "Under tmux backend, a buffer with bound `enkan-repl--tmux-mirror-id'
must be coerced to that string id."
  (let ((enkan-repl-terminal-backend 'tmux)
        (buf (generate-new-buffer "*ws:01 enkan:/p/*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p"))
          (should (string= "enkan-01:p"
                           (enkan-repl--terminal--coerce-id buf))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal--coerce-id-tmux-buffer-no-binding ()
  "Under tmux backend, a buffer without binding is returned as-is."
  (let ((enkan-repl-terminal-backend 'tmux)
        (buf (generate-new-buffer "*plain*")))
    (unwind-protect
        (should (eq buf (enkan-repl--terminal--coerce-id buf)))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal--coerce-id-tmux-string ()
  "Under tmux backend, a string id passes through unchanged."
  (let ((enkan-repl-terminal-backend 'tmux))
    (should (string= "enkan-01:lat"
                     (enkan-repl--terminal--coerce-id "enkan-01:lat")))))

(ert-deftest test-enkan-repl--terminal-alive-p-tmux-buffer-is-local ()
  "Tmux mirror buffer liveness must not synchronously query tmux."
  (let ((enkan-repl-terminal-backend 'tmux)
        (buf (generate-new-buffer "*ws:01 enkan:/p/*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:p"))
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-alive-p)
                     (lambda (_id)
                       (error "must not synchronously query tmux"))))
            (should (enkan-repl--terminal-alive-p buf))))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal-alive-p-tmux-closed-buffer ()
  "Closed tmux mirrors are not considered alive by the local check."
  (let ((enkan-repl-terminal-backend 'tmux)
        (buf (generate-new-buffer "*ws:01 enkan:/p/*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local enkan-repl--tmux-mirror-id "enkan-01:p")
          (setq-local enkan-repl--tmux-mirror-state 'closed)
          (should-not (enkan-repl--terminal-alive-p buf)))
      (kill-buffer buf))))

(ert-deftest test-enkan-repl--terminal--coerce-id-eat ()
  "Under eat backend, no coercion is applied."
  (let ((enkan-repl-terminal-backend 'eat)
        (buf (generate-new-buffer "*plain*")))
    (unwind-protect
        (should (eq buf (enkan-repl--terminal--coerce-id buf)))
      (kill-buffer buf)))
  (let ((enkan-repl-terminal-backend 'eat))
    (should (string= "anything"
                     (enkan-repl--terminal--coerce-id "anything")))))

(provide 'enkan-repl-terminal-test)
;;; enkan-repl-terminal-test.el ends here

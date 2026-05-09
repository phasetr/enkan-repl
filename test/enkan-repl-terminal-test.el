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
  ;; When pane-cwd succeeds: mirror buffer name uses the eat backend
  ;; format `*ws:NN enkan:/path/*' (with trailing slash) so existing
  ;; buffer-list-based layout / send-target lookup finds tmux mirror
  ;; buffers transparently.
  (let ((enkan-repl--current-workspace "01"))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd)
               (lambda (_id) "/path/to/lat")))
      (should (string= "*ws:01 enkan:/path/to/lat/*"
                       (enkan-repl--terminal-tmux--mirror-buffer-name
                        "enkan-01:lat"))))
    ;; pane-cwd already with trailing slash: idempotent
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd)
               (lambda (_id) "/path/to/lat/")))
      (should (string= "*ws:01 enkan:/path/to/lat/*"
                       (enkan-repl--terminal-tmux--mirror-buffer-name
                        "enkan-01:lat"))))
    ;; instance suffix is honored (lat-3 -> instance 3 -> *<3>)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd)
               (lambda (_id) "/path/to/lat")))
      (should (string= "*ws:01 enkan:/path/to/lat/*<3>"
                       (enkan-repl--terminal-tmux--mirror-buffer-name
                        "enkan-01:lat-3")))))
  ;; When pane-cwd cannot be determined, falls back to *tmux <id>* form.
  (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--pane-cwd)
             (lambda (_id) nil)))
    (should (string= "*tmux enkan-01:lat*"
                     (enkan-repl--terminal-tmux--mirror-buffer-name
                      "enkan-01:lat")))))

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
        (updated-content (concat (test-enkan-repl-terminal--lines 1 201) "\n")))
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

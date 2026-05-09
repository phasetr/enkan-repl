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

;;;; Default backend

(ert-deftest test-enkan-repl-terminal-default-backend ()
  "Default backend must be `eat' so existing setups behave unchanged."
  (should (eq enkan-repl-terminal-backend 'eat)))

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

(provide 'enkan-repl-terminal-test)
;;; enkan-repl-terminal-test.el ends here

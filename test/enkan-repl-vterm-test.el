;;; enkan-repl-vterm-test.el --- Direct tmux views -*- lexical-binding: t; -*-

;;; Commentary:
;; Deterministic tests; no native module or user tmux server is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'enkan-repl)

(defvar enkan-repl-tmux-display-method)
(defvar enkan-repl--tmux-vterm-workspace)
(defvar vterm-shell)
(defvar vterm-environment)

(ert-deftest enkan-vterm-auto-platforms-and-fallback ()
  "Auto uses vterm on Mac/Linux, and retains mirrors elsewhere."
  (let ((enkan-repl-tmux-display-method 'auto)
        (enkan-repl-terminal-backend 'tmux))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
               (lambda () t)))
      (dolist (platform '(darwin gnu/linux))
        (let ((system-type platform))
          (should (enkan-repl--terminal-tmux--use-vterm-p))))
      (let ((system-type 'windows-nt))
        (should-not (enkan-repl--terminal-tmux--use-vterm-p)))
      (let ((enkan-repl-tmux-display-method 'mirror))
        (should-not (enkan-repl--terminal-tmux--use-vterm-p))))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
               (lambda () nil)))
      (should-not (enkan-repl--terminal-tmux--use-vterm-p))
      (let ((enkan-repl-tmux-display-method 'vterm))
        (should-error (enkan-repl--terminal-tmux--use-vterm-p)
                      :type 'user-error)))))

(ert-deftest enkan-vterm-probe-never-loads-vterm-without-native-module ()
  "Avoid vterm's automatic native-module build path."
  (let ((module-file-suffix ".so") calls)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (push feature calls)
                 nil)))
      (should-not (enkan-repl--terminal-tmux--vterm-available-p))
      (should (equal calls '(vterm-module))))))

(ert-deftest enkan-vterm-probe-handles-broken-module ()
  "An installed but unloadable module is treated as unavailable."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) (error "Wrong architecture"))))
    (should-not (enkan-repl--terminal-tmux--vterm-available-p))))

(ert-deftest enkan-vterm-attach-reuses-workspace-and-isolates-environment ()
  "Attach directly, quote arguments, and keep one viewer per workspace."
  (let ((enkan-repl-terminal-backend 'tmux)
        (enkan-repl-tmux-executable "/tmp/tmux with spaces")
        (enkan-repl-tmux-session-prefix "test;enkan-")
        (process-environment (cons "TMUX=outer" (cons "TMUX_PANE=%8" process-environment)))
        (vterm-environment '("TMUX=injected" "TMUX_PANE=%9" "KEEP=yes"))
        buffers shells environments (starts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
                   (lambda () t))
                  ((symbol-function 'enkan-repl--terminal-tmux--has-session)
                   (lambda (_) t))
                  ((symbol-function 'vterm-mode)
                   (lambda ()
                     (cl-incf starts)
                     (push vterm-shell shells)
                     (push (list (getenv "TMUX") (getenv "TMUX_PANE")
                                 vterm-environment) environments)
                     (setq major-mode 'vterm-mode)
                     (use-local-map (make-sparse-keymap))))
                  ((symbol-function 'enkan-repl--terminal-tmux--vterm-live-p)
                   (lambda (_) t)))
          (let* ((first (enkan-repl--terminal-tmux--vterm-buffer "01"))
                 (second (enkan-repl--terminal-tmux--vterm-buffer "02")))
            (setq buffers (list first second))
            (should (eq first (enkan-repl--terminal-tmux--vterm-buffer "01")))
            (should (= starts 2))
            (should-not (eq first second))
            (should (equal (buffer-local-value 'enkan-repl--tmux-vterm-workspace first) "01"))
            (should (equal (enkan-repl--buffer-workspace-id first) "01"))
            (should (memq first (enkan-repl--workspace-terminal-buffers "01")))
            (should (eq (with-current-buffer first (key-binding (kbd "C-M-w")))
                        'enkan-repl-workspace-switch))
            (should (equal (car shells)
                           (mapconcat #'shell-quote-argument
                                      '("/tmp/tmux with spaces" "attach-session" "-t" "=test;enkan-02") " ")))
            (dolist (environment environments)
              (should (equal (seq-take environment 2) '(nil nil)))
              (should (equal (seq-take (nth 2 environment) 2) '("TMUX" "TMUX_PANE"))))))
      (mapc #'kill-buffer buffers))))

(ert-deftest enkan-vterm-dead-view-is-recreated ()
  "Closing an attached client must allow attaching again."
  (let ((enkan-repl-tmux-session-prefix "enkan-vterm-test-") first second)
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p) (lambda () t))
                  ((symbol-function 'enkan-repl--terminal-tmux--has-session) (lambda (_) t))
                  ((symbol-function 'vterm-mode)
                   (lambda () (use-local-map (make-sparse-keymap))))
                  ((symbol-function 'enkan-repl--terminal-tmux--vterm-live-p) (lambda (_) nil)))
          (setq first (enkan-repl--terminal-tmux--vterm-buffer "91"))
          (setq second (enkan-repl--terminal-tmux--vterm-buffer "91"))
          (should-not (buffer-live-p first))
          (should (buffer-live-p second)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(ert-deftest enkan-vterm-workspace-switch-displays-target-without-layout-extension ()
  "C-M-w must display the new session even without examples/window-layouts."
  (let ((enkan-repl-terminal-backend 'tmux)
        (enkan-repl--workspaces '(("01" :current-project "one") ("02" :current-project "two")))
        (enkan-repl--current-workspace "01") shown)
    (cl-letf (((symbol-function 'hmenu) (lambda (&rest _) "02 [two]"))
              ((symbol-function 'enkan-repl--save-workspace-state) #'ignore)
              ((symbol-function 'enkan-repl--load-workspace-state) #'ignore)
              ((symbol-function 'enkan-repl--terminal-tmux--use-vterm-p) (lambda () t))
              ((symbol-function 'enkan-repl--terminal-tmux-display-workspace)
               (lambda () (setq shown enkan-repl--current-workspace))))
      (enkan-repl-workspace-switch)
      (should (equal shown "02")))))

(ert-deftest enkan-vterm-display-selects-stable-pane-without-capture ()
  "A project display selects its tmux window and shows the workspace vterm."
  (let (calls shown)
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--use-vterm-p) (lambda () t))
              ((symbol-function 'enkan-repl--terminal-tmux--call)
               (lambda (args &rest _) (push args calls) t))
              ((symbol-function 'enkan-repl-tmux-vterm)
               (lambda (workspace) (setq shown workspace)))
              ((symbol-function 'enkan-repl--terminal-tmux--mirror-make)
               (lambda (&rest _) (ert-fail "Must not display a mirror"))))
      (enkan-repl--terminal-tmux-display "enkan-02:dr-remote.jp|%12")
      (should (equal shown "02"))
      (should (equal calls '(("select-window" "-t" "%12")))))))

(ert-deftest enkan-vterm-capture-cannot-overwrite-view-or-metadata ()
  "Pending captures and automatic refresh cannot write in vterm mode."
  (with-temp-buffer
    (setq-local enkan-repl--tmux-mirror-id "enkan-01:project")
    (insert "untouched")
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--use-vterm-p) (lambda () t)))
      (enkan-repl--terminal-tmux--mirror-refresh (current-buffer) t)
      (enkan-repl--terminal-tmux--mirror-apply-content
       (current-buffer) enkan-repl--tmux-mirror-id (current-time) "wrong" 0)
      (should (equal (buffer-string) "untouched"))
      (let ((enkan-repl-tmux-mirror-auto-refresh t))
        (should-not (enkan-repl--terminal-tmux--mirror-auto-refresh-enabled-p))))))

(ert-deftest enkan-vterm-renumber-updates-viewer-metadata ()
  "Renumber must preserve the viewer's identity for subsequent switches."
  (let ((buffer (generate-new-buffer "*enkan-tmux:91*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local enkan-repl--tmux-vterm-workspace "91"))
          (enkan-repl--apply-workspace-buffer-rename-plan
           (enkan-repl--workspace-buffer-rename-plan "91" "92"))
          (should (equal (buffer-local-value 'enkan-repl--tmux-vterm-workspace buffer) "92"))
          (should (equal (buffer-name buffer) "*enkan-tmux:92*")))
      (kill-buffer buffer))))

(ert-deftest enkan-vterm-switch-replaces-view-and-preserves-input-window ()
  "Changing workspaces reuses the terminal window without selecting it."
  (let ((old (generate-new-buffer " *enkan-old*"))
        (new (generate-new-buffer " *enkan-new*"))
        (enkan-repl--current-workspace "02"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((input-window (selected-window))
                (viewer-window (split-window-right)))
            (with-current-buffer old
              (setq-local enkan-repl--tmux-vterm-workspace "01"))
            (with-current-buffer new
              (setq-local enkan-repl--tmux-vterm-workspace "02"))
            (set-window-buffer viewer-window old)
            (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--has-session) (lambda (_) t))
                      ((symbol-function 'enkan-repl--terminal-tmux--vterm-buffer) (lambda (_) new)))
              (enkan-repl--terminal-tmux-display-workspace)
              (should (eq (window-buffer viewer-window) new))
              (should (eq (selected-window) input-window)))))
      (kill-buffer old)
      (kill-buffer new))))

(ert-deftest enkan-vterm-empty-workspace-hides-previous-view ()
  "An empty workspace must not keep displaying a different session."
  (let ((old (generate-new-buffer " *enkan-empty-test*"))
        (enkan-repl--current-workspace "02"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((viewer-window (split-window-right)))
            (with-current-buffer old
              (setq-local enkan-repl--tmux-vterm-workspace "01"))
            (set-window-buffer viewer-window old)
            (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--has-session) (lambda (_) nil))
                      ((symbol-function 'enkan-repl--terminal-tmux--vterm-buffer)
                       (lambda (_) (ert-fail "Do not attach a missing session"))))
              (enkan-repl--terminal-tmux-display-workspace)
              (should-not (eq (window-buffer viewer-window) old)))))
      (kill-buffer old))))

(ert-deftest enkan-vterm-start-displays-created-terminal ()
  "Starting a tmux session uses direct display without needing layout code."
  (let ((enkan-repl-terminal-backend 'tmux)
        (enkan-repl--current-project nil)
        shown)
    (cl-letf (((symbol-function 'enkan-repl--terminal-start) (lambda (_) "enkan-01:proj|%1"))
              ((symbol-function 'enkan-repl--terminal-tmux-id-instance-for-path) (lambda (&rest _) 1))
              ((symbol-function 'enkan-repl--terminal-tmux--mirror-make) #'ignore)
              ((symbol-function 'enkan-repl--terminal-tmux--use-vterm-p) (lambda () t))
              ((symbol-function 'enkan-repl--terminal-display) (lambda (id) (setq shown id))))
      (enkan-repl-start-session)
      (should (equal shown "enkan-01:proj|%1")))))

(ert-deftest enkan-vterm-view-is-not-an-extra-project-send-target ()
  "Workspace views must not inflate counts or receive project sends."
  (let ((viewer (generate-new-buffer " *enkan-view-target-test*"))
        (project (generate-new-buffer " *enkan-project-target-test*"))
        (enkan-repl--current-workspace "01"))
    (unwind-protect
        (progn
          (with-current-buffer viewer
            (setq-local enkan-repl--tmux-vterm-workspace "01"))
          (with-current-buffer project
            (setq-local enkan-repl--tmux-mirror-id "enkan-01:project"))
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (buffer) (and (eq buffer viewer) 'viewer-process)))
                    ((symbol-function 'process-live-p) (lambda (_) t)))
            (should (equal (enkan-repl--get-available-buffers (list viewer project))
                           (list project)))
            (should (= 1 (enkan-repl--get-workspace-buffer-count-pure
                          (list viewer project) "01")))))
      (kill-buffer viewer)
      (kill-buffer project))))

(provide 'enkan-repl-vterm-test)
;;; enkan-repl-vterm-test.el ends here

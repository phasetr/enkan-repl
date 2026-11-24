;;; enkan-repl-vterm-integration-test.el --- Optional vterm smoke test -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt in with ENKAN_REPL_VTERM_INTEGRATION=1 and an already installed vterm
;; on load-path.  Every tmux command uses a private temporary socket, never
;; the user's server.  No dependencies are installed or compiled.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'enkan-repl)

(defun enkan-test-vterm--wait (predicate)
  "Wait up to three seconds for PREDICATE, processing terminal output."
  (let ((deadline (+ (float-time) 3)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest enkan-vterm-integration-private-server-lifecycle ()
  "Render real output and exercise switching, renumber, detach, and deletion."
  (skip-unless (getenv "ENKAN_REPL_VTERM_INTEGRATION"))
  (skip-unless (executable-find "tmux"))
  (skip-unless (enkan-repl--terminal-tmux--vterm-available-p))
  (let* ((root (make-temp-file "enkan-vterm-test-" t))
         (socket (expand-file-name "tmux.sock" root))
         (wrapper (expand-file-name "tmux" root))
         (tmux (executable-find "tmux"))
         (enkan-repl-tmux-executable wrapper)
         (enkan-repl-terminal-backend 'tmux)
         (enkan-repl-tmux-display-method 'vterm)
         (enkan-repl-tmux-session-prefix "enkan-vterm-test-")
         (enkan-repl--current-workspace "91")
         first second)
    (unwind-protect
        (save-window-excursion
          (with-temp-file wrapper
            (insert "#!/bin/sh\nexec " (shell-quote-argument tmux)
                    " -S " (shell-quote-argument socket)
                    " -f /dev/null \"$@\"\n"))
          (set-file-modes wrapper #o700)
          (dolist (session '("enkan-vterm-test-91" "enkan-vterm-test-92"))
            (should (enkan-repl--terminal-tmux--call
                     (list "new-session" "-d" "-s" session "-c" root
                           "-n" "project.one" "/bin/sh"))))
          (setq first (enkan-repl--terminal-tmux--vterm-buffer "91"))
          (setq second (enkan-repl--terminal-tmux--vterm-buffer "92"))
          (should (enkan-test-vterm--wait
                   (lambda ()
                     (= 2 (length (split-string
                                   (or (enkan-repl--terminal-tmux--call
                                        '("list-clients" "-F" "#{session_name}") t)
                                       "") "\n" t))))))
          (let ((pane (enkan-repl--terminal-tmux--call
                       '("new-window" "-d" "-P" "-F" "#{pane_id}"
                         "-t" "enkan-vterm-test-91" "-n" "second.project"
                         "/bin/sh") t)))
            (should pane)
            (enkan-repl--terminal-tmux-display
             (enkan-repl--terminal-tmux--make-id
              "enkan-vterm-test-91" "second.project" pane))
            (should (equal "second.project"
                           (enkan-repl--terminal-tmux--call
                            '("display-message" "-p" "-t" "enkan-vterm-test-91"
                              "#{window_name}") t)))
            (should (enkan-repl--terminal-tmux-send
                     (enkan-repl--terminal-tmux--make-id
                      "enkan-vterm-test-91" "second.project" pane)
                     "printf 'ENKAN_%s_OK\\n' REAL" t)))
          (should (enkan-test-vterm--wait
                   (lambda ()
                     (with-current-buffer first
                       (string-match-p "ENKAN_REAL_OK"
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))))))
          (setq enkan-repl--current-workspace "92")
          (enkan-repl--terminal-tmux-display-workspace)
          (should (get-buffer-window second))
          (should-not (get-buffer-window first))
          (let ((plan (enkan-repl--workspace-buffer-rename-plan "91" "93")))
            (enkan-repl--terminal-tmux-rename-workspace "91" "93")
            (enkan-repl--apply-workspace-buffer-rename-plan plan))
          (should (eq first (enkan-repl--terminal-tmux--vterm-buffer "93")))
          (kill-buffer second)
          (should (enkan-repl--terminal-tmux--has-session "enkan-vterm-test-92"))
          (setq second (enkan-repl--terminal-tmux--vterm-buffer "92"))
          (should (enkan-repl--terminal-tmux--vterm-live-p second))
          (should (plist-get (enkan-repl--stop-workspace-terminals "93") :tmux-killed))
          (should-not (buffer-live-p first))
          (should-not (enkan-repl--terminal-tmux--has-session "enkan-vterm-test-93"))
          (should (enkan-repl--terminal-tmux--has-session "enkan-vterm-test-92")))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (when (file-executable-p wrapper)
        (enkan-repl--terminal-tmux--call '("kill-server")))
      (delete-directory root t))))

(provide 'enkan-repl-vterm-integration-test)
;;; enkan-repl-vterm-integration-test.el ends here

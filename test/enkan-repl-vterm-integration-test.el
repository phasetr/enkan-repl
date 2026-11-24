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

(ert-deftest enkan-vterm-integration-existing-project-workflow ()
  "Keep example keybindings, project columns, sends and workspace layouts."
  (skip-unless (getenv "ENKAN_REPL_VTERM_INTEGRATION"))
  (skip-unless (executable-find "tmux"))
  (skip-unless (enkan-repl--terminal-tmux--vterm-available-p))
  (let* ((root (make-temp-file "enkan-vterm-test-" t))
         (socket (expand-file-name "tmux.sock" root))
         (wrapper (expand-file-name "tmux" root))
         (config (expand-file-name "tmux.conf" root))
         (tmux (executable-find "tmux"))
         (enkan-repl-tmux-executable wrapper)
         (enkan-repl-terminal-backend 'tmux)
         (enkan-repl-tmux-display-method 'vterm)
         (enkan-repl-tmux-bell-notify nil)
         (enkan-repl-tmux-session-prefix "enkan-vterm-test-")
         (enkan-repl--current-workspace "91")
         (enkan-repl--current-project "suite")
         (enkan-repl-session-list '((1 . "one") (2 . "two")))
         (enkan-repl-project-aliases '("one" "two"))
         (enkan-repl-projects '(("suite" . ("one" "two"))))
         (enkan-repl-target-directories
          `(("one" . ("one" . ,(concat root "/one/")))
            ("two" . ("two" . ,(concat root "/two/")))))
         (enkan-repl--workspaces nil)
         (enkan-repl-state-file (expand-file-name "state.eld" root))
         (enkan-repl-global-minor-bindings nil)
         (enkan-repl-global-minor-mode nil)
         (enkan-repl-global-minor-mode-map (make-sparse-keymap))
         (minor-mode-map-alist (copy-tree minor-mode-map-alist))
         (enkan-repl-center-file nil)
         (load-path (cons (expand-file-name "examples") load-path))
         one two other ids)
    (unwind-protect
        (save-window-excursion
          (with-temp-file config (insert "set-option -g default-command /bin/sh\n"))
          (with-temp-file wrapper
            (insert "#!/bin/sh\nexec " (shell-quote-argument tmux)
                    " -S " (shell-quote-argument socket)
                    " -f " (shell-quote-argument config) " \"$@\"\n"))
          (set-file-modes wrapper #o700)
          (make-directory (concat root "/one"))
          (make-directory (concat root "/two"))
          (load (expand-file-name "keybinding.el" (car load-path)) nil t)
          (setq enkan-repl-center-file (expand-file-name "center.org" root))
          (with-temp-file enkan-repl-center-file (insert "Center\n"))
          (dolist (entry '(("91" "one") ("91" "two") ("92" "one")))
            (let ((enkan-repl--current-workspace (car entry))
                  (enkan-repl-tmux-disable-alternate-screen nil)
                  (enkan-repl-tmux-history-limit nil))
              (push (enkan-repl--terminal-tmux-start (concat root "/" (cadr entry) "/")) ids)))
          (setq ids (nreverse ids))
          (setq one (enkan-repl--terminal-tmux--mirror-make (nth 0 ids) t (concat root "/one/")))
          (setq two (enkan-repl--terminal-tmux--mirror-make (nth 1 ids) t (concat root "/two/")))
          (setq other (enkan-repl--terminal-tmux--mirror-make (nth 2 ids) t (concat root "/one/")))
          (should (enkan-test-vterm--wait
                   (lambda ()
                     (= 3 (length (split-string
                                   (or (enkan-repl--terminal-tmux--call
                                        '("list-clients" "-F" "#{session_name}") t) "") "\n" t))))))
          (call-interactively (key-binding (kbd "C-M-l")))
          (should (= 3 (length (window-list))))
          (should (get-buffer-window one))
          (should (get-buffer-window two))
          (should (equal (buffer-file-name) enkan-repl-center-file))
          ;; Same-session project views must retain distinct selected windows.
          (dolist (buffer (list one two))
            (with-current-buffer buffer
              (should (equal "1" (enkan-repl--terminal-tmux--call
                                  (list "display-message" "-p" "-t"
                                        (concat "=" enkan-repl--tmux-vterm-session ":")
                                        "#{session_windows}") t)))
              (should (equal
                       (enkan-repl--terminal-tmux--id-window enkan-repl--tmux-mirror-id)
                       (enkan-repl--terminal-tmux--call
                        (list "display-message" "-p" "-t"
                              (concat "=" enkan-repl--tmux-vterm-session ":")
                              "#{window_name}") t)))))
          ;; C-M-i sends aliases to the same project targets as before.
          (dolist (entry `(("one" . ,one) ("two" . ,two)))
            (erase-buffer)
            (insert (format ":%s printf 'PROJECT_%%s_OK\\n' %s" (car entry) (car entry)))
            (call-interactively (key-binding (kbd "C-M-i")))
            (should (enkan-test-vterm--wait
                     (lambda ()
                       (with-current-buffer (cdr entry)
                         (string-match-p (format "PROJECT_%s_OK" (car entry))
                                         (buffer-substring-no-properties (point-min) (point-max))))))))
          (call-interactively (key-binding (kbd "C-M-b")))
          (call-interactively (key-binding (kbd "C-M-g")))
          ;; Existing workspace switch saves/restores the same project layout.
          (enkan-repl--save-workspace-state)
          (push (cons "92" (list :current-project "suite" :session-list '((1 . "one"))
                                 :session-counter 1 :project-aliases '("one")
                                 :target-directories enkan-repl-target-directories))
                enkan-repl--workspaces)
          (cl-letf (((symbol-function 'hmenu) (lambda (&rest _) "92 [suite]")))
            (call-interactively (key-binding (kbd "C-M-w"))))
          (should (equal enkan-repl--current-workspace "92"))
          (should (= 2 (length (window-list))))
          (should (get-buffer-window other))
          (should-not (get-buffer-window one))
          (should-not (get-buffer-window two))
          ;; Closing one view preserves its original project and permits reopen.
          (let ((view (buffer-local-value 'enkan-repl--tmux-vterm-session one)))
            (kill-buffer one)
            (should-not (enkan-repl--terminal-tmux--has-session view)))
          (should (enkan-repl--terminal-tmux-alive-p (nth 0 ids)))
          (setq one (enkan-repl--terminal-tmux--mirror-make (nth 0 ids) t (concat root "/one/")))
          (should (enkan-repl--terminal-tmux--vterm-live-p one))
          ;; Renumber preserves per-project buffer identity and private clients.
          (let ((plan (enkan-repl--workspace-buffer-rename-plan "91" "93")))
            (enkan-repl--terminal-tmux-rename-workspace "91" "93")
            (enkan-repl--apply-workspace-buffer-rename-plan plan))
          (should (string-prefix-p "*ws:93 enkan:" (buffer-name one)))
          (should (equal "enkan-vterm-test-93"
                         (enkan-repl--terminal-tmux--id-session (enkan-repl--buffer-tmux-id one))))
          ;; A finished CLI closes its own view, rather than showing another
          ;; project under this project's buffer name.
          (should (enkan-repl--terminal-send one "exit" t))
          (should (enkan-test-vterm--wait (lambda () (not (buffer-live-p one)))))
          (should (buffer-live-p two))
          (should (plist-get (enkan-repl--stop-workspace-terminals "93") :tmux-killed))
          (should-not (buffer-live-p one))
          (should-not (buffer-live-p two))
          (should (enkan-repl--terminal-tmux--has-session "enkan-vterm-test-92")))
      (dolist (buffer (list one two other))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (when (file-executable-p wrapper)
        (enkan-repl--terminal-tmux--call '("kill-server")))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer) (string-prefix-p root (buffer-file-name buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(provide 'enkan-repl-vterm-integration-test)
;;; enkan-repl-vterm-integration-test.el ends here

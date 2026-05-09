;;; enkan-repl-workspace-setup-delete-test.el --- Tests for workspace setup/delete -*- lexical-binding: t -*-

;;; Commentary:
;; Test that setup/delete operations are workspace-scoped.

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-utils)

(ert-deftest test-enkan-repl-setup-delete-workspace-scope ()
  "Test that setup and delete operations are workspace-scoped."
  ;; Test setup: ensure we have a workspace
  (should (equal enkan-repl--current-workspace "01"))
  
  ;; Test that setup functions use workspace accessors
  ;; This is verified by the fact that the functions use enkan-repl--ws-* functions
  
  ;; Test setup log state uses workspace accessors
  ;; The function uses princ which outputs to standard-output, not to the buffer itself
  ;; So we test that the function runs without error and accepts workspace accessors
  (let ((test-output ""))
    (cl-letf (((symbol-function 'princ)
               (lambda (str) (setq test-output (concat test-output str)))))
      (let ((buffer-name "*test-setup*"))
        (with-temp-buffer
          (rename-buffer buffer-name)
          (enkan-repl--setup-log-state buffer-name "Test" 
                                       (enkan-repl--ws-current-project)
                                       (enkan-repl--ws-session-list)
                                       (enkan-repl--ws-session-counter))
          ;; Check the output contains expected text
          (should (string-match-p "Test state" test-output))))))
  
  ;; Test reset config uses workspace accessors
  (let ((test-output ""))
    (cl-letf (((symbol-function 'princ)
               (lambda (str) (setq test-output (concat test-output str)))))
      (let ((buffer-name "*test-reset*"))
        (with-temp-buffer
          (rename-buffer buffer-name)
          (enkan-repl--setup-reset-config buffer-name)
          ;; Verify workspace state was reset
          (should (null (enkan-repl--ws-session-list)))
          (should (= (enkan-repl--ws-session-counter) 0))
          (should (null (enkan-repl--ws-current-project)))
          ;; Check output mentions reset
          (should (string-match-p "Reset" test-output)))))))

(ert-deftest test-enkan-repl-stop-workspace-terminals-kills-stray-buffers ()
  "Stopping a workspace kills all enkan buffers for it, including stray mirrors."
  (let ((enkan-repl-terminal-backend 'tmux)
        (killed-workspaces nil)
        (buf1 (generate-new-buffer "*ws:08 enkan:/tmp/project/*"))
        (buf2 (generate-new-buffer "*ws:08 enkan:/tmp/stray/*"))
        (other (generate-new-buffer "*ws:09 enkan:/tmp/project/*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (setq-local enkan-repl--tmux-mirror-id "enkan-08:project"))
          (with-current-buffer buf2
            (setq-local enkan-repl--tmux-mirror-id "enkan-08:stray"))
          (with-current-buffer other
            (setq-local enkan-repl--tmux-mirror-id "enkan-09:project"))
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-kill-workspace)
                     (lambda (workspace-id)
                       (push workspace-id killed-workspaces)
                       t)))
            (let ((result (enkan-repl--stop-workspace-terminals "08")))
              (should (= 2 (plist-get result :buffers-killed)))
              (should (equal '("08") killed-workspaces))
              (should-not (buffer-live-p buf1))
              (should-not (buffer-live-p buf2))
              (should (buffer-live-p other)))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest test-enkan-repl-delete-workspace-completely-current ()
  "Deleting the current workspace stops terminals and moves to a remaining workspace."
  (let ((enkan-repl--workspaces '(("08" . (:current-project "p"
                                           :session-list ((1 . "p"))
                                           :session-counter 1
                                           :project-aliases nil))
                                  ("09" . (:current-project "q"
                                           :session-list nil
                                           :session-counter 0
                                           :project-aliases nil))))
        (enkan-repl--current-workspace "08")
        (enkan-repl-session-list '((1 . "p")))
        (stopped nil)
        (loaded nil))
    (cl-letf (((symbol-function 'enkan-repl--stop-workspace-terminals)
               (lambda (workspace-id)
                 (push workspace-id stopped)
                 (list :buffers-killed 0 :tmux-killed t)))
              ((symbol-function 'enkan-repl--load-workspace-state)
               (lambda (workspace-id)
                 (setq loaded workspace-id))))
      (let ((result (enkan-repl--delete-workspace-completely "08")))
        (should (plist-get result :deleted))
        (should (equal '("08") stopped))
        (should-not (assoc "08" enkan-repl--workspaces))
        (should (string= "09" enkan-repl--current-workspace))
        (should (string= "09" loaded))
        (should (null enkan-repl-session-list))))))

(ert-deftest test-enkan-repl-workspace-delete-current-delegates-to-complete-delete ()
  "`enkan-repl-workspace-delete' is the only current-workspace deletion command."
  (let ((enkan-repl--current-workspace "08")
        (enkan-repl--workspaces '(("08" . (:current-project "p"
                                           :session-list nil
                                           :session-counter 0
                                           :project-aliases nil))))
        (deleted nil))
    (should-not (fboundp 'enkan-repl-teardown))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'enkan-repl--delete-workspace-completely)
               (lambda (workspace-id)
                 (setq deleted workspace-id)
                 (list :deleted t :stop-result nil)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (enkan-repl-workspace-delete)
      (should (string= "08" deleted)))))

(ert-deftest test-enkan-repl-workspace-delete-delegates-to-complete-delete ()
  "`enkan-repl-workspace-delete' accepts a workspace id and uses complete delete."
  (let ((enkan-repl--workspaces '(("08" . (:current-project "p"
                                           :session-list nil
                                           :session-counter 0
                                           :project-aliases nil))))
        (enkan-repl--current-workspace "08")
        (deleted nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'enkan-repl--delete-workspace-completely)
               (lambda (workspace-id)
                 (setq deleted workspace-id)
                 (list :deleted t :stop-result nil)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (enkan-repl-workspace-delete "08")
      (should (string= "08" deleted)))))

(ert-deftest test-enkan-repl-setup-delete-docstrings ()
  "Test that setup/delete docstrings mention workspace context."
  ;; Check setup docstring mentions workspace
  (should (string-match-p "workspace" 
                          (documentation 'enkan-repl-setup)))
  
  ;; Check delete docstring mentions workspace
  (should (string-match-p "workspace"
                          (documentation 'enkan-repl-workspace-delete)))
  
  ;; Check complete-delete helper mentions workspace
  (should (string-match-p "workspace"
                          (documentation 'enkan-repl--delete-workspace-completely))))

(provide 'enkan-repl-workspace-setup-delete-test)
;;; enkan-repl-workspace-setup-delete-test.el ends here

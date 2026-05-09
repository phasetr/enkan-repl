;;; enkan-repl-start-eat-session-test.el --- Test for session registration on eat start -*- lexical-binding: t -*-

;;; Commentary:
;; Test that enkan-repl-start-eat properly registers sessions

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(defun enkan-repl-test--kill-stale-enkan-buffers ()
  "Kill any leftover enkan buffers from prior test runs.
The terminal abstraction now derives multi-instance index from buffer
name suffix, so leftover same-named buffers would otherwise inflate the
detected instance to <2>, <3>, ... and break tests that assume a fresh
workspace starts at instance 1."
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and (stringp name)
                 (string-match-p "^\\*ws:[0-9]\\{2\\} enkan:" name))
        (kill-buffer buf)))))

(ert-deftest test-start-eat-registers-session ()
  "Test that enkan-repl-start-eat registers session and saves workspace."
  (enkan-repl-test--kill-stale-enkan-buffers)
  ;; Exercises the eat backend explicitly (mocks `eat').
  (let ((enkan-repl-terminal-backend 'eat)
        (enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (default-directory "/path/to/er/")
        (mock-eat-buffer nil))

    ;; Create workspace
    (setq enkan-repl--workspaces
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl--current-project "er")
    
    ;; Mock eat function to return a buffer
    (cl-letf (((symbol-function 'eat)
               (lambda ()
                 (setq mock-eat-buffer (generate-new-buffer "*eat*"))
                 mock-eat-buffer))
              ((symbol-function 'require)
               (lambda (_) t)))
      
      ;; Call enkan-repl-start-eat
      (enkan-repl-start-eat)
      
      ;; Session should be registered
      (should (equal '((1 . "er")) enkan-repl-session-list))
      (should (= 1 enkan-repl--session-counter))
      
      ;; Workspace state should be saved
      (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
        (should saved-state)
        (should (equal '((1 . "er")) (plist-get saved-state :session-list)))
        (should (= 1 (plist-get saved-state :session-counter))))
      
      ;; Clean up
      (when (buffer-live-p mock-eat-buffer)
        (kill-buffer mock-eat-buffer)))))

(ert-deftest test-multiple-eat-sessions-in-workspace ()
  "Test that multiple eat sessions are properly registered."
  (enkan-repl-test--kill-stale-enkan-buffers)
  ;; Exercises the eat backend explicitly (mocks `eat').
  (let ((enkan-repl-terminal-backend 'eat)
        (enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (default-directory "/path/to/er/")
        (mock-buffers '()))

    ;; Create workspace
    (setq enkan-repl--workspaces
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl--current-project "er")
    
    ;; Mock eat function
    (cl-letf (((symbol-function 'eat)
               (lambda ()
                 (let ((buf (generate-new-buffer "*eat*")))
                   (push buf mock-buffers)
                   buf)))
              ((symbol-function 'require)
               (lambda (_) t)))
      
      ;; Start first eat session (instance 1 -> legacy string form)
      (enkan-repl-start-eat)
      (should (equal '((1 . "er")) enkan-repl-session-list))

      ;; Start second eat session (instance 2 -> cons form)
      (enkan-repl-start-eat)
      (should (equal '((1 . "er") (2 "er" . 2)) enkan-repl-session-list))
      (should (= 2 enkan-repl--session-counter))

      ;; Workspace state should have both sessions
      (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
        (should (equal '((1 . "er") (2 "er" . 2)) (plist-get saved-state :session-list))))
      
      ;; Clean up
      (dolist (buf mock-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'enkan-repl-start-eat-session-test)
;;; enkan-repl-start-eat-session-test.el ends here
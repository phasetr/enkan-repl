;;; enkan-repl-start-eat-session-test.el --- Test for session registration on eat start -*- lexical-binding: t -*-

;;; Commentary:
;; Test that enkan-repl-start-eat properly registers sessions

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-start-eat-registers-session ()
  "Test that enkan-repl-start-eat registers session and saves workspace."
  (let ((enkan-repl--workspaces '())
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
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
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
  (let ((enkan-repl--workspaces '())
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
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
               (lambda ()
                 (let ((buf (generate-new-buffer "*eat*")))
                   (push buf mock-buffers)
                   buf)))
              ((symbol-function 'require)
               (lambda (_) t)))
      
      ;; Start first eat session
      (enkan-repl-start-eat)
      (should (equal '((1 . "er")) enkan-repl-session-list))
      
      ;; Start second eat session
      (enkan-repl-start-eat)
      (should (equal '((1 . "er") (2 . "er")) enkan-repl-session-list))
      (should (= 2 enkan-repl--session-counter))
      
      ;; Workspace state should have both sessions
      (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
        (should (equal '((1 . "er") (2 . "er")) (plist-get saved-state :session-list))))
      
      ;; Clean up
      (dolist (buf mock-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'enkan-repl-start-eat-session-test)
;;; enkan-repl-start-eat-session-test.el ends here
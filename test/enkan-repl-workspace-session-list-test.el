;;; enkan-repl-workspace-session-list-test.el --- Test for session list in workspace switching -*- lexical-binding: t -*-

;;; Commentary:
;; Test case for verifying session list is correctly maintained when switching workspaces

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-workspace-session-list-preservation ()
  "Test that session list is preserved when switching workspaces."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er")))))
    
    ;; Create and setup workspace 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "01")
    
    ;; Verify session list is saved
    (let ((saved-state (cdr (assoc "01" enkan-repl--workspaces))))
      (should saved-state)
      (should (equal '((1 . "er")) (plist-get saved-state :session-list))))
    
    ;; Create and setup workspace 02
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "02")
    
    ;; Clear current session list to test loading
    (setq enkan-repl--session-list nil)
    (setq enkan-repl-session-list nil)
    
    ;; Switch back to workspace 01 and load state
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Verify session list is restored
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (equal "er" enkan-repl--current-project))
    (should (equal '("er-alias") enkan-repl-project-aliases))))

(ert-deftest test-workspace-session-buffer-setup ()
  "Test that buffer setup works with correct session list."
  
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-session-list '((1 . "er")))
        (enkan-repl--session-list '((1 . "er")))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er")))))
    
    ;; Test enkan-repl--ws-session-list getter
    (should (equal '((1 . "er")) (enkan-repl--ws-session-list)))
    
    ;; Test buffer name generation
    (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1
                     (enkan-repl--ws-session-list)
                     enkan-repl-target-directories)))
        
        (should result)
        (should (eq 'test-window (car result)))
        (should (string-match-p "\\*ws:01 enkan:/path/to/er/\\*" (cdr result)))))))

(ert-deftest test-workspace-session-after-switch ()
  "Test session handling after workspace switch."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))))
        (enkan-repl-center-file "/path/to/center.md"))
    
    ;; Setup workspace 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "01")
    
    ;; Setup workspace 02
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "02")
    
    ;; Switch to ws:01
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Mock buffer operations
    (cl-letf (((symbol-function 'select-window) (lambda (w) t))
              ((symbol-function 'get-buffer)
               (lambda (name)
                 ;; Return nil for eat buffers (not created yet)
                 (when (string-match "\\*ws:" name)
                   nil)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 ;; Capture message output
                 (let ((msg (apply #'format fmt args)))
                   ;; Check if correct error message is shown
                   (when (string-match "not found\\|No session" msg)
                     msg)))))
      
      ;; Test setup-session-eat-buffer behavior
      (when (fboundp 'enkan-repl--setup-session-eat-buffer)
        (let ((msg (enkan-repl--setup-session-eat-buffer 'test-window 1)))
          ;; Should show message about missing buffer or no session
          (should (or (string-match-p "not found" msg)
                      (string-match-p "No session" msg))))))))

(provide 'enkan-repl-workspace-session-list-test)
;;; enkan-repl-workspace-session-list-test.el ends here
;;; debug-workspace-issue.el --- Debug actual workspace issue -*- lexical-binding: t -*-

;;; Commentary:
;; Debug test to identify the actual problem with workspace switching

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest debug-workspace-switching-issue ()
  "Debug the actual issue with workspace switching."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories '())
        (enkan-repl-center-file "/path/to/center.md")
        (debug-log '()))
    
    ;; Helper function to log state
    (cl-flet ((log-state (context)
                (push (list :context context
                           :workspace enkan-repl--current-workspace
                           :session-list enkan-repl-session-list
                           :target-dirs enkan-repl-target-directories
                           :project-aliases enkan-repl-project-aliases)
                      debug-log)))
      
      ;; Setup workspace 01
      (log-state "Before ws:01 setup")
      (setq enkan-repl--workspaces 
            (enkan-repl--add-workspace enkan-repl--workspaces "01"))
      (setq enkan-repl--current-workspace "01")
      (setq enkan-repl-session-list '((1 . "er")))
      (setq enkan-repl--session-counter 1)
      (setq enkan-repl--current-project "er")
      (setq enkan-repl-project-aliases '("er-alias"))
      ;; Manually set target-directories for ws:01
      (setq enkan-repl-target-directories 
            '(("er-alias" . ("er" . "/path/to/er"))))
      (enkan-repl--save-workspace-state "01")
      (log-state "After ws:01 saved")
      
      ;; Setup workspace 02
      (setq enkan-repl--workspaces 
            (enkan-repl--add-workspace enkan-repl--workspaces "02"))
      (setq enkan-repl--current-workspace "02")
      (setq enkan-repl-session-list '((1 . "er")))
      (setq enkan-repl--session-counter 1)
      (setq enkan-repl--current-project "er")
      (setq enkan-repl-project-aliases '("er-alias"))
      ;; Manually set target-directories for ws:02
      (setq enkan-repl-target-directories 
            '(("er-alias" . ("er" . "/path/to/er"))))
      (enkan-repl--save-workspace-state "02")
      (log-state "After ws:02 saved")
      
      ;; Switch to ws:02
      (setq enkan-repl--current-workspace "02")
      (enkan-repl--load-workspace-state "02")
      (log-state "After switch to ws:02")
      
      ;; Switch back to ws:01
      (setq enkan-repl--current-workspace "01")
      (enkan-repl--load-workspace-state "01")
      (log-state "After switch back to ws:01")
      
      ;; Print debug log
      (message "\n=== DEBUG LOG ===")
      (dolist (entry (reverse debug-log))
        (message "Context: %s" (plist-get entry :context))
        (message "  Workspace: %s" (plist-get entry :workspace))
        (message "  Session List: %s" (plist-get entry :session-list))
        (message "  Target Dirs: %s" (plist-get entry :target-dirs))
        (message "  Project Aliases: %s" (plist-get entry :project-aliases))
        (message ""))
      
      ;; Assertions
      (should (equal "01" enkan-repl--current-workspace))
      (should (equal '((1 . "er")) enkan-repl-session-list))
      (should (equal '("er-alias") enkan-repl-project-aliases))
      ;; This is the problem - target-directories might be empty
      (should enkan-repl-target-directories)
      
      ;; Test setup-window-eat-buffer-pure
      (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
        (let ((result (enkan-repl--setup-window-eat-buffer-pure
                       'test-window 1
                       enkan-repl-session-list
                       enkan-repl-target-directories)))
          (message "setup-window-eat-buffer-pure result: %s" result)
          (should result)
          (when result
            (should (string-match-p "\\*ws:01 enkan:/path/to/er\\*" (cdr result)))))))))

(provide 'debug-workspace-issue)
;;; debug-workspace-issue.el ends here
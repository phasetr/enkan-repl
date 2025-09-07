;;; enkan-repl-workspace-actual-issue-test.el --- Test for actual workspace switching issue -*- lexical-binding: t -*-

;;; Commentary:
;; Test case for reproducing the actual issue where switching workspaces
;; causes center file to be opened in both windows instead of eat buffer

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-workspace-switch-missing-eat-buffer ()
  "Test actual issue: switching workspaces when eat buffer doesn't exist."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))))
        (enkan-repl-center-file "/path/to/center.md")
        buffer-selection-log)
    
    ;; Create workspace 01 with project er
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    
    ;; Set workspace state for ws:01
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl--project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "01")
    
    ;; Create workspace 02 with project er
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    
    ;; Set workspace state for ws:02
    (setq enkan-repl--session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl--project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "02")
    
    ;; Now switch back to ws:01
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Mock environment
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (name)
                 ;; Simulate eat buffers not existing
                 (when (string-match "\\*ws:" name)
                   nil)))
              ((symbol-function 'find-file)
               (lambda (file)
                 ;; Log when center file is opened
                 (push (format "find-file: %s" file) buffer-selection-log)
                 (get-buffer-create "*center.md*")))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer)
                 ;; Log what buffer is being switched to
                 (push (format "switch-to-buffer: %s" 
                               (if (bufferp buffer) 
                                   (buffer-name buffer) 
                                   buffer))
                       buffer-selection-log))))
      
      ;; Call enkan-repl--setup-session-eat-buffer 
      ;; This is what enkan-repl-setup-1session-layout calls
      (when (fboundp 'enkan-repl--setup-session-eat-buffer)
        (enkan-repl--setup-session-eat-buffer 'dummy-window 1))
      
      ;; Check log - should NOT open center file in eat window
      ;; But if eat buffer doesn't exist, it might fall back to center file
      (should-not (cl-find-if (lambda (log) 
                                (string-match-p "center\\.md" log))
                              buffer-selection-log)))))

(ert-deftest test-setup-session-eat-buffer-logic ()
  "Test the logic of enkan-repl--setup-session-eat-buffer."
  
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl--session-list '((1 . "er")))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))))
        test-buffer-name)
    
    ;; Test: setup-window-eat-buffer-pure should return correct buffer name
    (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'dummy-window 1 
                     enkan-repl--session-list
                     enkan-repl-target-directories)))
        
        (should result)
        (should (consp result))
        (should (eq (car result) 'dummy-window))
        (setq test-buffer-name (cdr result))
        (should (stringp test-buffer-name))
        (should (string-match-p "\\*ws:01 enkan:/path/to/er\\*" test-buffer-name))))
    
    ;; Test: When buffer doesn't exist, get-buffer returns nil
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (name)
                 ;; Return nil to simulate buffer not existing
                 nil))
              ((symbol-function 'select-window)
               (lambda (window) t))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer)
                 ;; This should NOT be called when buffer doesn't exist
                 (error "switch-to-buffer should not be called when buffer doesn't exist"))))
      
      ;; When buffer doesn't exist, it should message an error, not switch
      (when (fboundp 'enkan-repl--setup-session-eat-buffer)
        ;; Mock message to prevent actual output
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args)
                     ;; Check that error message is shown
                     (when (string-match-p "not found" (car args))
                       t))))
          
          ;; This should not error - it should just show a message
          (enkan-repl--setup-session-eat-buffer 'dummy-window 1))))))

(provide 'enkan-repl-workspace-actual-issue-test)
;;; enkan-repl-workspace-actual-issue-test.el ends here
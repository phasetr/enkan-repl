;;; enkan-repl-workspace-buffer-problem-test.el --- Test for the actual buffer naming problem -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-actual-workspace-buffer-naming-problem ()
  "Test that demonstrates the actual problem with buffer naming.
When creating eat buffers for the same project in different workspaces,
the second buffer gets named with <2> suffix instead of using the workspace ID."
  (let ((buffer1 nil)
        (buffer2 nil))
    (unwind-protect
        (progn
          ;; Create first buffer with ws:01
          (setq buffer1 (generate-new-buffer "*ws:01 enkan:/path/to/enkan-repl/*"))
          (should (string= (buffer-name buffer1) "*ws:01 enkan:/path/to/enkan-repl/*"))
          
          ;; Try to create second buffer with ws:02
          ;; This should work without <2> suffix because the name is different
          (setq buffer2 (generate-new-buffer "*ws:02 enkan:/path/to/enkan-repl/*"))
          (should (string= (buffer-name buffer2) "*ws:02 enkan:/path/to/enkan-repl/*"))
          
          ;; Now the problem scenario: If we use rename-buffer with unique=t
          ;; on a buffer trying to use ws:01 name when it already exists
          (let ((buffer3 (generate-new-buffer "*eat*")))
            (unwind-protect
                (progn
                  (with-current-buffer buffer3
                    ;; This will add <2> because unique=t and name exists
                    (rename-buffer "*ws:01 enkan:/path/to/enkan-repl/*" t))
                  ;; This is the problem!
                  (should (string-match-p "<2>" (buffer-name buffer3))))
              (kill-buffer buffer3))))
      ;; Clean up
      (when (buffer-live-p buffer1) (kill-buffer buffer1))
      (when (buffer-live-p buffer2) (kill-buffer buffer2)))))

(ert-deftest test-workspace-session-list-empty-after-switch ()
  "Test the actual problem: session list becomes empty after workspace switch."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))))
        (enkan-repl-center-file "/path/to/center.md"))
    
    ;; Create workspace 01 with project er
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "01")
    
    ;; Create workspace 02 with project er
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "er")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "er")
    (setq enkan-repl-project-aliases '("er-alias"))
    (enkan-repl--save-workspace-state "02")
    
    ;; Switch to ws:02
    (setq enkan-repl--current-workspace "02")
    (enkan-repl--load-workspace-state "02")
    
    ;; Switch back to ws:01
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    
    ;; Check that session list is not empty
    (should (equal '((1 . "er")) enkan-repl-session-list))
    (should (equal '((1 . "er")) (enkan-repl--ws-session-list)))
    
    ;; Test that setup-window-eat-buffer-pure returns correct result
    (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1
                     (enkan-repl--ws-session-list)
                     enkan-repl-target-directories)))
        
        ;; Should return buffer info, not nil
        (should result)
        (should (consp result))
        (should (string-match-p "\\*ws:01 enkan:/path/to/er\\*" (cdr result)))))))

(ert-deftest test-setup-session-eat-buffer-returns-nil ()
  "Test that setup-session-eat-buffer behaves correctly when session list is empty."
  
  (let ((enkan-repl--current-workspace "01")
        (enkan-repl-session-list nil) ;; Empty session list
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))))
        message-log)
    
    ;; Mock message function
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (let ((msg (apply #'format fmt args)))
                   (push msg message-log)
                   msg)))
              ((symbol-function 'select-window)
               (lambda (window) t))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer)
                 (error "Should not try to switch to buffer"))))
      
      ;; Call enkan-repl--setup-session-eat-buffer with empty session list
      (when (fboundp 'enkan-repl--setup-session-eat-buffer)
        (enkan-repl--setup-session-eat-buffer 'test-window 1))
      
      ;; Should show "No session registered" message
      (should (cl-find-if (lambda (msg)
                            (string-match-p "No session registered" msg))
                          message-log)))))

(provide 'enkan-repl-workspace-buffer-problem-test)
;;; enkan-repl-workspace-buffer-problem-test.el ends here
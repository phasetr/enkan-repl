;;; enkan-repl-workspace-target-directories-test.el --- Test for workspace-specific target directories -*- lexical-binding: t -*-

;;; Commentary:
;; Test case for verifying that enkan-repl-target-directories is handled correctly per workspace

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-workspace-target-directories-isolation ()
  "Test that target directories are isolated per workspace."
  
  ;; Setup: Create two workspaces with different projects
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))
                                ("project-b" . ("pb-alias"))))
        ;; Global target directories should contain both projects
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er"))
           ("pb-alias" . ("project-b" . "/path/to/project-b"))))
        test-ws1 test-ws2)
    
    ;; Create workspace 01 with project er
    (setq test-ws1 "01")
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces test-ws1))
    (setq enkan-repl--current-workspace test-ws1)
    
    ;; Mock workspace state for ws:01 with project er
    (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
               (lambda (ws-id)
                 (when (string= ws-id test-ws1)
                   (setq enkan-repl--session-list '((1 . "er")))
                   (setq enkan-repl--session-counter 1)
                   (setq enkan-repl--current-project "er")
                   (setq enkan-repl--project-aliases '("er-alias")))))
              ((symbol-function 'enkan-repl--save-workspace-state)
               (lambda (ws-id) t)))
      
      (enkan-repl--load-workspace-state test-ws1)
      
      ;; Test: In ws:01, should find er project path
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'dummy-window 1 
                     '((1 . "er"))
                     enkan-repl-target-directories)))
        (should result)
        (should (string-match-p "\\*ws:01 enkan:/path/to/er/\\*" (cdr result))))
      
      ;; Create workspace 02 with project-b
      (setq test-ws2 "02")
      (setq enkan-repl--workspaces
            (enkan-repl--add-workspace enkan-repl--workspaces test-ws2))
      (setq enkan-repl--current-workspace test-ws2)
      
      ;; Mock workspace state for ws:02 with project-b
      (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
                 (lambda (ws-id)
                   (when (string= ws-id test-ws2)
                     (setq enkan-repl--session-list '((1 . "project-b")))
                     (setq enkan-repl--session-counter 1)
                     (setq enkan-repl--current-project "project-b")
                     (setq enkan-repl--project-aliases '("pb-alias"))))))
        
        (enkan-repl--load-workspace-state test-ws2)
        
        ;; Test: In ws:02, should find project-b path 
        (let ((result (enkan-repl--setup-window-eat-buffer-pure
                       'dummy-window 1
                       '((1 . "project-b"))
                       enkan-repl-target-directories)))
          (should result)
          (should (string-match-p "\\*ws:02 enkan:/path/to/project-b/\\*" (cdr result))))
        
        ;; Now switch back to ws:01
        (setq enkan-repl--current-workspace test-ws1)
        
        ;; Mock reload state for ws:01
        (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
                   (lambda (ws-id)
                     (when (string= ws-id test-ws1)
                       (setq enkan-repl--session-list '((1 . "er")))
                       (setq enkan-repl--session-counter 1)
                       (setq enkan-repl--current-project "er")
                       (setq enkan-repl--project-aliases '("er-alias"))))))
          
          (enkan-repl--load-workspace-state test-ws1)
          
          ;; Test: After switching back to ws:01, should still find er project
          (let ((result (enkan-repl--setup-window-eat-buffer-pure
                         'dummy-window 1
                         '((1 . "er"))
                         enkan-repl-target-directories)))
            (should result)
            (should (string-match-p "\\*ws:01 enkan:/path/to/er/\\*" (cdr result)))))))))

(ert-deftest test-workspace-missing-eat-buffer-selection ()
  "Test that missing eat buffers are handled correctly when switching workspaces."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories 
         '(("er-alias" . ("er" . "/path/to/er")))))
    
    ;; Create workspace 01
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    
    ;; Test when buffer doesn't exist
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (name)
                 ;; Simulate buffer not existing
                 nil)))
      
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'dummy-window 1
                     '((1 . "er"))
                     enkan-repl-target-directories)))
        ;; Should still return the buffer name even if buffer doesn't exist
        (should result)
        (should (string-match-p "\\*ws:01 enkan:/path/to/er/\\*" (cdr result)))))))

(provide 'enkan-repl-workspace-target-directories-test)
;;; enkan-repl-workspace-target-directories-test.el ends here
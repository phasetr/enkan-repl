;;; enkan-repl-workspace-switch-layout-test.el --- Test for workspace switching layout issues -*- lexical-binding: t -*-

;;; Commentary:
;; Test case for reproducing the issue where switching workspaces
;; causes incorrect buffer selection in enkan-repl-setup-current-project-layout

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest test-workspace-switch-buffer-selection ()
  "Test that switching workspaces maintains correct buffer selection."
  
  ;; Setup: Create two workspaces with project 'er'
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        test-ws1 test-ws2)
    
    ;; Create workspace 01 with project er
    (setq test-ws1 "01")
    (setq enkan-repl--workspaces 
          (enkan-repl--add-workspace enkan-repl--workspaces test-ws1))
    (setq enkan-repl--current-workspace test-ws1)
    
    ;; Mock workspace state for ws:01
    (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
               (lambda (ws-id)
                 (when (string= ws-id test-ws1)
                   ;; Set state for ws:01
                   (setq enkan-repl--session-list '((1 . "er")))
                   (setq enkan-repl--session-counter 1)
                   (setq enkan-repl--current-project "er")
                   (setq enkan-repl--project-aliases '("er-alias")))))
              ((symbol-function 'enkan-repl--save-workspace-state)
               (lambda (ws-id) t)))
      
      ;; Load ws:01 state
      (enkan-repl--load-workspace-state test-ws1)
      
      ;; Create workspace 02 with project er  
      (setq test-ws2 "02")
      (setq enkan-repl--workspaces
            (enkan-repl--add-workspace enkan-repl--workspaces test-ws2))
      (setq enkan-repl--current-workspace test-ws2)
      
      ;; Mock workspace state for ws:02
      (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
                 (lambda (ws-id)
                   (when (string= ws-id test-ws2)
                     ;; Set state for ws:02
                     (setq enkan-repl--session-list '((1 . "er")))
                     (setq enkan-repl--session-counter 1)
                     (setq enkan-repl--current-project "er")
                     (setq enkan-repl--project-aliases '("er-alias"))))))
        
        ;; Load ws:02 state
        (enkan-repl--load-workspace-state test-ws2)
        
        ;; Now switch back to ws:01
        (setq enkan-repl--current-workspace test-ws1)
        
        ;; Mock load state for ws:01 again
        (cl-letf (((symbol-function 'enkan-repl--load-workspace-state)
                   (lambda (ws-id)
                     (when (string= ws-id test-ws1)
                       (setq enkan-repl--session-list '((1 . "er")))
                       (setq enkan-repl--session-counter 1)
                       (setq enkan-repl--current-project "er")
                       (setq enkan-repl--project-aliases '("er-alias"))))))
          
          (enkan-repl--load-workspace-state test-ws1)
          
          ;; Test: setup-current-project-layout should select correct buffers
          ;; The function should respect current workspace ID
          (when (fboundp 'enkan-repl-setup-current-project-layout)
            
            ;; Mock window configuration
            (cl-letf (((symbol-function 'delete-other-windows) (lambda () t))
                      ((symbol-function 'split-window-horizontally) (lambda () 'mock-window))
                      ((symbol-function 'windmove-right) (lambda () t))
                      ((symbol-function 'split-window-vertically) (lambda () 'mock-window))
                      ((symbol-function 'windmove-down) (lambda () t))
                      ((symbol-function 'windmove-left) (lambda () t))
                      ((symbol-function 'windmove-up) (lambda () t))
                      ((symbol-function 'balance-windows) (lambda () t))
                      ((symbol-function 'get-buffer) 
                       (lambda (name) 
                         ;; Return mock buffer for workspace-specific names
                         (when (or (string-match "\\*ws:01 " name)
                                   (string-match "\\*ws:02 " name)
                                   (string-match "center\\.md" name))
                           (get-buffer-create name))))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buffer)
                         ;; Verify that correct workspace buffer is selected
                         (cond
                          ;; When in ws:01, should select ws:01 buffers
                          ((string= enkan-repl--current-workspace test-ws1)
                           (should (or (string-match-p "\\*ws:01 " (buffer-name buffer))
                                       (string-match-p "center\\.md" (buffer-name buffer)))))
                          ;; When in ws:02, should select ws:02 buffers  
                          ((string= enkan-repl--current-workspace test-ws2)
                           (should (or (string-match-p "\\*ws:02 " (buffer-name buffer))
                                       (string-match-p "center\\.md" (buffer-name buffer)))))))))
              
              ;; This should work correctly for ws:01
              (enkan-repl-setup-current-project-layout))))))))

(ert-deftest test-workspace-buffer-selection-pure ()
  "Test pure function buffer selection logic for workspaces."
  
  ;; Test that enkan-repl--setup-window-eat-buffer-pure respects workspace ID
  (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
    
    ;; Test with ws:01
    (let ((enkan-repl--current-workspace "01")
          (session-list '((1 . "er")))
          (project-registry '(("er-alias" . ("er" . "/path/to/er")))))
      
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'dummy-window 1 session-list project-registry)))
        (when result
          ;; Should return ws:01 buffer name
          (should (string-match-p "\\*ws:01 " (cdr result))))))
    
    ;; Test with ws:02  
    (let ((enkan-repl--current-workspace "02")
          (session-list '((1 . "er")))
          (project-registry '(("er-alias" . ("er" . "/path/to/er")))))
      
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'dummy-window 1 session-list project-registry)))
        (when result
          ;; Should return ws:02 buffer name
          (should (string-match-p "\\*ws:02 " (cdr result))))))))

(provide 'enkan-repl-workspace-switch-layout-test)
;;; enkan-repl-workspace-switch-layout-test.el ends here
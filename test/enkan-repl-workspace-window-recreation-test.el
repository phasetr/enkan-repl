;;; enkan-repl-workspace-window-recreation-test.el --- Test actual window recreation after workspace switch -*- lexical-binding: t -*-

;;; Commentary:
;; Test that workspace switching correctly recreates window layout with actual buffers

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(load (expand-file-name "examples/window-layouts.el" default-directory) nil t)

(ert-deftest test-workspace-window-recreation-after-switch ()
  "Test that workspace switching correctly recreates windows with buffers."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-center-file "/tmp/test-center.txt")
        (default-directory default-directory)
        (buffer-ws01 nil)
        (buffer-ws02 nil)
        (window-setup-calls '()))
    
    (unwind-protect
        (progn
          ;; Create center file for testing
          (with-temp-file enkan-repl-center-file
            (insert "Test center file content"))
          
          (message "\n=== Create WS 01 with eat buffer ===")
          ;; Create WS 01
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "01" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create eat buffer for WS 01
          (setq buffer-ws01 (generate-new-buffer "*ws:01 enkan:/tmp/er/*"))
          (with-current-buffer buffer-ws01
            (setq-local eat--process (start-process "mock-eat-1" buffer-ws01 "cat")))
          
          ;; Register session
          (setq enkan-repl--session-counter 1)
          (enkan-repl--register-session 1 "er")
          (enkan-repl--save-workspace-state)
          
          (message "\n=== Create WS 02 with eat buffer ===")
          ;; Create WS 02
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "02" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create eat buffer for WS 02
          (setq buffer-ws02 (generate-new-buffer "*ws:02 enkan:/tmp/er/*"))
          (with-current-buffer buffer-ws02
            (setq-local eat--process (start-process "mock-eat-2" buffer-ws02 "cat")))
          
          ;; Register session
          (setq enkan-repl--session-counter 1)
          (enkan-repl--register-session 1 "er")
          (enkan-repl--save-workspace-state)
          
          (message "\n=== Switch back to WS 01 and test window recreation ===")
          ;; Switch back to WS 01
          (setq enkan-repl--current-workspace "01")
          (enkan-repl--load-workspace-state "01")
          
          ;; Verify correct buffer count
          (let ((buffer-count (enkan-repl--get-workspace-buffer-count-pure 
                               (buffer-list) "01")))
            (should (= 1 buffer-count))
            (message "WS 01 buffer count: %d" buffer-count))
          
          ;; Mock window setup functions to track what happens
          (let ((window-1-called nil)
                (window-2-buffer nil))
            
            (cl-letf (((symbol-function 'delete-other-windows)
                       (lambda () (message "delete-other-windows called")))
                      ((symbol-function 'split-window-right)
                       (lambda () (message "split-window-right called")))
                      ((symbol-function 'selected-window)
                       (lambda () 'mock-window))
                      ((symbol-function 'other-window)
                       (lambda (_) (message "other-window called")))
                      ((symbol-function 'select-window)
                       (lambda (window)
                         (message "select-window called with: %S" window)))
                      ((symbol-function 'find-file)
                       (lambda (file)
                         (setq window-1-called file)
                         (message "find-file called with: %s" file)))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buffer)
                         (setq window-2-buffer buffer)
                         (message "switch-to-buffer called with: %S" (buffer-name buffer))))
                      ((symbol-function 'get-buffer)
                       (lambda (name)
                         (message "get-buffer called with: %s" name)
                         (when (string= name "*ws:01 enkan:/tmp/er/*")
                           buffer-ws01))))
              
              ;; Call setup-current-project-layout
              (enkan-repl-setup-current-project-layout)
              
              ;; Verify the setup was called correctly
              (should window-1-called)
              (should (string= window-1-called enkan-repl-center-file))
              ;; session-setup-called is no longer used with new implementation
              (should window-2-buffer)
              (should (eq window-2-buffer buffer-ws01))))
          
          (message "\n=== Switch to WS 02 and test window recreation ===")
          ;; Switch to WS 02
          (setq enkan-repl--current-workspace "02")
          (enkan-repl--load-workspace-state "02")
          
          ;; Verify correct buffer count
          (let ((buffer-count (enkan-repl--get-workspace-buffer-count-pure 
                               (buffer-list) "02")))
            (should (= 1 buffer-count))
            (message "WS 02 buffer count: %d" buffer-count))
          
          ;; Test window recreation for WS 02
          (let ((window-2-buffer nil))
            
            (cl-letf (((symbol-function 'delete-other-windows)
                       (lambda () (message "delete-other-windows called")))
                      ((symbol-function 'split-window-right)
                       (lambda () (message "split-window-right called")))
                      ((symbol-function 'selected-window)
                       (lambda () 'mock-window))
                      ((symbol-function 'other-window)
                       (lambda (_) (message "other-window called")))
                      ((symbol-function 'select-window)
                       (lambda (window)
                         (message "select-window called with: %S" window)))
                      ((symbol-function 'find-file)
                       (lambda (file)
                         (message "find-file called with: %s" file)))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buffer)
                         (setq window-2-buffer buffer)
                         (message "switch-to-buffer called with: %S" (buffer-name buffer))))
                      ((symbol-function 'get-buffer)
                       (lambda (name)
                         (message "get-buffer called with: %s" name)
                         (when (string= name "*ws:02 enkan:/tmp/er/*")
                           buffer-ws02))))
              
              ;; Call setup-current-project-layout for WS 02
              (enkan-repl-setup-current-project-layout)
              
              ;; Verify WS 02 buffer was used
              ;; session-setup-called is no longer used with new implementation
              (should window-2-buffer)
              (should (eq window-2-buffer buffer-ws02)))))
      
      ;; Clean up
      (when (file-exists-p enkan-repl-center-file)
        (delete-file enkan-repl-center-file))
      (dolist (buffer (list buffer-ws01 buffer-ws02))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (boundp 'eat--process) eat--process)
              (delete-process eat--process)))
          (kill-buffer buffer))))))

(provide 'enkan-repl-workspace-window-recreation-test)
;;; enkan-repl-workspace-window-recreation-test.el ends here
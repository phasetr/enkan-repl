;;; enkan-repl-workspace-project-binding-test.el --- Test workspace-project binding -*- lexical-binding: t -*-

;;; Commentary:
;; Test that workspace, project, and buffer/session are correctly bound together

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-workspace-project-buffer-binding ()
  "Test that workspace, project, and buffer are correctly bound."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory default-directory)
        (buffer1 nil)
        (buffer2 nil))
    
    (unwind-protect
        (progn
          (message "\n=== Creating WS 01 with project er ===")
          ;; Create WS 01 as center file with project er
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "01" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create buffer for WS 01 with mock eat process  
          (setq buffer1 (generate-new-buffer "*ws:01 enkan:/tmp/er/*"))
          (with-current-buffer buffer1
            ;; Mock eat variables to simulate a valid eat buffer
            (setq-local eat--process (start-process
                                     "mock-eat-1" buffer1 "cat")))
          
          ;; Register session
          (setq enkan-repl--session-counter 1)
          (enkan-repl--register-session 1 "er")
          (enkan-repl--save-workspace-state)
          
          (message "WS 01 state:")
          (message "  Workspace: %s" enkan-repl--current-workspace)
          (message "  Project: %s" enkan-repl--current-project)
          (message "  Sessions: %S" enkan-repl-session-list)
          (message "  Buffer: %s" (buffer-name buffer1))
          
          ;; Verify WS 01 binding
          (should (equal '((1 . "er")) enkan-repl-session-list))
          
          (message "\n=== Creating WS 02 with same project er ===")
          ;; Create WS 02 as center file with same project er
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "02" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create buffer for WS 02 with mock eat process
          (setq buffer2 (generate-new-buffer "*ws:02 enkan:/tmp/er/*"))
          (with-current-buffer buffer2
            ;; Mock eat variables to simulate a valid eat buffer
            (setq-local eat--process (start-process
                                     "mock-eat-2" buffer2 "cat")))
          
          ;; Register session
          (setq enkan-repl--session-counter 1)
          (enkan-repl--register-session 1 "er")
          (enkan-repl--save-workspace-state)
          
          (message "WS 02 state:")
          (message "  Workspace: %s" enkan-repl--current-workspace)
          (message "  Project: %s" enkan-repl--current-project)
          (message "  Sessions: %S" enkan-repl-session-list)
          (message "  Buffer: %s" (buffer-name buffer2))
          
          ;; Verify WS 02 binding
          (should (equal '((1 . "er")) enkan-repl-session-list))
          
          (message "\n=== Checking buffer identification by ws: prefix ===")
          ;; Check that buffers can be identified by ws: prefix
          (should (string-match-p "^\\*ws:01 " (buffer-name buffer1)))
          (should (string-match-p "^\\*ws:02 " (buffer-name buffer2)))
          
          ;; Check that workspace ID can be extracted from buffer name
          (should (string= "01" (enkan-repl--extract-workspace-id (buffer-name buffer1))))
          (should (string= "02" (enkan-repl--extract-workspace-id (buffer-name buffer2))))
          
          (message "\n=== Switching to WS 01 ===")
          ;; Switch to WS 01
          (enkan-repl--save-workspace-state) ; Save WS 02
          (setq enkan-repl--current-workspace "01")
          (enkan-repl--load-workspace-state "01")
          
          (message "After switch to WS 01:")
          (message "  Workspace: %s" enkan-repl--current-workspace)
          (message "  Project: %s" enkan-repl--current-project)
          (message "  Sessions: %S" enkan-repl-session-list)
          
          ;; Verify correct state loaded
          (should (string= "01" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          (should (equal '((1 . "er")) enkan-repl-session-list))
          
          ;; Check that correct buffer can be found
          (let ((buffers (enkan-repl--get-available-buffers (buffer-list))))
            (message "Available buffers: %S" (mapcar #'buffer-name buffers))
            ;; Should only find buffer for current workspace
            (should (member buffer1 buffers))
            (should-not (member buffer2 buffers)))
          
          (message "\n=== Switching to WS 02 ===")
          ;; Switch to WS 02
          (enkan-repl--save-workspace-state) ; Save WS 01
          (setq enkan-repl--current-workspace "02")
          (enkan-repl--load-workspace-state "02")
          
          (message "After switch to WS 02:")
          (message "  Workspace: %s" enkan-repl--current-workspace)
          (message "  Project: %s" enkan-repl--current-project)
          (message "  Sessions: %S" enkan-repl-session-list)
          
          ;; Verify correct state loaded
          (should (string= "02" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          (should (equal '((1 . "er")) enkan-repl-session-list))
          
          ;; Check that correct buffer can be found
          (let ((buffers (enkan-repl--get-available-buffers (buffer-list))))
            (message "Available buffers: %S" (mapcar #'buffer-name buffers))
            ;; Should only find buffer for current workspace
            (should-not (member buffer1 buffers))
            (should (member buffer2 buffers)))
          
          (message "\n=== Final verification ===")
          ;; Both workspaces should have their sessions saved
          (let ((ws01-state (enkan-repl--get-workspace-state enkan-repl--workspaces "01"))
                (ws02-state (enkan-repl--get-workspace-state enkan-repl--workspaces "02")))
            (message "WS 01 final state: %S" ws01-state)
            (message "WS 02 final state: %S" ws02-state)
            
            ;; Both should have project "er"
            (should (string= "er" (plist-get ws01-state :current-project)))
            (should (string= "er" (plist-get ws02-state :current-project)))
            
            ;; Both should have 1 session
            (should (= 1 (length (plist-get ws01-state :session-list))))
            (should (= 1 (length (plist-get ws02-state :session-list))))))
      
      ;; Clean up
      (when (buffer-live-p buffer1)
        (with-current-buffer buffer1
          (when (and (boundp 'eat--process) eat--process)
            (delete-process eat--process)))
        (kill-buffer buffer1))
      (when (buffer-live-p buffer2)
        (with-current-buffer buffer2
          (when (and (boundp 'eat--process) eat--process)
            (delete-process eat--process)))
        (kill-buffer buffer2)))))

(provide 'enkan-repl-workspace-project-binding-test)
;;; enkan-repl-workspace-project-binding-test.el ends here
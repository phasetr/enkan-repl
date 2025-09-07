;;; enkan-repl-workspace-window-layout-test.el --- Test workspace window layout creation -*- lexical-binding: t -*-

;;; Commentary:
;; Test that workspace switching correctly finds buffers and creates window layout

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(load (expand-file-name "examples/window-layouts.el" default-directory) nil t)

(ert-deftest test-workspace-switch-finds-correct-buffers ()
  "Test that workspace switching finds correct buffers for window layout."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory default-directory)
        (buffer-ws01 nil)
        (buffer-ws02 nil))
    
    (unwind-protect
        (progn
          (message "\n=== Create WS 01 with eat buffer ===")
          ;; Create WS 01
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "01" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create eat buffer for WS 01
          (setq buffer-ws01 (generate-new-buffer "*ws:01 enkan:/path/to/er/*"))
          (with-current-buffer buffer-ws01
            (setq-local eat--process (start-process "mock-eat-1" buffer-ws01 "cat")))
          
          (message "\n=== Create WS 02 with eat buffer ===")
          ;; Create WS 02
          (enkan-repl--setup-create-workspace-with-project nil "er")
          (should (string= "02" enkan-repl--current-workspace))
          (should (string= "er" enkan-repl--current-project))
          
          ;; Create eat buffer for WS 02
          (setq buffer-ws02 (generate-new-buffer "*ws:02 enkan:/path/to/er/*"))
          (with-current-buffer buffer-ws02
            (setq-local eat--process (start-process "mock-eat-2" buffer-ws02 "cat")))
          
          (message "\n=== Switch to WS 01 ===")
          ;; Manual switch to WS 01 (simulating user's scenario)
          (setq enkan-repl--current-workspace "01")
          (enkan-repl--load-workspace-state "01")
          
          ;; Check that WS 01 can find its buffer
          (let ((available-buffers (enkan-repl--get-available-buffers (buffer-list))))
            (message "WS 01 available buffers: %S" (mapcar #'buffer-name available-buffers))
            (should (member buffer-ws01 available-buffers))
            (should-not (member buffer-ws02 available-buffers)))
          
          (message "\n=== Switch to WS 02 ===")
          ;; Manual switch to WS 02
          (setq enkan-repl--current-workspace "02")
          (enkan-repl--load-workspace-state "02")
          
          ;; Check that WS 02 can find its buffer
          (let ((available-buffers (enkan-repl--get-available-buffers (buffer-list))))
            (message "WS 02 available buffers: %S" (mapcar #'buffer-name available-buffers))
            (should-not (member buffer-ws01 available-buffers))
            (should (member buffer-ws02 available-buffers)))
          
          (message "\n=== Test enkan-repl-setup-current-project-layout behavior ===")
          ;; Switch back to WS 01
          (setq enkan-repl--current-workspace "01")
          (enkan-repl--load-workspace-state "01")
          
          ;; Mock the layout function to see what buffer it gets
          (let ((layout-called-with-buffer nil))
            (cl-letf (((symbol-function 'enkan-repl-layout-standard-with-eat)
                       (lambda (buffer)
                         (setq layout-called-with-buffer buffer)
                         (message "Layout called with buffer: %S" (when buffer (buffer-name buffer))))))
              
              ;; Call setup-current-project-layout
              (enkan-repl-setup-current-project-layout)
              
              ;; Should have been called with WS 01 buffer
              (should (eq layout-called-with-buffer buffer-ws01))))
          
          ;; Switch to WS 02 and test again
          (setq enkan-repl--current-workspace "02")
          (enkan-repl--load-workspace-state "02")
          
          (let ((layout-called-with-buffer nil))
            (cl-letf (((symbol-function 'enkan-repl-layout-standard-with-eat)
                       (lambda (buffer)
                         (setq layout-called-with-buffer buffer)
                         (message "Layout called with buffer: %S" (when buffer (buffer-name buffer))))))
              
              (enkan-repl-setup-current-project-layout)
              
              ;; Should have been called with WS 02 buffer
              (should (eq layout-called-with-buffer buffer-ws02)))))
      
      ;; Clean up
      (when (buffer-live-p buffer-ws01)
        (with-current-buffer buffer-ws01
          (when (and (boundp 'eat--process) eat--process)
            (delete-process eat--process)))
        (kill-buffer buffer-ws01))
      (when (buffer-live-p buffer-ws02)
        (with-current-buffer buffer-ws02
          (when (and (boundp 'eat--process) eat--process)
            (delete-process eat--process)))
        (kill-buffer buffer-ws02)))))

(provide 'enkan-repl-workspace-window-layout-test)
;;; enkan-repl-workspace-window-layout-test.el ends here
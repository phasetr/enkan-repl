;;; enkan-repl-workspace-buffer-count-test.el --- Test workspace buffer count calculation -*- lexical-binding: t -*-

;;; Commentary:
;; Test pure function to calculate actual living buffer count for workspace

;;; Code:

(require 'ert)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-enkan-repl--get-workspace-buffer-count-pure ()
  "Test pure function to get living buffer count for workspace."
  ;; Test with mock workspace state and buffer list
  (let ((buffer1 (generate-new-buffer "*ws:01 enkan:/path/to/er/*"))
        (buffer2 (generate-new-buffer "*ws:02 enkan:/path/to/er/*"))
        (buffer3 (generate-new-buffer "*ws:01 enkan:/path/to/other/*")))
    
    (unwind-protect
        (progn
          ;; Set up mock eat processes
          (with-current-buffer buffer1
            (setq-local eat--process (start-process "mock-1" buffer1 "cat")))
          (with-current-buffer buffer2
            (setq-local eat--process (start-process "mock-2" buffer2 "cat")))
          (with-current-buffer buffer3
            (setq-local eat--process (start-process "mock-3" buffer3 "cat")))
          
          (let ((all-buffers (list buffer1 buffer2 buffer3)))
            ;; Should return 2 for workspace "01" (buffer1 and buffer3)
            (should (= 2 (enkan-repl--get-workspace-buffer-count-pure all-buffers "01")))
            ;; Should return 1 for workspace "02" (buffer2)
            (should (= 1 (enkan-repl--get-workspace-buffer-count-pure all-buffers "02")))
            ;; Should return 0 for workspace "99" (no matching buffers)
            (should (= 0 (enkan-repl--get-workspace-buffer-count-pure all-buffers "99")))))
      
      ;; Clean up
      (dolist (buffer (list buffer1 buffer2 buffer3))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (boundp 'eat--process) eat--process)
              (delete-process eat--process)))
          (kill-buffer buffer))))))

(provide 'enkan-repl-workspace-buffer-count-test)
;;; enkan-repl-workspace-buffer-count-test.el ends here
;;; enkan-repl-window-management-test.el --- Tests for window management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for window management in enkan-repl-start-eat

;;; Code:

(require 'ert)

;; Load the main package
(let ((package-dir (or (and load-file-name
                            (file-name-directory load-file-name))
                       default-directory)))
  (load (expand-file-name "../enkan-repl.el" package-dir)))

(ert-deftest test-start-eat-single-window ()
  "Test that eat buffer is displayed on the right when starting from single window."
  (let ((original-window-config (current-window-configuration))
        (test-buffer (generate-new-buffer "*test-input*"))
        (mock-eat-buffer nil)
        (windows-after-start nil))
    (cl-letf (((symbol-function 'enkan-repl--get-session-info)
               (lambda () (list "/test/dir/" nil nil)))
              ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir/"))
              ((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (dir) nil))
              ((symbol-function 'enkan-repl--can-send-text)
               (lambda (dir) 
                 (when mock-eat-buffer
                   (buffer-live-p mock-eat-buffer))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror) t))
              ((symbol-function 'fboundp)
               (lambda (func) t))
              ((symbol-function 'eat)
               (lambda ()
                 (if mock-eat-buffer
                     mock-eat-buffer
                   (setq mock-eat-buffer (generate-new-buffer "*eat-mock*"))
                   (with-current-buffer mock-eat-buffer
                     (setq-local eat-mode t))
                   mock-eat-buffer)))
              ((symbol-function 'rename-buffer)
               (lambda (name &optional unique)
                 ;; Do nothing - buffer is already named
                 nil)))
      (unwind-protect
          (progn
            ;; Start from single window
            (delete-other-windows)
            (switch-to-buffer test-buffer)
            (enkan-repl-start-eat)
            
            ;; Check window configuration
            (setq windows-after-start (length (window-list)))
            (should (= 2 windows-after-start))
            
            ;; Check that focus moved to eat buffer
            (should (buffer-live-p mock-eat-buffer))
            (should (eq (window-buffer (selected-window)) mock-eat-buffer))
            
            ;; Check that input buffer is in left window
            (other-window 1)
            (should (eq (window-buffer (selected-window)) test-buffer)))
        ;; Cleanup
        (when (and test-buffer (buffer-live-p test-buffer))
          (kill-buffer test-buffer))
        (when (and mock-eat-buffer (buffer-live-p mock-eat-buffer))
          (kill-buffer mock-eat-buffer))
        (set-window-configuration original-window-config)))))

(ert-deftest test-start-eat-two-windows ()
  "Test that eat buffer uses right window when starting from two windows."
  (let ((original-window-config (current-window-configuration))
        (test-buffer-left (generate-new-buffer "*test-input*"))
        (test-buffer-right (generate-new-buffer "*test-other*"))
        (mock-eat-buffer nil)
        (windows-after-start nil))
    (cl-letf (((symbol-function 'enkan-repl--get-session-info)
               (lambda () (list "/test/dir/" nil nil)))
              ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir/"))
              ((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (dir) nil))
              ((symbol-function 'enkan-repl--can-send-text)
               (lambda (dir) 
                 (when mock-eat-buffer
                   (buffer-live-p mock-eat-buffer))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror) t))
              ((symbol-function 'fboundp)
               (lambda (func) t))
              ((symbol-function 'eat)
               (lambda ()
                 (if mock-eat-buffer
                     mock-eat-buffer
                   (setq mock-eat-buffer (generate-new-buffer "*eat-mock*"))
                   (with-current-buffer mock-eat-buffer
                     (setq-local eat-mode t))
                   mock-eat-buffer)))
              ((symbol-function 'rename-buffer)
               (lambda (name &optional unique)
                 ;; Do nothing - buffer is already named
                 nil)))
      (unwind-protect
          (progn
            ;; Setup two windows
            (delete-other-windows)
            (switch-to-buffer test-buffer-left)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer test-buffer-right)
            (other-window 1) ; Back to left window
            
            ;; Start eat from left window
            (enkan-repl-start-eat)
            
            ;; Check window configuration - should still be 2 windows
            (setq windows-after-start (length (window-list)))
            (should (= 2 windows-after-start))
            
            ;; Check that focus moved to eat buffer (right window)
            (should (buffer-live-p mock-eat-buffer))
            (should (eq (window-buffer (selected-window)) mock-eat-buffer))
            
            ;; Check that input buffer is still in left window
            (other-window 1)
            (should (eq (window-buffer (selected-window)) test-buffer-left))
            
            ;; Check that test-buffer-right is no longer displayed
            (let ((displayed nil))
              (walk-windows (lambda (w)
                             (when (eq (window-buffer w) test-buffer-right)
                               (setq displayed t))))
              (should-not displayed)))
        ;; Cleanup
        (when (and test-buffer-left (buffer-live-p test-buffer-left))
          (kill-buffer test-buffer-left))
        (when (and test-buffer-right (buffer-live-p test-buffer-right))
          (kill-buffer test-buffer-right))
        (when (and mock-eat-buffer (buffer-live-p mock-eat-buffer))
          (kill-buffer mock-eat-buffer))
        (set-window-configuration original-window-config)))))

(ert-deftest test-start-eat-three-windows ()
  "Test that eat buffer splits original window when there are 3+ windows."
  (let ((original-window-config (current-window-configuration))
        (test-buffer-1 (generate-new-buffer "*test-input*"))
        (test-buffer-2 (generate-new-buffer "*test-other-1*"))
        (test-buffer-3 (generate-new-buffer "*test-other-2*"))
        (mock-eat-buffer nil)
        (windows-after-start nil))
    (cl-letf (((symbol-function 'enkan-repl--get-session-info)
               (lambda () (list "/test/dir/" nil nil)))
              ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir/"))
              ((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (dir) nil))
              ((symbol-function 'enkan-repl--can-send-text)
               (lambda (dir) 
                 (when mock-eat-buffer
                   (buffer-live-p mock-eat-buffer))))
              ((symbol-function 'require)
               (lambda (feature &optional filename noerror) t))
              ((symbol-function 'fboundp)
               (lambda (func) t))
              ((symbol-function 'eat)
               (lambda ()
                 (if mock-eat-buffer
                     mock-eat-buffer
                   (setq mock-eat-buffer (generate-new-buffer "*eat-mock*"))
                   (with-current-buffer mock-eat-buffer
                     (setq-local eat-mode t))
                   mock-eat-buffer)))
              ((symbol-function 'rename-buffer)
               (lambda (name &optional unique)
                 ;; Do nothing - buffer is already named
                 nil)))
      (unwind-protect
          (progn
            ;; Setup three windows
            (delete-other-windows)
            (switch-to-buffer test-buffer-1)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer test-buffer-2)
            (split-window-below)
            (other-window 1)
            (switch-to-buffer test-buffer-3)
            (select-window (get-buffer-window test-buffer-1)) ; Back to first window
            
            ;; Start eat from first window
            (enkan-repl-start-eat)
            
            ;; Check window configuration - should be 4 windows
            (setq windows-after-start (length (window-list)))
            (should (= 4 windows-after-start))
            
            ;; Check that focus moved to eat buffer
            (should (buffer-live-p mock-eat-buffer))
            (should (eq (window-buffer (selected-window)) mock-eat-buffer)))
        ;; Cleanup
        (when (and test-buffer-1 (buffer-live-p test-buffer-1))
          (kill-buffer test-buffer-1))
        (when (and test-buffer-2 (buffer-live-p test-buffer-2))
          (kill-buffer test-buffer-2))
        (when (and test-buffer-3 (buffer-live-p test-buffer-3))
          (kill-buffer test-buffer-3))
        (when (and mock-eat-buffer (buffer-live-p mock-eat-buffer))
          (kill-buffer mock-eat-buffer))
        (set-window-configuration original-window-config)))))

(provide 'enkan-repl-window-management-test)
;;; enkan-repl-window-management-test.el ends here
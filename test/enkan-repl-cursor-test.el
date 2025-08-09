;;; enkan-repl-cursor-test.el --- Tests for cursor position after sending text -*- lexical-binding: t -*-

;;; Commentary:
;; Tests to ensure eat buffer cursor stays at bottom after sending text

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'eat nil t)

(ert-deftest test-enkan-repl-cursor-stays-at-bottom-after-send ()
  "Test that cursor stays at bottom of eat buffer after sending text."
  (skip-unless (featurep 'eat))
  (let ((test-dir "/tmp/test-enkan-repl/"))
    (unwind-protect
        (progn
          ;; Create test directory
          (make-directory test-dir t)
          (let ((default-directory test-dir))
            ;; Start eat session
            (enkan-repl-start-eat)
            ;; Get the session buffer
            (let ((session-buffer (enkan-repl--get-buffer-for-directory test-dir)))
              (should session-buffer)
              ;; Send some text
              (enkan-repl--send-text "echo test" test-dir)
              ;; Wait for eat to process
              (sit-for 0.1)
              ;; Check cursor position
              (with-current-buffer session-buffer
                (should (= (point) (point-max))))
              ;; Send more text
              (enkan-repl--send-text "echo another test" test-dir)
              ;; Wait for eat to process
              (sit-for 0.1)
              ;; Check cursor position again
              (with-current-buffer session-buffer
                (should (= (point) (point-max))))
              ;; Clean up
              (kill-buffer session-buffer))))
      ;; Clean up test directory
      (delete-directory test-dir t))))

(ert-deftest test-enkan-repl-recenter-bottom-moves-cursor ()
  "Test that enkan-repl-recenter-bottom moves cursor to bottom."
  (skip-unless (featurep 'eat))
  (let ((test-dir "/tmp/test-enkan-repl2/"))
    (unwind-protect
        (progn
          ;; Create test directory
          (make-directory test-dir t)
          (let ((default-directory test-dir))
            ;; Start eat session
            (enkan-repl-start-eat)
            ;; Get the session buffer
            (let ((session-buffer (enkan-repl--get-buffer-for-directory test-dir)))
              (should session-buffer)
              ;; Move cursor to beginning
              (with-current-buffer session-buffer
                (goto-char (point-min)))
              ;; Call recenter-bottom
              (enkan-repl-recenter-bottom)
              ;; Check cursor position
              (with-current-buffer session-buffer
                (should (= (point) (point-max))))
              ;; Clean up
              (kill-buffer session-buffer))))
      ;; Clean up test directory
      (delete-directory test-dir t))))

(provide 'enkan-repl-cursor-test)
;;; enkan-repl-cursor-test.el ends here
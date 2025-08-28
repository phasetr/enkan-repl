;;; center-send-region-multiline-test.el --- Tests for multiline text sending with unified backend -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Tests for multiline text sending with the new unified backend.
;; Updated to work with enkan-repl--center-send-unified.

;;; Code:

(require 'ert)

;; Load the main package
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el"
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

;;;; Tests for multiline text sending (legacy compatibility)

(ert-deftest test-center-send-text-to-buffer-multiline ()
  "Test enkan-repl--center-send-text-to-buffer with multiline text."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (multiline-text "echo hello\necho world\necho test")
        (sent-strings '())
        (process-obj nil))

    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))

    ;; Mock eat--send-string to capture what was sent
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (process string)
                 (push string sent-strings)))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'run-at-time) (lambda (&rest args) nil)))

      ;; Test the function
      (should (enkan-repl--center-send-text-to-buffer multiline-text test-buffer))

      ;; Verify what was sent: should be text + carriage return
      (should (= 2 (length sent-strings)))
      (should (equal "\r" (nth 0 sent-strings)))  ; Last sent (LIFO order)
      (should (equal multiline-text (nth 1 sent-strings))))  ; First sent

    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-text-to-buffer-with-newlines-only ()
  "Test enkan-repl--center-send-text-to-buffer with text containing only newlines."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (newline-text "\n\n\n")
        (sent-strings '())
        (process-obj nil))

    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))

    ;; Mock eat--send-string to capture what was sent
    (cl-letf (((symbol-function 'eat--send-string)
               (lambda (process string)
                 (push string sent-strings)))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'run-at-time) (lambda (&rest args) nil)))

      ;; Test the function
      (should (enkan-repl--center-send-text-to-buffer newline-text test-buffer))

      ;; Verify what was sent: should be text + carriage return
      (should (= 2 (length sent-strings)))
      (should (equal "\r" (nth 0 sent-strings)))  ; Last sent (LIFO order)
      (should (equal newline-text (nth 1 sent-strings))))  ; First sent

    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

;;;; Tests for unified backend multiline text sending

(ert-deftest test-center-send-region-multiline-with-alias-unified ()
  "Test enkan-repl-center-send-region with multiline text and alias using unified backend."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (multiline-text "echo hello\necho world\necho test")
        (sent-strings '())
        (process-obj nil))

    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))

    ;; Mock functions for unified backend
    (cl-letf (((symbol-function 'buffer-list)
               (lambda () (list test-buffer)))
              ((symbol-function 'eat--send-string)
               (lambda (process string)
                 (push string sent-strings)))
              ((symbol-function 'process-live-p) (lambda (proc) t))
              ((symbol-function 'enkan-repl--resolve-target-buffer-pure)
               (lambda (prefix-arg alias buffers) test-buffer)))

      (with-temp-buffer
        (insert ":test " multiline-text)
        (let ((start (point-min))
              (end (point-max)))

          (let ((enkan-repl-project-aliases '(("test" . "test"))))
            ;; Test the function
            (enkan-repl-center-send-region start end)

            ;; Verify what was sent: text + carriage return
            (should (= 2 (length sent-strings)))
            (should (equal "\r" (nth 0 sent-strings)))  ; Last sent (LIFO order)
            (should (equal multiline-text (nth 1 sent-strings)))))))

    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(ert-deftest test-center-send-region-multiline-interactive-selection-unified ()
  "Test enkan-repl-center-send-region with multiline text using interactive selection and unified backend."
  (let ((test-buffer (generate-new-buffer "*enkan:test*"))
        (multiline-text "function test() {\n  console.log('hello');\n  return 42;\n}")
        (sent-strings '())
        (process-obj nil))

    (with-current-buffer test-buffer
      ;; Set up mock process
      (setq-local eat--process (start-process "test-process" nil "echo" "test"))
      (setq process-obj eat--process))

    ;; Mock functions for unified backend
    (cl-letf (((symbol-function 'buffer-list)
               (lambda () (list test-buffer)))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &rest args)
                 (car choices)))  ; Select first choice
              ((symbol-function 'eat--send-string)
               (lambda (process string)
                 (push string sent-strings)))
              ((symbol-function 'process-live-p) (lambda (proc) t)))

      (with-temp-buffer
        (insert multiline-text)
        (let ((start (point-min))
              (end (point-max)))

          ;; Test the function
          (enkan-repl-center-send-region start end)

          ;; Verify what was sent: text + carriage return
          (should (= 2 (length sent-strings)))
          (should (equal "\r" (nth 0 sent-strings)))  ; Last sent (LIFO order)
          (should (equal multiline-text (nth 1 sent-strings))))))

    ;; Clean up
    (when (and process-obj (process-live-p process-obj))
      (delete-process process-obj))
    (kill-buffer test-buffer)))

(provide 'center-send-region-multiline-test)
;;; center-send-region-multiline-test.el ends here

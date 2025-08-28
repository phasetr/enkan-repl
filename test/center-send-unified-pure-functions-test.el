;;; center-send-unified-pure-functions-test.el --- Tests for center-send unified pure functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unit tests for center-send integration pure functions.
;; Tests the Phase 1 pure function layer with 100% coverage.

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

;;;; Tests for enkan-repl--get-available-buffers-pure

(ert-deftest test-get-available-buffers-pure-empty-list ()
  "Test enkan-repl--get-available-buffers-pure with empty buffer list."
  (should (equal '() (enkan-repl--get-available-buffers-pure '()))))

(ert-deftest test-get-available-buffers-pure-no-enkan-buffers ()
  "Test enkan-repl--get-available-buffers-pure with non-enkan buffers."
  (let ((test-buffer (generate-new-buffer "regular-buffer")))
    (unwind-protect
        (should (equal '() (enkan-repl--get-available-buffers-pure (list test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-get-available-buffers-pure-with-valid-enkan-buffer ()
  "Test enkan-repl--get-available-buffers-pure with valid enkan buffer."
  (let ((test-buffer (generate-new-buffer "*enkan:/test/path*"))
        (process-obj nil))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'process-live-p) (lambda (proc) t)))
            (let ((result (enkan-repl--get-available-buffers-pure (list test-buffer))))
              (should (= 1 (length result)))
              (should (eq test-buffer (car result))))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-get-available-buffers-pure-filters-dead-processes ()
  "Test enkan-repl--get-available-buffers-pure filters out dead processes."
  (let ((test-buffer (generate-new-buffer "*enkan:/test/path*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process nil))  ; Dead process
          (should (equal '() (enkan-repl--get-available-buffers-pure (list test-buffer)))))
      (kill-buffer test-buffer))))

;;;; Tests for enkan-repl--resolve-target-buffer-pure

(ert-deftest test-resolve-target-buffer-pure-prefix-arg-priority ()
  "Test enkan-repl--resolve-target-buffer-pure prefix-arg takes priority."
  (let ((buffer1 (generate-new-buffer "*enkan:/test1*"))
        (buffer2 (generate-new-buffer "*enkan:/test2*"))
        (enkan-repl-project-aliases '(("test" . "alias-project"))))
    (unwind-protect
        (let ((buffers (list buffer1 buffer2)))
          ;; prefix-arg should override alias
          (should (eq buffer2 (enkan-repl--resolve-target-buffer-pure 2 "test" buffers)))
          (should (eq buffer1 (enkan-repl--resolve-target-buffer-pure 1 "test" buffers))))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-resolve-target-buffer-pure-alias-fallback ()
  "Test enkan-repl--resolve-target-buffer-pure falls back to alias when no prefix-arg."
  (let ((test-buffer (generate-new-buffer "*enkan:/test/project*"))
        (enkan-repl-project-aliases '(("test" . "project"))))
    (unwind-protect
        (let ((buffers (list test-buffer)))
          (should (eq test-buffer (enkan-repl--resolve-target-buffer-pure nil "test" buffers))))
      (kill-buffer test-buffer))))

(ert-deftest test-resolve-target-buffer-pure-interactive-fallback ()
  "Test enkan-repl--resolve-target-buffer-pure returns nil for interactive selection."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*")))
    (unwind-protect
        (let ((buffers (list test-buffer)))
          (should (null (enkan-repl--resolve-target-buffer-pure nil nil buffers)))
          (should (null (enkan-repl--resolve-target-buffer-pure nil "" buffers))))
      (kill-buffer test-buffer))))

(ert-deftest test-resolve-target-buffer-pure-invalid-prefix-arg ()
  "Test enkan-repl--resolve-target-buffer-pure handles invalid prefix-arg."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*")))
    (unwind-protect
        (let ((buffers (list test-buffer)))
          (should (null (enkan-repl--resolve-target-buffer-pure 0 nil buffers)))
          (should (null (enkan-repl--resolve-target-buffer-pure -1 nil buffers)))
          (should (null (enkan-repl--resolve-target-buffer-pure 99 nil buffers))))
      (kill-buffer test-buffer))))

;;;; Tests for enkan-repl--send-primitive-pure

(ert-deftest test-send-primitive-pure-text-content ()
  "Test enkan-repl--send-primitive-pure with text content."
  (let ((result (enkan-repl--send-primitive-pure "hello world" nil)))
    (should (string= "hello world" (plist-get result :content)))
    (should (eq 'text (plist-get result :action)))))

(ert-deftest test-send-primitive-pure-enter-key ()
  "Test enkan-repl--send-primitive-pure with enter key."
  (let ((result (enkan-repl--send-primitive-pure "ignored" :enter)))
    (should (string= "\r" (plist-get result :content)))
    (should (eq 'key (plist-get result :action)))))

(ert-deftest test-send-primitive-pure-escape-key ()
  "Test enkan-repl--send-primitive-pure with escape key."
  (let ((result (enkan-repl--send-primitive-pure "ignored" :escape)))
    (should (string= "\e" (plist-get result :content)))
    (should (eq 'key (plist-get result :action)))))

(ert-deftest test-send-primitive-pure-number-keys ()
  "Test enkan-repl--send-primitive-pure with number keys 1-9."
  (dolist (num '(1 2 3 4 5 6 7 8 9))
    (let ((result (enkan-repl--send-primitive-pure "ignored" num)))
      (should (string= (number-to-string num) (plist-get result :content)))
      (should (eq 'number (plist-get result :action))))))

(ert-deftest test-send-primitive-pure-invalid-special-key ()
  "Test enkan-repl--send-primitive-pure with invalid special key."
  (should-error (enkan-repl--send-primitive-pure "text" :invalid))
  (should-error (enkan-repl--send-primitive-pure "text" 0))
  (should-error (enkan-repl--send-primitive-pure "text" 10)))

;;;; Tests for enkan-repl--send-primitive-action

(ert-deftest test-send-primitive-action-success ()
  "Test enkan-repl--send-primitive-action successful send."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'process-live-p) (lambda (proc) t)))
            (let ((send-data (list :content "test content" :action 'text)))
              (should (enkan-repl--send-primitive-action test-buffer send-data))
              (should (= 2 (length sent-strings)))
              (should (equal "test content" (nth 1 sent-strings)))  ; LIFO order
              (should (equal "\r" (nth 0 sent-strings))))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-send-primitive-action-key-no-carriage-return ()
  "Test enkan-repl--send-primitive-action for keys doesn't add carriage return."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'process-live-p) (lambda (proc) t)))
            (let ((send-data (list :content "\e" :action 'key)))
              (should (enkan-repl--send-primitive-action test-buffer send-data))
              (should (= 1 (length sent-strings)))
              (should (equal "\e" (car sent-strings))))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-send-primitive-action-dead-buffer ()
  "Test enkan-repl--send-primitive-action with dead buffer."
  (let ((send-data (list :content "test" :action 'text)))
    (should (null (enkan-repl--send-primitive-action nil send-data)))))

(ert-deftest test-send-primitive-action-no-process ()
  "Test enkan-repl--send-primitive-action with buffer but no process."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process nil))
          (let ((send-data (list :content "test" :action 'text)))
            (should (null (enkan-repl--send-primitive-action test-buffer send-data)))))
      (kill-buffer test-buffer))))

(provide 'center-send-unified-pure-functions-test)
;;; center-send-unified-pure-functions-test.el ends here
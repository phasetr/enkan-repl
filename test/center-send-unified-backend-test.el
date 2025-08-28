;;; center-send-unified-backend-test.el --- Tests for center-send unified backend -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Integration tests for center-send unified backend.
;; Tests the Phase 2 unified backend implementation.

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

;;;; Tests for enkan-repl--center-send-unified

(ert-deftest test-center-send-unified-no-buffers ()
  "Test enkan-repl--center-send-unified with no active buffers."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'buffer-list) (lambda () '()))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq message-shown (apply #'format format-string args))))
            ((symbol-function 'completing-read)
               (lambda (prompt choices &rest args) 
                 (error "completing-read should not be called with no buffers"))))
      (should (null (enkan-repl--center-send-unified "test content")))
      (should (string-match-p "No active enkan sessions found" message-shown)))))

(ert-deftest test-center-send-unified-simple-text ()
  "Test enkan-repl--center-send-unified with simple text."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '())
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-shown (apply #'format format-string args)))))
            (should (enkan-repl--center-send-unified "hello world"))
            (should (= 2 (length sent-strings)))
            (should (equal "hello world" (nth 1 sent-strings)))
            (should (equal "\r" (nth 0 sent-strings)))
            (should (string-match-p "Sent to buffer" message-shown))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-special-key-escape ()
  "Test enkan-repl--center-send-unified with escape special key."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil)))
            (should (enkan-repl--center-send-unified "ignored" nil :escape))
            (should (= 1 (length sent-strings)))
            (should (equal "\e" (car sent-strings)))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-special-key-enter ()
  "Test enkan-repl--center-send-unified with enter special key."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil)))
            (should (enkan-repl--center-send-unified "ignored" nil :enter))
            (should (= 1 (length sent-strings)))
            (should (equal "\r" (car sent-strings)))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-special-key-number ()
  "Test enkan-repl--center-send-unified with number special key."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*"))
        (process-obj nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil)))
            (should (enkan-repl--center-send-unified "ignored" nil 5))
            (should (= 1 (length sent-strings)))
            (should (equal "5" (car sent-strings)))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-alias-parsing-escape ()
  "Test enkan-repl--center-send-unified with alias escape command."
  (let ((test-buffer (generate-new-buffer "*enkan:/test/project*"))
        (process-obj nil)
        (sent-strings '())
        (enkan-repl-project-aliases '(("test" . "project"))))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil))
                    ((symbol-function 'enkan-repl--resolve-target-buffer-pure)
                     (lambda (prefix-arg alias buffers) test-buffer)))
            (should (enkan-repl--center-send-unified ":test esc"))
            (should (= 1 (length sent-strings)))
            (should (equal "\e" (car sent-strings)))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-alias-parsing-text ()
  "Test enkan-repl--center-send-unified with alias text command."
  (let ((test-buffer (generate-new-buffer "*enkan:/test/project*"))
        (process-obj nil)
        (sent-strings '())
        (enkan-repl-project-aliases '(("test" . "project"))))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (setq-local eat--process (start-process "test" nil "echo" "test"))
            (setq process-obj eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list test-buffer)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push string sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil))
                    ((symbol-function 'enkan-repl--resolve-target-buffer-pure)
                     (lambda (prefix-arg alias buffers) test-buffer)))
            (should (enkan-repl--center-send-unified ":test hello world"))
            (should (= 2 (length sent-strings)))
            (should (equal "hello world" (nth 1 sent-strings)))
            (should (equal "\r" (nth 0 sent-strings)))))
      (when (and process-obj (process-live-p process-obj))
        (delete-process process-obj))
      (kill-buffer test-buffer))))

(ert-deftest test-center-send-unified-prefix-arg-priority ()
  "Test enkan-repl--center-send-unified prefix-arg takes priority over alias."
  (let ((buffer1 (generate-new-buffer "*enkan:/test1*"))
        (buffer2 (generate-new-buffer "*enkan:/test2*"))
        (process-obj1 nil)
        (process-obj2 nil)
        (sent-strings '())
        (enkan-repl-project-aliases '(("test" . "project"))))
    (unwind-protect
        (progn
          (with-current-buffer buffer1
            (setq-local eat--process (start-process "test1" nil "echo" "test"))
            (setq process-obj1 eat--process))
          (with-current-buffer buffer2
            (setq-local eat--process (start-process "test2" nil "echo" "test"))
            (setq process-obj2 eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buffer1 buffer2)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push (cons (buffer-name (current-buffer)) string) sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil)))
            ;; prefix-arg 2 should select buffer2 regardless of alias
            (should (enkan-repl--center-send-unified ":test content" 2))
            (should (= 2 (length sent-strings)))
            (should (string= "*enkan:/test2*" (car (nth 1 sent-strings))))
            (should (equal "content" (cdr (nth 1 sent-strings))))))
      (when (and process-obj1 (process-live-p process-obj1))
        (delete-process process-obj1))
      (when (and process-obj2 (process-live-p process-obj2))
        (delete-process process-obj2))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-center-send-unified-interactive-selection ()
  "Test enkan-repl--center-send-unified with interactive buffer selection."
  (let ((buffer1 (generate-new-buffer "*enkan:/test1*"))
        (buffer2 (generate-new-buffer "*enkan:/test2*"))
        (process-obj1 nil)
        (process-obj2 nil)
        (sent-strings '()))
    (unwind-protect
        (progn
          (with-current-buffer buffer1
            (setq-local eat--process (start-process "test1" nil "echo" "test"))
            (setq process-obj1 eat--process))
          (with-current-buffer buffer2
            (setq-local eat--process (start-process "test2" nil "echo" "test"))
            (setq process-obj2 eat--process))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buffer1 buffer2)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (process string)
                       (push (cons (buffer-name (current-buffer)) string) sent-strings)))
                    ((symbol-function 'message) (lambda (&rest args) nil))
                    ((symbol-function 'completing-read)
                     (lambda (prompt choices &rest args)
                       ;; Select second buffer
                       (car (nth 1 choices)))))
            (should (enkan-repl--center-send-unified "test content"))
            (should (= 2 (length sent-strings)))
            (should (string= "*enkan:/test2*" (car (nth 1 sent-strings))))
            (should (equal "test content" (cdr (nth 1 sent-strings))))))
      (when (and process-obj1 (process-live-p process-obj1))
        (delete-process process-obj1))
      (when (and process-obj2 (process-live-p process-obj2))
        (delete-process process-obj2))
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(provide 'center-send-unified-backend-test)
;;; center-send-unified-backend-test.el ends here
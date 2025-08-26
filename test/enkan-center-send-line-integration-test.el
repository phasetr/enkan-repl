;;; enkan-center-send-line-integration-test.el --- Integration tests for center-send-line functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Integration tests for enkan-repl-center-send-line function with real buffer scenarios.

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

;;;; Integration tests for enkan-repl-center-send-line

(ert-deftest test-center-send-line-with-er-esc ()
  "Test center-send-line with :er esc command - should find enkan-repl buffer."
  (let ((test-buffer-er (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (test-buffer-pt (generate-new-buffer "*enkan:/Users/sekine/pt-tools*"))
        (enkan-repl-project-aliases '(("pt" . "pt-tools") ("er" . "enkan-repl")))
        (escape-sent nil)
        (actual-buffer nil)
        (process-obj-er nil)
        (process-obj-pt nil))
    
    ;; Setup mock processes for both buffers
    (with-current-buffer test-buffer-er
      (setq-local eat--process (start-process "test-process-er" nil "echo" "test"))
      (setq process-obj-er eat--process))
    (with-current-buffer test-buffer-pt
      (setq-local eat--process (start-process "test-process-pt" nil "echo" "test"))
      (setq process-obj-pt eat--process))
    
    (with-temp-buffer
      (insert ":er esc")
      ;; Mock functions
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list test-buffer-er test-buffer-pt)))
                ((symbol-function 'eat--send-string)
                 (lambda (process string)
                   (when (string= string "\e")
                     (setq escape-sent t)
                     (setq actual-buffer (current-buffer)))))
                ((symbol-function 'process-live-p) 
                 (lambda (proc) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) nil)))
        
        ;; Test the function
        (enkan-repl-center-send-line)
        ;; Should send ESC to enkan-repl buffer, not pt-tools buffer
        (should escape-sent)
        (should (eq actual-buffer test-buffer-er))
        (should-not (eq actual-buffer test-buffer-pt))))
    
    ;; Clean up
    (when (and process-obj-er (process-live-p process-obj-er))
      (delete-process process-obj-er))
    (when (and process-obj-pt (process-live-p process-obj-pt))
      (delete-process process-obj-pt))
    (kill-buffer test-buffer-er)
    (kill-buffer test-buffer-pt)))

(ert-deftest test-center-send-line-no-project-aliases ()
  "Test center-send-line with :er esc when no project aliases are set."
  (let ((test-buffer-er (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (test-buffer-pt (generate-new-buffer "*enkan:/Users/sekine/pt-tools*"))
        (enkan-repl-project-aliases nil)  ; No aliases set
        (message-shown nil)
        (process-obj-er nil)
        (process-obj-pt nil))
    
    ;; Setup mock processes
    (with-current-buffer test-buffer-er
      (setq-local eat--process (start-process "test-process-er" nil "echo" "test"))
      (setq process-obj-er eat--process))
    (with-current-buffer test-buffer-pt
      (setq-local eat--process (start-process "test-process-pt" nil "echo" "test"))
      (setq process-obj-pt eat--process))
    
    (with-temp-buffer
      (insert ":er esc")
      ;; Mock functions
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list test-buffer-er test-buffer-pt)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-shown (apply #'format format-string args))))
                ((symbol-function 'process-live-p) 
                 (lambda (proc) t))
                ((symbol-function 'eat--send-string)
                 (lambda (process string)
                   ;; Mock should not be called when no aliases are set
                   (error "eat--send-string should not be called without valid aliases"))))
        
        ;; Test the function - should fail to find buffer
        (enkan-repl-center-send-line)
        (should (string-match-p "Invalid action format" message-shown))))
    
    ;; Clean up
    (when (and process-obj-er (process-live-p process-obj-er))
      (delete-process process-obj-er))
    (when (and process-obj-pt (process-live-p process-obj-pt))
      (delete-process process-obj-pt))
    (kill-buffer test-buffer-er)
    (kill-buffer test-buffer-pt)))

(ert-deftest test-center-send-line-buffer-matching ()
  "Test that :er resolves to correct buffer based on project aliases."
  (let ((test-buffer-er (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (test-buffer-pt (generate-new-buffer "*enkan:/Users/sekine/pt-tools*"))
        (enkan-repl-project-aliases '(("er" . "enkan-repl") ("pt" . "pt-tools")))
        (matched-buffer nil)
        (process-obj-er nil)
        (process-obj-pt nil))
    
    ;; Setup mock processes
    (with-current-buffer test-buffer-er
      (setq-local eat--process (start-process "test-process-er" nil "echo" "test"))
      (setq process-obj-er eat--process))
    (with-current-buffer test-buffer-pt
      (setq-local eat--process (start-process "test-process-pt" nil "echo" "test"))
      (setq process-obj-pt eat--process))
    
    ;; Test buffer resolution directly
    (cl-letf (((symbol-function 'process-live-p) (lambda (proc) t)))
      (let* ((enkan-buffers (list test-buffer-er test-buffer-pt))
             (resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure "er" enkan-buffers)))
        (should (eq resolved-buffer test-buffer-er))
        (should-not (eq resolved-buffer test-buffer-pt))))
    
    ;; Clean up
    (when (and process-obj-er (process-live-p process-obj-er))
      (delete-process process-obj-er))
    (when (and process-obj-pt (process-live-p process-obj-pt))
      (delete-process process-obj-pt))
    (kill-buffer test-buffer-er)
    (kill-buffer test-buffer-pt)))

(provide 'enkan-center-send-line-integration-test)
;;; enkan-center-send-line-integration-test.el ends here
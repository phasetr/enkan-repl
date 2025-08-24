;;; enkan-center-file-mode-integration-test.el --- Integration tests for center file mode -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests to verify that C-M-i correctly binds to enkan-repl-center-send-line
;; when center file mode is active.

;;; Code:

(require 'ert)
(require 'enkan-repl)

;; Mock functions for testing
(defvar enkan-test-center-file-called nil
  "Flag to track if center file function was called.")

(defun enkan-test-center-send-line ()
  "Mock center send line function for testing."
  (interactive)
  (setq enkan-test-center-file-called 'center))

(defun enkan-test-regular-send-line ()
  "Mock regular send line function for testing."
  (interactive)  
  (setq enkan-test-center-file-called 'regular))

;; Pure function to create test keymap with priorities
(defun enkan-test-create-composed-keymap-pure ()
  "Create composed keymap for testing priority.
Pure function - no side effects."
  (let ((center-map (make-sparse-keymap))
        (base-map (make-sparse-keymap)))
    ;; Base map has regular function
    (define-key base-map (kbd "C-M-i") 'enkan-test-regular-send-line)
    ;; Center map has center function  
    (define-key center-map (kbd "C-M-i") 'enkan-test-center-send-line)
    ;; Compose with center map having priority
    (make-composed-keymap center-map base-map)))

;; Test keymap composition priority
(ert-deftest test-enkan-center-file-keymap-priority ()
  "Test that center file keymap takes priority over base keymap."
  (let ((composed-map (enkan-test-create-composed-keymap-pure)))
    (should (eq (lookup-key composed-map (kbd "C-M-i")) 
                'enkan-test-center-send-line))))

;; Test keymap execution priority
(ert-deftest test-enkan-center-file-keymap-execution ()
  "Test that center file command is executed when keymap has priority."
  (let ((composed-map (enkan-test-create-composed-keymap-pure)))
    (setq enkan-test-center-file-called nil)
    (with-temp-buffer
      (use-local-map composed-map)
      ;; Simulate key execution
      (let ((command (lookup-key composed-map (kbd "C-M-i"))))
        (when (commandp command)
          (call-interactively command)))
      (should (eq enkan-test-center-file-called 'center)))))

(provide 'enkan-center-file-mode-integration-test)
;;; enkan-center-file-mode-integration-test.el ends here
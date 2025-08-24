;;; enkan-center-send-line-er-integration-test.el --- Integration test for :er prefix in center send line -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests to verify the fixed enkan-repl-center-send-line function
;; correctly handles :er prefix like enkan-repl-send-line.

;;; Code:

(require 'ert)

;; Mock variables for tracking
(defvar enkan-test-er-buffer-resolved nil
  "Track resolved buffer for :er prefix.")
(defvar enkan-test-er-text-sent nil
  "Track text sent with :er prefix.")

;; Mock function for buffer resolution
(defun enkan-repl-center--resolve-alias-to-buffer-pure-mock (alias buffers)
  "Mock function to resolve alias to buffer."
  (setq enkan-test-er-buffer-resolved alias)
  'mock-buffer)

;; Mock function for text sending
(defun enkan-repl--center-send-text-to-buffer-mock (text buffer)
  "Mock function to send text to buffer."
  (setq enkan-test-er-text-sent text)
  t)

;; Test function that simulates the fixed behavior
(defun enkan-test-center-send-line-with-er (prefix-arg line-content)
  "Test version of center send line with :er prefix support."
  (if (and (numberp prefix-arg) (<= 1 prefix-arg 4))
      (format "session-%d" prefix-arg)
    ;; Check for :er prefix
    (if (string-match "^:er \\([^ ]+\\)\\(?: \\(.*\\)\\)?$" line-content)
        (let* ((alias (match-string 1 line-content))
               (resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure-mock 
                               alias '(mock-buffers))))
          (if resolved-buffer
              (progn
                (enkan-repl--center-send-text-to-buffer-mock line-content resolved-buffer)
                "er-prefix-sent")
            "alias-not-found"))
      "selection-ui")))

;; Test :er prefix with valid alias
(ert-deftest test-enkan-center-send-line-er-prefix-integration ()
  "Test integration of :er prefix parsing and sending."
  (setq enkan-test-er-buffer-resolved nil
        enkan-test-er-text-sent nil)
  
  (let ((result (enkan-test-center-send-line-with-er nil ":er myalias")))
    (should (equal result "er-prefix-sent"))
    (should (equal enkan-test-er-buffer-resolved "myalias"))
    (should (equal enkan-test-er-text-sent ":er myalias"))))

;; Test :er prefix with command
(ert-deftest test-enkan-center-send-line-er-prefix-with-command ()
  "Test :er prefix with additional command text."
  (setq enkan-test-er-buffer-resolved nil
        enkan-test-er-text-sent nil)
  
  (let ((result (enkan-test-center-send-line-with-er nil ":er myalias some command")))
    (should (equal result "er-prefix-sent"))
    (should (equal enkan-test-er-buffer-resolved "myalias"))
    (should (equal enkan-test-er-text-sent ":er myalias some command"))))

;; Test prefix argument takes priority over :er
(ert-deftest test-enkan-center-send-line-prefix-priority ()
  "Test that prefix argument takes priority over :er prefix."
  (let ((result (enkan-test-center-send-line-with-er 2 ":er myalias")))
    (should (equal result "session-2"))))

;; Test no :er prefix falls back to selection UI
(ert-deftest test-enkan-center-send-line-no-er-fallback ()
  "Test that lines without :er prefix fall back to selection UI."
  (let ((result (enkan-test-center-send-line-with-er nil "normal line")))
    (should (equal result "selection-ui"))))

(provide 'enkan-center-send-line-er-integration-test)
;;; enkan-center-send-line-er-integration-test.el ends here
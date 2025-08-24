;;; enkan-center-send-line-er-prefix-test.el --- TDD tests for :er prefix support in center send line -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ensuring enkan-repl-center-send-line supports :er prefix like enkan-repl-send-line.
;; Must handle both prefix arguments (1-4) and line content with :er prefix.

;;; Code:

(require 'ert)

;; Pure function to parse line content for :er prefix
(defun enkan-repl-center-send-line-parse-line-pure (line-content)
  "Parse LINE-CONTENT for :er prefix notation.
Returns plist with :has-er-prefix, :command, :alias."
  (if (and (stringp line-content)
           (string-match "^:er \\([^ ]+\\)\\(?: \\(.*\\)\\)?$" line-content))
      (let ((alias (match-string 1 line-content))
            (command-part (match-string 2 line-content)))
        (list :has-er-prefix t
              :alias alias
              :command (if command-part command-part "")))
    (list :has-er-prefix nil)))

;; Pure function to determine action for center send line
(defun enkan-repl-center-send-line-determine-action-pure (prefix-arg line-content)
  "Determine action for center send line with PREFIX-ARG and LINE-CONTENT.
Returns: 'session-N, 'er-prefix, or 'selection-ui."
  (cond
   ;; Prefix argument 1-4 takes priority
   ((and (numberp prefix-arg) (<= 1 prefix-arg 4))
    (intern (format "session-%d" prefix-arg)))
   ;; Check for :er prefix in line content
   ((plist-get (enkan-repl-center-send-line-parse-line-pure line-content) :has-er-prefix)
    'er-prefix)
   ;; Default to selection UI
   (t 'selection-ui)))

;; Test prefix argument priority over :er
(ert-deftest test-enkan-center-send-line-prefix-priority-over-er ()
  "Test that prefix arguments 1-4 take priority over :er prefix in line."
  (should (eq (enkan-repl-center-send-line-determine-action-pure 1 ":er alias") 'session-1))
  (should (eq (enkan-repl-center-send-line-determine-action-pure 2 ":er alias") 'session-2))
  (should (eq (enkan-repl-center-send-line-determine-action-pure 3 ":er alias") 'session-3))
  (should (eq (enkan-repl-center-send-line-determine-action-pure 4 ":er alias") 'session-4)))

;; Test :er prefix parsing
(ert-deftest test-enkan-center-send-line-er-prefix-parsing ()
  "Test parsing of :er prefix in line content."
  (let ((result (enkan-repl-center-send-line-parse-line-pure ":er myalias")))
    (should (plist-get result :has-er-prefix))
    (should (equal (plist-get result :alias) "myalias"))
    (should (equal (plist-get result :command) "")))
  
  (let ((result (enkan-repl-center-send-line-parse-line-pure ":er myalias some command")))
    (should (plist-get result :has-er-prefix))
    (should (equal (plist-get result :alias) "myalias"))
    (should (equal (plist-get result :command) "some command"))))

;; Test :er prefix detection
(ert-deftest test-enkan-center-send-line-er-prefix-detection ()
  "Test detection of :er prefix in various line contents."
  (should (eq (enkan-repl-center-send-line-determine-action-pure nil ":er alias") 'er-prefix))
  (should (eq (enkan-repl-center-send-line-determine-action-pure nil ":er alias command") 'er-prefix))
  (should (eq (enkan-repl-center-send-line-determine-action-pure nil "normal line") 'selection-ui))
  (should (eq (enkan-repl-center-send-line-determine-action-pure nil "") 'selection-ui)))

;; Test no prefix argument with :er
(ert-deftest test-enkan-center-send-line-no-prefix-with-er ()
  "Test that :er prefix is used when no prefix argument is given."
  (should (eq (enkan-repl-center-send-line-determine-action-pure nil ":er test") 'er-prefix)))

;; Test invalid prefix arguments with :er
(ert-deftest test-enkan-center-send-line-invalid-prefix-with-er ()
  "Test that invalid prefix arguments fall back to :er prefix parsing."
  (should (eq (enkan-repl-center-send-line-determine-action-pure 0 ":er alias") 'er-prefix))
  (should (eq (enkan-repl-center-send-line-determine-action-pure 5 ":er alias") 'er-prefix))
  (should (eq (enkan-repl-center-send-line-determine-action-pure -1 ":er alias") 'er-prefix)))

;; Test edge cases for :er parsing
(ert-deftest test-enkan-center-send-line-er-parsing-edge-cases ()
  "Test edge cases for :er prefix parsing."
  (should-not (plist-get (enkan-repl-center-send-line-parse-line-pure "er alias") :has-er-prefix))
  (should-not (plist-get (enkan-repl-center-send-line-parse-line-pure ":eralias") :has-er-prefix))
  (should-not (plist-get (enkan-repl-center-send-line-parse-line-pure ":er") :has-er-prefix)))

(provide 'enkan-center-send-line-er-prefix-test)
;;; enkan-center-send-line-er-prefix-test.el ends here
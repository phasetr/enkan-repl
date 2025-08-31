;;; enkan-repl-numbered-list-test.el --- Tests for numbered session list functions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for numbered session list display and interaction

;;; Code:

(require 'ert)
(require 'enkan-repl-utils)

;;;; Tests for enkan-repl--format-numbered-sessions

(ert-deftest test-format-numbered-sessions ()
  "Test formatting numbered session list."
  (let* ((sessions
          (list
           (list :name "*enkan:/home/user/*" :directory "/home/user/" :status 'alive)
           (list :name "*enkan:/tmp/*" :directory "/tmp/" :status 'dead)))
         (result (enkan-repl--format-numbered-sessions sessions)))
    (should (string-match-p "1\\. .+enkan:/home/user/.+ — Directory: /home/user/, Status: alive" result))
    (should (string-match-p "2\\. .+enkan:/tmp/.+ — Directory: /tmp/, Status: dead" result))))

(ert-deftest test-format-numbered-sessions-empty ()
  "Test formatting empty session list."
  (let ((result (enkan-repl--format-numbered-sessions '())))
    (should (equal result ""))))

(ert-deftest test-format-numbered-sessions-single ()
  "Test formatting single session."
  (let* ((sessions
          (list
           (list :name "*enkan:/root/*" :directory "/root/" :status 'alive)))
         (result (enkan-repl--format-numbered-sessions sessions)))
    (should (string-match-p "1\\. .+enkan:/root/.+ — Directory: /root/, Status: alive" result))
    ;; Should not have a second item
    (should-not (string-match-p "2\\." result))))

(ert-deftest test-format-numbered-sessions-ordering ()
  "Test that sessions are numbered sequentially."
  (let* ((sessions
          (list
           (list :name "*enkan:/a/*" :directory "/a/" :status 'alive)
           (list :name "*enkan:/b/*" :directory "/b/" :status 'alive)
           (list :name "*enkan:/c/*" :directory "/c/" :status 'alive)))
         (result (enkan-repl--format-numbered-sessions sessions)))
    (should (string-match-p "1\\. .+enkan:/a/.+ — " result))
    (should (string-match-p "2\\. .+enkan:/b/.+ — " result))
    (should (string-match-p "3\\. .+enkan:/c/.+ — " result))))

(ert-deftest test-format-numbered-sessions-formatting ()
  "Test that formatting is consistent."
  (let* ((sessions
          (list
           (list :name "*enkan:/test/*" :directory "/test/" :status 'dead)))
         (result (enkan-repl--format-numbered-sessions sessions))
         (lines (split-string result "\n")))
    ;; Check formatting structure - single line format
    (should (string-match-p "^1\\. .+enkan:/test/.+ — Directory: /test/, Status: dead$" (nth 0 lines)))
    ;; Should only have one line per session
    (should (equal (nth 1 lines) ""))))

(provide 'enkan-repl-numbered-list-test)
;;; enkan-repl-numbered-list-test.el ends here
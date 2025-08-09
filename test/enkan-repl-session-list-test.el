;;; enkan-repl-session-list-test.el --- Tests for session list pure functions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for session list management pure functions

;;; Code:

(require 'ert)
(require 'enkan-repl-utils)

;;;; Tests for enkan-repl--extract-session-info-pure

(ert-deftest test-extract-session-info-pure-valid-session ()
  "Test extracting info from valid session buffer."
  (let ((result (enkan-repl--extract-session-info-pure
                 "*enkan:/home/user/project/*"
                 t  ; buffer-live-p
                 t  ; has-eat-process
                 t  ; process-live-p
                 )))
    (should (equal (plist-get result :name) "*enkan:/home/user/project/*"))
    (should (equal (plist-get result :directory) "/home/user/project/"))
    (should (eq (plist-get result :status) 'alive))))

(ert-deftest test-extract-session-info-pure-dead-session ()
  "Test extracting info from dead session buffer."
  (let ((result (enkan-repl--extract-session-info-pure
                 "*enkan:/tmp/test/*"
                 t   ; buffer-live-p
                 t   ; has-eat-process
                 nil ; process-live-p (dead)
                 )))
    (should (equal (plist-get result :name) "*enkan:/tmp/test/*"))
    (should (equal (plist-get result :directory) "/tmp/test/"))
    (should (eq (plist-get result :status) 'dead))))

(ert-deftest test-extract-session-info-pure-no-process ()
  "Test extracting info from buffer without eat process."
  (let ((result (enkan-repl--extract-session-info-pure
                 "*enkan:/var/log/*"
                 t   ; buffer-live-p
                 nil ; has-eat-process (no process)
                 nil ; process-live-p
                 )))
    (should (equal (plist-get result :name) "*enkan:/var/log/*"))
    (should (equal (plist-get result :directory) "/var/log/"))
    (should (eq (plist-get result :status) 'dead))))

(ert-deftest test-extract-session-info-pure-non-session ()
  "Test extracting info from non-session buffer."
  (should (null (enkan-repl--extract-session-info-pure
                 "*scratch*"
                 t t t)))
  (should (null (enkan-repl--extract-session-info-pure
                 "test.el"
                 t t t)))
  (should (null (enkan-repl--extract-session-info-pure
                 nil
                 t t t))))

(ert-deftest test-extract-session-info-pure-dead-buffer ()
  "Test extracting info from dead buffer."
  (should (null (enkan-repl--extract-session-info-pure
                 "*enkan:/home/user/*"
                 nil ; buffer-live-p (dead buffer)
                 t t))))

;;;; Tests for enkan-repl--collect-sessions-pure

(ert-deftest test-collect-sessions-pure-multiple ()
  "Test collecting multiple sessions."
  (let* ((buffer-info-list
          (list
           (list :name "*enkan:/home/user/*" :live-p t :has-eat-process t :process-live-p t)
           (list :name "*scratch*" :live-p t :has-eat-process nil :process-live-p nil)
           (list :name "*enkan:/tmp/*" :live-p t :has-eat-process t :process-live-p nil)
           (list :name "test.el" :live-p t :has-eat-process nil :process-live-p nil)))
         (sessions (enkan-repl--collect-sessions-pure buffer-info-list)))
    (should (= (length sessions) 2))
    (should (equal (plist-get (nth 0 sessions) :directory) "/home/user/"))
    (should (eq (plist-get (nth 0 sessions) :status) 'alive))
    (should (equal (plist-get (nth 1 sessions) :directory) "/tmp/"))
    (should (eq (plist-get (nth 1 sessions) :status) 'dead))))

(ert-deftest test-collect-sessions-pure-empty ()
  "Test collecting sessions from empty list."
  (should (null (enkan-repl--collect-sessions-pure '()))))

(ert-deftest test-collect-sessions-pure-no-sessions ()
  "Test collecting sessions when no enkan buffers exist."
  (let* ((buffer-info-list
          (list
           (list :name "*scratch*" :live-p t :has-eat-process nil :process-live-p nil)
           (list :name "test.el" :live-p t :has-eat-process nil :process-live-p nil)))
         (sessions (enkan-repl--collect-sessions-pure buffer-info-list)))
    (should (null sessions))))

;;;; Tests for enkan-repl--format-sessions-pure

(ert-deftest test-format-sessions-pure-with-sessions ()
  "Test formatting session list."
  (let* ((sessions
          (list
           (list :name "*enkan:/home/user/*" :directory "/home/user/" :status 'alive)
           (list :name "*enkan:/tmp/*" :directory "/tmp/" :status 'dead)))
         (output (enkan-repl--format-sessions-pure sessions)))
    (should (string-match "Active enkan-repl sessions:" output))
    (should (string-match "Press 'd' to delete" output))
    (should (string-match "\\*enkan:/home/user/\\*" output))
    (should (string-match "Directory: /home/user/" output))
    (should (string-match "Status: alive" output))
    (should (string-match "\\*enkan:/tmp/\\*" output))
    (should (string-match "Directory: /tmp/" output))
    (should (string-match "Status: dead" output))))

(ert-deftest test-format-sessions-pure-empty ()
  "Test formatting empty session list."
  (let ((output (enkan-repl--format-sessions-pure '())))
    (should (equal output "No active sessions found\n"))))

;;;; Tests for enkan-repl--find-deletion-bounds-pure

(ert-deftest test-find-deletion-bounds-on-session-name ()
  "Test finding deletion bounds when cursor is on session name."
  (let* ((lines '("Active enkan-repl sessions:"
                  "Press 'd' to delete a session, 'q' to quit"
                  "─────────────────────────────────────────"
                  ""
                  "  *enkan:/home/user/*"
                  "    Directory: /home/user/"
                  "    Status: alive"
                  ""
                  "  *enkan:/tmp/*"
                  "    Directory: /tmp/"
                  "    Status: dead"
                  ""))
         (bounds (enkan-repl--find-deletion-bounds-pure lines 4)))
    (should bounds)
    (should (= (plist-get bounds :start-line) 4))
    (should (= (plist-get bounds :end-line) 8))))

(ert-deftest test-find-deletion-bounds-on-directory ()
  "Test finding deletion bounds when cursor is on directory line."
  (let* ((lines '("Active enkan-repl sessions:"
                  "Press 'd' to delete a session, 'q' to quit"
                  "─────────────────────────────────────────"
                  ""
                  "  *enkan:/home/user/*"
                  "    Directory: /home/user/"
                  "    Status: alive"
                  ""))
         (bounds (enkan-repl--find-deletion-bounds-pure lines 5)))
    (should bounds)
    (should (= (plist-get bounds :start-line) 4))
    (should (= (plist-get bounds :end-line) 8))))

(ert-deftest test-find-deletion-bounds-on-status ()
  "Test finding deletion bounds when cursor is on status line."
  (let* ((lines '("Active enkan-repl sessions:"
                  "Press 'd' to delete a session, 'q' to quit"
                  "─────────────────────────────────────────"
                  ""
                  "  *enkan:/home/user/*"
                  "    Directory: /home/user/"
                  "    Status: alive"
                  ""))
         (bounds (enkan-repl--find-deletion-bounds-pure lines 6)))
    (should bounds)
    (should (= (plist-get bounds :start-line) 4))
    (should (= (plist-get bounds :end-line) 8))))

(ert-deftest test-find-deletion-bounds-on-header ()
  "Test finding deletion bounds when cursor is on header."
  (let* ((lines '("Active enkan-repl sessions:"
                  "Press 'd' to delete a session, 'q' to quit"
                  "─────────────────────────────────────────"
                  ""
                  "  *enkan:/home/user/*"))
         (bounds (enkan-repl--find-deletion-bounds-pure lines 0)))
    (should (null bounds))
    (should (null (enkan-repl--find-deletion-bounds-pure lines 1)))
    (should (null (enkan-repl--find-deletion-bounds-pure lines 2)))))

(ert-deftest test-find-deletion-bounds-empty-line ()
  "Test finding deletion bounds on empty line."
  (let* ((lines '("Active enkan-repl sessions:"
                  "Press 'd' to delete a session, 'q' to quit"
                  "─────────────────────────────────────────"
                  ""
                  "  *enkan:/home/user/*"
                  "    Directory: /home/user/"
                  "    Status: alive"
                  ""
                  ""))
         (bounds (enkan-repl--find-deletion-bounds-pure lines 7)))
    (should (null bounds))))

(provide 'enkan-repl-session-list-test)
;;; enkan-repl-session-list-test.el ends here
;;; enkan-repl-session-minibuffer-test.el --- Tests for session minibuffer functions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for session selection and minibuffer interaction pure functions

;;; Code:

(require 'ert)
(require 'enkan-repl-utils)

;;;; Tests for enkan-repl--prepare-session-candidates-pure

(ert-deftest test-prepare-session-candidates-pure ()
  "Test preparing candidates for completing-read."
  (let* ((sessions
          (list
           (list :name "*enkan:/home/user/*" :directory "/home/user/" :status 'alive)
           (list :name "*enkan:/tmp/*" :directory "/tmp/" :status 'dead)))
         (candidates (enkan-repl--prepare-session-candidates-pure sessions)))
    (should (= (length candidates) 2))
    (should (equal (car (nth 0 candidates)) "*enkan:/home/user/*"))
    (should (string-match "Directory: /home/user/, Status: alive" (cdr (nth 0 candidates))))
    (should (equal (car (nth 1 candidates)) "*enkan:/tmp/*"))
    (should (string-match "Directory: /tmp/, Status: dead" (cdr (nth 1 candidates))))))

(ert-deftest test-prepare-session-candidates-pure-empty ()
  "Test preparing candidates with empty session list."
  (let ((candidates (enkan-repl--prepare-session-candidates-pure '())))
    (should (null candidates))))

(ert-deftest test-prepare-session-candidates-pure-single ()
  "Test preparing candidates with single session."
  (let* ((sessions
          (list
           (list :name "*enkan:/root/*" :directory "/root/" :status 'alive)))
         (candidates (enkan-repl--prepare-session-candidates-pure sessions)))
    (should (= (length candidates) 1))
    (should (equal (car (nth 0 candidates)) "*enkan:/root/*"))
    (should (string-match "Directory: /root/, Status: alive" (cdr (nth 0 candidates))))))

;;;; Tests for enkan-repl--find-session-buffer-pure

(ert-deftest test-find-session-buffer-pure-found ()
  "Test finding buffer for existing session."
  (let* ((mock-buffer 'mock-buffer-object)
         (buffer-info-list
          (list
           (list :name "*enkan:/home/user/*" :live-p t :buffer mock-buffer)
           (list :name "*scratch*" :live-p t :buffer 'scratch-buffer))))
    (should (eq (enkan-repl--find-session-buffer-pure "*enkan:/home/user/*" buffer-info-list)
                mock-buffer))))

(ert-deftest test-find-session-buffer-pure-not-found ()
  "Test finding buffer for non-existent session."
  (let ((buffer-info-list
         (list
          (list :name "*enkan:/home/user/*" :live-p t :buffer 'buffer1)
          (list :name "*scratch*" :live-p t :buffer 'buffer2))))
    (should (null (enkan-repl--find-session-buffer-pure "*enkan:/tmp/*" buffer-info-list)))))

(ert-deftest test-find-session-buffer-pure-empty-list ()
  "Test finding buffer with empty buffer list."
  (should (null (enkan-repl--find-session-buffer-pure "*enkan:/home/user/*" '()))))

(ert-deftest test-find-session-buffer-pure-nil-name ()
  "Test finding buffer with nil name."
  (let ((buffer-info-list
         (list
          (list :name "*enkan:/home/user/*" :live-p t :buffer 'buffer1))))
    (should (null (enkan-repl--find-session-buffer-pure nil buffer-info-list)))))

;;;; Tests for enkan-repl--session-action-pure

(ert-deftest test-session-action-pure-switch ()
  "Test switch action."
  (let ((result (enkan-repl--session-action-pure ?s "*enkan:/home/user/*")))
    (should (eq (plist-get result :type) 'switch))
    (should (string-match "Switched to session:" (plist-get result :message)))
    (should (string-match "\\*enkan:/home/user/\\*" (plist-get result :message)))))

(ert-deftest test-session-action-pure-delete ()
  "Test delete action."
  (let ((result (enkan-repl--session-action-pure ?d "*enkan:/tmp/*")))
    (should (eq (plist-get result :type) 'delete))
    (should (string-match "Session deleted:" (plist-get result :message)))
    (should (string-match "\\*enkan:/tmp/\\*" (plist-get result :message)))))

(ert-deftest test-session-action-pure-cancel ()
  "Test cancel action."
  (let ((result (enkan-repl--session-action-pure ?c "*enkan:/any/*")))
    (should (eq (plist-get result :type) 'cancel))
    (should (equal (plist-get result :message) "Cancelled"))))

(ert-deftest test-session-action-pure-unknown ()
  "Test unknown action."
  (let ((result (enkan-repl--session-action-pure ?x "*enkan:/any/*")))
    (should (eq (plist-get result :type) 'unknown))
    (should (equal (plist-get result :message) "Unknown action"))))

(ert-deftest test-session-action-pure-all-actions ()
  "Test all valid actions have different types."
  (let ((switch-result (enkan-repl--session-action-pure ?s "test"))
        (delete-result (enkan-repl--session-action-pure ?d "test"))
        (cancel-result (enkan-repl--session-action-pure ?c "test")))
    (should (not (eq (plist-get switch-result :type)
                     (plist-get delete-result :type))))
    (should (not (eq (plist-get switch-result :type)
                     (plist-get cancel-result :type))))
    (should (not (eq (plist-get delete-result :type)
                     (plist-get cancel-result :type))))))

(provide 'enkan-repl-session-minibuffer-test)
;;; enkan-repl-session-minibuffer-test.el ends here
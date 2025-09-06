;;; enkan-repl-name-api-test.el --- Tests for buffer name API -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the buffer name API functions.

;;; Code:

(require 'ert)
(require 'enkan-repl-utils)

;;;; Tests for enkan-repl--is-enkan-buffer-name

(ert-deftest test-enkan-repl--is-enkan-buffer-name ()
  "Test enkan-repl--is-enkan-buffer-name function."
  ;; Valid enkan buffer names (new format)
  (should (enkan-repl--is-enkan-buffer-name "*ws:01 enkan:/home/user/project*"))
  (should (enkan-repl--is-enkan-buffer-name "*ws:99 enkan:/tmp/test*"))
  (should (enkan-repl--is-enkan-buffer-name "*ws:00 enkan:.*"))
  
  ;; Invalid cases
  (should-not (enkan-repl--is-enkan-buffer-name "*enkan:/home/user/project*"))  ; old format
  (should-not (enkan-repl--is-enkan-buffer-name "*other:/home/user*"))
  (should-not (enkan-repl--is-enkan-buffer-name "ws:01 enkan:/home/user*"))
  (should-not (enkan-repl--is-enkan-buffer-name "*ws:1 enkan:"))  ; single digit
  (should-not (enkan-repl--is-enkan-buffer-name "*ws:abc enkan:"))  ; non-numeric
  (should-not (enkan-repl--is-enkan-buffer-name nil))
  (should-not (enkan-repl--is-enkan-buffer-name ""))
  (should-not (enkan-repl--is-enkan-buffer-name 123)))

;;;; Tests for enkan-repl--buffer-name->path

(ert-deftest test-enkan-repl--buffer-name->path ()
  "Test enkan-repl--buffer-name->path function."
  ;; Valid extraction (new format)
  (let ((result (enkan-repl--buffer-name->path "*ws:01 enkan:/home/user/project*")))
    (should (stringp result))
    (should (string-suffix-p "/" result))
    (should (string-match "/home/user/project/$" result)))
  
  (let ((result (enkan-repl--buffer-name->path "*ws:99 enkan:~/test*")))
    (should (stringp result))
    (should (string-suffix-p "/" result))
    (should (string-match "/test/$" result)))
  
  ;; Invalid cases
  (should-not (enkan-repl--buffer-name->path "*enkan:/home/user/project*"))  ; old format
  (should-not (enkan-repl--buffer-name->path "*other:/home/user*"))
  (should-not (enkan-repl--buffer-name->path "ws:01 enkan:/home/user*"))
  (should-not (enkan-repl--buffer-name->path "*ws:01 enkan"))
  (should-not (enkan-repl--buffer-name->path nil))
  (should-not (enkan-repl--buffer-name->path ""))
  (should-not (enkan-repl--buffer-name->path 123)))

;;;; Tests for enkan-repl--path->buffer-name

(ert-deftest test-enkan-repl--path->buffer-name ()
  "Test enkan-repl--path->buffer-name function."
  ;; Absolute path
  (let ((result (enkan-repl--path->buffer-name "/home/user/project")))
    (should (stringp result))
    (should (string-prefix-p "*ws:01 enkan:" result))
    (should (string-suffix-p "*" result))
    (should (string-match "\\*ws:01 enkan:/home/user/project\\*" result)))
  
  ;; Path with trailing slash
  (let ((result (enkan-repl--path->buffer-name "/home/user/project/")))
    (should (stringp result))
    (should (string-match "\\*ws:01 enkan:/home/user/project/\\*" result)))
  
  ;; Relative path (will be expanded)
  (let ((result (enkan-repl--path->buffer-name "~/test")))
    (should (stringp result))
    (should (string-prefix-p "*ws:01 enkan:" result))
    (should (string-match "/test\\*$" result))))

;;;; Compatibility tests - verify old and new APIs return same results

(ert-deftest test-enkan-repl-name-api-compatibility ()
  "Test that old wrapper functions still work with new format."
  ;; Test enkan-repl--extract-directory-from-buffer-name with new format
  (let* ((buffer-name "*ws:01 enkan:/home/user/project*")
         (result (enkan-repl--extract-directory-from-buffer-name buffer-name)))
    (should (stringp result))
    (should (string-match "/home/user/project/$" result)))
  
  ;; Test enkan-repl--make-buffer-name generates new format
  (let* ((path "/home/user/project")
         (result (enkan-repl--make-buffer-name path)))
    (should (string-prefix-p "*ws:01 enkan:" result))
    (should (string-match "\\*ws:01 enkan:/home/user/project\\*" result)))
  
  ;; Test enkan-repl--buffer-matches-directory with new format
  (let* ((buffer-name "*ws:01 enkan:/home/user/project*")
         (target-dir "/home/user/project"))
    (should (enkan-repl--buffer-matches-directory buffer-name target-dir))))

;;;; Edge cases

(ert-deftest test-enkan-repl-name-api-edge-cases ()
  "Test edge cases for buffer name API."
  ;; Empty path components with new format
  (should (enkan-repl--is-enkan-buffer-name "*ws:01 enkan:/*"))
  (let ((result (enkan-repl--buffer-name->path "*ws:01 enkan:/*")))
    (should (equal result "/")))
  
  ;; Path with special characters
  (let* ((path "/home/user/my-project (copy)")
         (buffer-name (enkan-repl--path->buffer-name path))
         (extracted-path (enkan-repl--buffer-name->path buffer-name)))
    (should (string-match "/my-project (copy)/$" extracted-path)))
  
  ;; Very long path
  (let* ((long-path (concat "/home/user/" (make-string 200 ?a)))
         (buffer-name (enkan-repl--path->buffer-name long-path))
         (extracted-path (enkan-repl--buffer-name->path buffer-name)))
    (should (string-suffix-p (concat (make-string 200 ?a) "/") extracted-path))))

(provide 'enkan-repl-name-api-test)

;;; enkan-repl-name-api-test.el ends here
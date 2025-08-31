;;; enkan-repl-project-filename-test.el --- Tests for project filename helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for enkan-repl--make-project-filename and
;; enkan-repl--decode-project-filename wrappers.

;;; Code:

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-enkan-repl--make-and-decode-project-filename-basic ()
  "Encode a typical absolute path and decode back."
  (let* ((path "/tmp/enkan-repl-test/")
         (encoded (enkan-repl--make-project-filename path))
         (decoded (enkan-repl--decode-project-filename encoded)))
    (should (string= encoded "enkan--tmp--enkan-repl-test"))
    (should (string= decoded "/tmp/enkan-repl-test/"))))

(ert-deftest test-enkan-repl--make-and-decode-project-filename-root ()
  "Handle root path correctly."
  (let* ((path "/")
         (encoded (enkan-repl--make-project-filename path))
         (decoded (enkan-repl--decode-project-filename encoded)))
    (should (string= encoded "enkan"))
    (should (string= decoded "/"))))

(ert-deftest test-enkan-repl--make-and-decode-project-filename-nested ()
  "Nested directories encode/decode round-trip."
  (let* ((path "/var/tmp/a/b/c/")
         (encoded (enkan-repl--make-project-filename path))
         (decoded (enkan-repl--decode-project-filename encoded)))
    (should (string= encoded "enkan--var--tmp--a--b--c"))
    (should (string= decoded "/var/tmp/a/b/c/"))))

(provide 'enkan-repl-project-filename-test)
;;; enkan-repl-project-filename-test.el ends here


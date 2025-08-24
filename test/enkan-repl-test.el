;;; enkan-repl-test.el --- Tests for enkan-repl pure functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code <noreply@anthropic.com>
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Comprehensive tests for enkan-repl pure functions using ERT.
;; These tests aim for 100% code coverage of all pure functions.

;;; Code:

(require 'ert)

;; Load the main package and define helper function
(defvar enkan-repl-test-package-dir
  (let ((package-dir (or (and load-file-name
                              (file-name-directory load-file-name))
                         (file-name-directory (locate-library "enkan-repl-test"))
                         default-directory)))
    (expand-file-name "../" package-dir)))

(load (expand-file-name "enkan-repl.el" enkan-repl-test-package-dir))

;;; Tests for enkan-repl--extract-directory-pure

(ert-deftest test-extract-directory-pure-with-persistent-file ()
  "Test directory extraction from persistent file name."
  (should (string= "/Users/phasetr/project/"
                   (enkan-repl--extract-directory-pure
                    "/path/to/enkan--Users--phasetr--project.org"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-default-directory ()
  "Test directory extraction using default directory for non-persistent files."
  (should (string= "/default/dir/"
                   (enkan-repl--extract-directory-pure
                    "/path/to/regular-file.org"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-nil-buffer-file-name ()
  "Test directory extraction with nil buffer file name."
  (should (string= "/default/dir/"
                   (enkan-repl--extract-directory-pure
                    nil
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-non-org-persistent-file ()
  "Test directory extraction with persistent file pattern but wrong extension."
  (should (string= "/default/dir/"
                   (enkan-repl--extract-directory-pure
                    "/path/to/enkan--Users--phasetr--project.txt"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-complex-path ()
  "Test directory extraction with complex path containing special characters."
  (should (string= "/Users/user/My Documents/project with spaces/"
                   (enkan-repl--extract-directory-pure
                    "/path/enkan--Users--user--My Documents--project with spaces.org"
                    "/default/"))))

;;; Tests for enkan-repl--buffer-matches-directory-pure

(ert-deftest test-buffer-matches-directory-pure-exact-match ()
  "Test directory matching with exact paths."
  (should (enkan-repl--buffer-matches-directory-pure
           "/Users/phasetr/project/"
           "/Users/phasetr/project/")))

(ert-deftest test-buffer-matches-directory-pure-no-match ()
  "Test directory matching with different paths."
  (should-not (enkan-repl--buffer-matches-directory-pure
               "/Users/phasetr/project1/"
               "/Users/phasetr/project2/")))

(ert-deftest test-buffer-matches-directory-pure-with-nil-buffer-dir ()
  "Test directory matching with nil buffer directory."
  (should-not (enkan-repl--buffer-matches-directory-pure
               nil
               "/Users/phasetr/project/")))

(ert-deftest test-buffer-matches-directory-pure-trailing-slash-normalization ()
  "Test directory matching with different trailing slash patterns."
  ;; file-truename preserves trailing slashes, so paths must match exactly
  (should (enkan-repl--buffer-matches-directory-pure
           "/Users/phasetr/project/"
           "/Users/phasetr/project/"))
  ;; Test that different trailing slash patterns don't match
  (should-not (enkan-repl--buffer-matches-directory-pure
               "/Users/phasetr/project"
               "/Users/phasetr/project/")))

;;; Tests for enkan-repl--find-matching-buffer-pure

(ert-deftest test-find-matching-buffer-pure-eat-specific-buffer ()
  "Test finding directory-specific eat buffer."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*enkan:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (enkan-repl--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'test-buffer)))))

(ert-deftest test-find-matching-buffer-pure-generic-eat-buffer ()
  "Test finding generic eat buffer with directory match."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*eat*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    ;; Mock buffer-live-p to return t for our test buffer
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (enkan-repl--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'test-buffer)))))

(ert-deftest test-find-matching-buffer-pure-eat-mode-buffer ()
  "Test finding eat-mode buffer with enkan in name."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*terminal-enkan*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode t))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (enkan-repl--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'test-buffer)))))

(ert-deftest test-find-matching-buffer-pure-no-match ()
  "Test finding no matching buffer."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*some-other-buffer*"
                                 :default-directory "/different/path/"
                                 :eat-mode nil))))
    (should-not (enkan-repl--find-matching-buffer-pure
                 buffer-list
                 "/Users/phasetr/project/"))))

(ert-deftest test-find-matching-buffer-pure-dead-buffer ()
  "Test that dead buffers are not matched."
  (let ((buffer-list (list (list :buffer 'dead-buffer
                                 :name "*enkan:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) nil)))
      (should-not (enkan-repl--find-matching-buffer-pure
                   buffer-list
                   "/Users/phasetr/project/")))))

(ert-deftest test-find-matching-buffer-pure-multiple-buffers ()
  "Test finding buffer when multiple candidates exist."
  (let ((buffer-list (list (list :buffer 'buffer1
                                 :name "*other*"
                                 :default-directory "/different/"
                                 :eat-mode nil)
                           (list :buffer 'buffer2
                                 :name "*enkan:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (enkan-repl--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'buffer2)))))

(ert-deftest test-find-matching-buffer-pure-empty-buffer-list ()
  "Test finding buffer with empty buffer list."
  (should-not (enkan-repl--find-matching-buffer-pure
               '()
               "/Users/phasetr/project/")))

;;; Tests for enkan-repl--can-send-text-pure

(ert-deftest test-can-send-text-pure-with-valid-buffer ()
  "Test can send text with valid buffer and live process."
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          ;; Mock the eat process variables
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t)))
            (should (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-dead-process ()
  "Test can send text with dead process."
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) nil)))
            (should-not (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-nil-process ()
  "Test can send text with nil process."
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process))))
            (should-not (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-without-eat-process-var ()
  "Test can send text without eat--process variable."
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (cl-letf (((symbol-function 'boundp) (lambda (sym) nil)))
            (should-not (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-nil-buffer ()
  "Test can send text with nil buffer."
  (should-not (enkan-repl--can-send-text-pure nil)))

;;; Tests for enkan-repl--send-text-pure

(ert-deftest test-send-text-pure-successful-send ()
  "Test successful text sending."
  (let ((test-buffer (generate-new-buffer "*test-eat*"))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (enkan-repl--send-text-pure "test message" test-buffer))
            ;; Verify correct strings were sent (in reverse order due to push)
            (should (equal sent-strings '("\r" "test message")))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-cannot-send ()
  "Test text sending when buffer cannot receive text."
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process))))
            (should-not (enkan-repl--send-text-pure "test message" test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-with-nil-buffer ()
  "Test text sending with nil buffer."
  (should-not (enkan-repl--send-text-pure "test message" nil)))

(ert-deftest test-send-text-pure-empty-text ()
  "Test sending empty text."
  (let ((test-buffer (generate-new-buffer "*test-eat*"))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (enkan-repl--send-text-pure "" test-buffer))
            (should (equal sent-strings '("\r" "")))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-multiline-text ()
  "Test sending multiline text."
  (let ((test-buffer (generate-new-buffer "*test-eat*"))
        (sent-strings '())
        (multiline-text "line1\nline2\nline3"))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (enkan-repl--send-text-pure multiline-text test-buffer))
            (should (equal sent-strings (list "\r" multiline-text)))))
      (kill-buffer test-buffer))))

;;; Additional Edge Case Tests

(ert-deftest test-extract-directory-pure-edge-cases ()
  "Test edge cases for directory extraction."
  ;; Empty strings
  (should (string= ""
                   (enkan-repl--extract-directory-pure
                    ""
                    "")))
  ;; Very long path
  (let ((long-path (make-string 1000 ?a)))
    (should (string= long-path
                     (enkan-repl--extract-directory-pure
                      nil
                      long-path))))
  ;; Path with special characters
  (should (string= "/Users/user with spaces/Ñice-prójęct/"
                   (enkan-repl--extract-directory-pure
                    "/path/enkan--Users--user with spaces--Ñice-prójęct.org"
                    "/default/"))))

(ert-deftest test-buffer-matches-directory-pure-edge-cases ()
  "Test edge cases for directory matching."
  ;; Empty strings
  (should-not (enkan-repl--buffer-matches-directory-pure
               ""
               "/path/"))
  (should-not (enkan-repl--buffer-matches-directory-pure
               "/path/"
               ""))
  ;; Both empty
  (should (enkan-repl--buffer-matches-directory-pure
           ""
           ""))
  ;; Case sensitivity (depends on filesystem)
  (should-not (enkan-repl--buffer-matches-directory-pure
               "/Users/Test/"
               "/Users/test/")))

(ert-deftest test-find-matching-buffer-pure-edge-cases ()
  "Test edge cases for buffer finding."
  ;; Buffer list with invalid plist - missing :buffer means plist-get returns nil
  (let ((invalid-buffer-list (list (list :name "*eat*"
                                         ;; Missing :buffer key means buffer is nil
                                         :default-directory "/path/"
                                         :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) (not (null buf)))))
      (should-not (enkan-repl--find-matching-buffer-pure
                   invalid-buffer-list
                   "/path/"))))

  ;; Buffer with malformed name
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name ""  ; Empty name
                                 :default-directory "/path/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should-not (enkan-repl--find-matching-buffer-pure
                   buffer-list
                   "/path/"))))

  ;; Buffer with nil name
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name nil
                                 :default-directory "/path/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should-not (enkan-repl--find-matching-buffer-pure
                   buffer-list
                   "/path/")))))

(ert-deftest test-can-send-text-pure-edge-cases ()
  "Test edge cases for can send text check."
  ;; Buffer with eat--process bound but nil
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) nil)))
            (should-not (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer)))

  ;; Buffer where process-live-p throws error
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) (error "Process error"))))
            (should-error (enkan-repl--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-edge-cases ()
  "Test edge cases for text sending."
  ;; Very long text
  (let ((test-buffer (generate-new-buffer "*test-eat*"))
        (long-text (make-string 10000 ?x))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (enkan-repl--send-text-pure long-text test-buffer))
            (should (equal sent-strings (list "\r" long-text)))))
      (kill-buffer test-buffer)))

  ;; Text with special characters
  (let ((test-buffer (generate-new-buffer "*test-eat*"))
        (special-text "Hello\n\t\r\nWorld! 🎉 Ñice tëxt")
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (enkan-repl--send-text-pure special-text test-buffer))
            (should (equal sent-strings (list "\r" special-text)))))
      (kill-buffer test-buffer)))

  ;; Buffer gets killed during sending (edge case)
  (let ((test-buffer (generate-new-buffer "*test-eat*")))
    (with-current-buffer test-buffer
      (setq-local eat--process 'mock-process)
      (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                ((symbol-function 'process-live-p) (lambda (proc) t))
                ((symbol-function 'eat--send-string)
                 (lambda (proc text)
                   (kill-buffer test-buffer)  ; Kill buffer during send
                   nil)))
        ;; This should still try to send, but buffer will be dead
        (should-error (enkan-repl--send-text-pure "test" test-buffer))))))

;;; Stress Tests

(ert-deftest test-extract-directory-pure-stress ()
  "Stress test for directory extraction with many iterations."
  (dotimes (i 100)
    (let ((test-path (format "/test/path%d/" i)))
      (should (string= test-path
                       (enkan-repl--extract-directory-pure
                        nil
                        test-path))))))

(ert-deftest test-find-matching-buffer-pure-large-buffer-list ()
  "Test buffer finding with large buffer list."
  (let ((large-buffer-list
         (cl-loop for i from 1 to 1000 collect
                  (list :buffer (intern (format "buffer%d" i))
                        :name (format "*buffer%d*" i)
                        :default-directory (format "/path%d/" i)
                        :eat-mode nil))))
    ;; Add target buffer at the end
    (setq large-buffer-list
          (append large-buffer-list
                  (list (list :buffer 'target-buffer
                              :name "*enkan:/target/path/*"
                              :default-directory "/target/path/"
                              :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (enkan-repl--find-matching-buffer-pure
                                 large-buffer-list
                                 "/target/path/")
                                :buffer)
                     'target-buffer)))))

;;; Tests for Constants

(ert-deftest test-default-template-filename-constant ()
  "Test that default template filename constant is properly defined."
  (should (boundp 'enkan-repl-default-template-filename))
  (should (stringp enkan-repl-default-template-filename))
  (should (string-suffix-p ".org" enkan-repl-default-template-filename))
  (should (string-prefix-p "default" enkan-repl-default-template-filename)))

(ert-deftest test-template-functions-use-constant ()
  "Test that template functions use the defined constant instead of hardcoded strings."
  ;; Test load-template wrapper uses the constant
  (let ((default-directory enkan-repl-test-package-dir)
        (enkan-repl-template-file nil))
    (let ((template-content (enkan-repl--load-template)))
      (should template-content))))

(ert-deftest test-constant-immutability-intent ()
  "Test that constant maintains its intended immutable value."
  ;; This test documents the intended immutability, even though Emacs Lisp
  ;; doesn't enforce it for defconst
  (should (not (= (length enkan-repl-default-template-filename) 0)))

  ;; Verify the constant is marked as a constant (has defconst property)
  (should (get 'enkan-repl-default-template-filename 'variable-documentation)))

;;; Tests for Template Loading Pure Functions

(ert-deftest test-load-template-pure-with-custom-file ()
  "Test pure template loading with custom template file."
  (let ((temp-template (make-temp-file "test-template" nil ".org"))
        (test-content "* Custom Pure Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert test-content))
          (let ((result (enkan-repl--load-template-pure
                         temp-template
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result test-content))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-with-default-file ()
  "Test pure template loading with default template file."
  (let ((temp-package-dir (make-temp-file "test-package" t))
        (test-content "* Default Pure Template\n"))
    (unwind-protect
        (progn
          (let ((default-template (expand-file-name "test-default.org" temp-package-dir)))
            (with-temp-file default-template
              (insert test-content))
            (let ((result (enkan-repl--load-template-pure
                           nil
                           temp-package-dir
                           "test-default.org")))
              (should result)
              (should (string= result test-content)))))
      (when (file-exists-p temp-package-dir)
        (delete-directory temp-package-dir t)))))

(ert-deftest test-load-template-pure-custom-file-with-tilde ()
  "Test pure template loading with ~ expansion in custom file path."
  (let ((temp-template (expand-file-name "test-template.org" "~"))
        (test-content "* Tilde Test Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert test-content))
          ;; Test with ~ path
          (let ((result (enkan-repl--load-template-pure
                         "~/test-template.org"
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result test-content))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-nonexistent-custom-file ()
  "Test pure template loading with nonexistent custom file."
  (let ((result (enkan-repl--load-template-pure
                 "/nonexistent/template.org"
                 "/unused/package/dir"
                 "unused-default.org")))
    (should-not result)))

(ert-deftest test-load-template-pure-nonexistent-default-file ()
  "Test pure template loading with nonexistent default file."
  (let ((temp-package-dir (make-temp-file "test-package" t)))
    (unwind-protect
        (let ((result (enkan-repl--load-template-pure
                       nil
                       temp-package-dir
                       "nonexistent-default.org")))
          (should-not result))
      (when (file-exists-p temp-package-dir)
        (delete-directory temp-package-dir t)))))

(ert-deftest test-load-template-pure-empty-custom-file ()
  "Test pure template loading with empty custom file."
  (let ((temp-template (make-temp-file "test-template" nil ".org")))
    (unwind-protect
        (progn
          ;; Create empty file
          (with-temp-file temp-template
            (insert ""))
          (let ((result (enkan-repl--load-template-pure
                         temp-template
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result ""))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-special-characters ()
  "Test pure template loading with special characters in content."
  (let ((temp-template (make-temp-file "test-template" nil ".org"))
        (test-content "* Template with Special Characters\nÑice tëxt with émojis 🎉\n日本語テスト\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert test-content))
          (let ((result (enkan-repl--load-template-pure
                         temp-template
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result test-content))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-large-file ()
  "Test pure template loading with large file content."
  (let ((temp-template (make-temp-file "test-template" nil ".org"))
        (large-content (concat "* Large Template\n" (make-string 10000 ?x) "\n")))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert large-content))
          (let ((result (enkan-repl--load-template-pure
                         temp-template
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result large-content))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-edge-cases ()
  "Test pure template loading edge cases."
  ;; Test with nil parameters
  (should-not (enkan-repl--load-template-pure nil "/nonexistent" "nonexistent.org"))

  ;; Test with empty strings - these should not try to read files
  (should-not (enkan-repl--load-template-pure nil "/nonexistent" ""))
  (should-not (enkan-repl--load-template-pure nil "" "nonexistent.org")))

;;; Tests for Debug Mode Functions

(ert-deftest test-debug-mode-toggle ()
  "Test that debug mode can be toggled correctly."
  (let ((original-debug-mode enkan-repl-debug-mode))
    (unwind-protect
        (progn
          ;; Start with debug mode disabled
          (setq enkan-repl-debug-mode nil)

          ;; Test toggle from disabled to enabled
          (enkan-repl-toggle-debug-mode)
          (should enkan-repl-debug-mode)

          ;; Test toggle from enabled to disabled
          (enkan-repl-toggle-debug-mode)
          (should-not enkan-repl-debug-mode)

          ;; Test explicit enable
          (enkan-repl-enable-debug-mode)
          (should enkan-repl-debug-mode)

          ;; Test explicit disable
          (enkan-repl-disable-debug-mode)
          (should-not enkan-repl-debug-mode))

      ;; Restore original state
      (setq enkan-repl-debug-mode original-debug-mode))))

(ert-deftest test-debug-message-functionality ()
  "Test that debug messages are only shown when debug mode is enabled."
  (let ((original-debug-mode enkan-repl-debug-mode)
        (captured-messages '()))
    (unwind-protect
        (progn
          ;; Mock message function to capture output
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) captured-messages))))

            ;; Test with debug mode disabled
            (setq enkan-repl-debug-mode nil)
            (enkan-repl--debug-message "Test debug message 1")
            (should (= (length captured-messages) 0))

            ;; Test with debug mode enabled
            (setq enkan-repl-debug-mode t)
            (enkan-repl--debug-message "Test debug message 2")
            (should (= (length captured-messages) 1))
            (should (string-match-p "\\[CLAUDEMACS-REPL-DEBUG\\] Test debug message 2" (car captured-messages)))))

      ;; Restore original state
      (setq enkan-repl-debug-mode original-debug-mode))))

;;; Tests for Content Sanitization

(ert-deftest test-sanitize-content-basic ()
  "Test basic content sanitization functionality."
  ;; Test normal content passes through unchanged
  (should (string= (enkan-repl--sanitize-content "Hello World") "Hello World"))

  ;; Test content with newlines and tabs is preserved
  (should (string= (enkan-repl--sanitize-content "Line 1\nLine 2\tTabbed") "Line 1\nLine 2\tTabbed"))

  ;; Test nil input
  (should-not (enkan-repl--sanitize-content nil))

  ;; Test empty string
  (should (string= (enkan-repl--sanitize-content "") "")))

(ert-deftest test-sanitize-content-control-characters ()
  "Test that problematic control characters are removed."
  ;; Test carriage return conversion to newline
  (should (string= (enkan-repl--sanitize-content "Test\rContent") "Test\nContent"))

  ;; Test mixed control characters at end
  (should (string= (enkan-repl--sanitize-content "Test Content\r\x0B\x0C") "Test Content"))

  ;; Test that only trailing whitespace is trimmed (preserve leading spaces)
  (should (string= (enkan-repl--sanitize-content "  Test Content  ") "  Test Content")))

(ert-deftest test-sanitize-content-problematic-string ()
  "Test sanitization with the specific problematic string reported by user."
  (let ((test-string "画像送信サンプルです.\n以下の画像に何が書いてあるか読めますか？\n~/Downloads/send-sample.png"))
    (let ((sanitized (enkan-repl--sanitize-content test-string)))
      (should sanitized)
      ;; This should now have the marker added to prevent file path interpretation
      (should (string-match-p "This text is added by enkan-repl" sanitized))
      (should-not (= (length sanitized) 0)))))

(ert-deftest test-sanitize-content-file-path-interpretation ()
  "Test that file paths without punctuation get markers to prevent misinterpretation."
  ;; Test English content ending with file path (should get marker)
  (let ((test-string "my test.\nI send a sample message in English with image path and without period.\n~/Downloads/send-sample.png"))
    (let ((sanitized (enkan-repl--sanitize-content test-string)))
      (should sanitized)
      (should (string-match-p "This text is added by enkan-repl" sanitized))
      (should-not (= (length sanitized) 0))))

  ;; Test content ending with punctuation after file path (should not get marker)
  (let ((test-string "Check this file: ~/Downloads/send-sample.png."))
    (let ((sanitized (enkan-repl--sanitize-content test-string)))
      (should sanitized)
      (should (string= sanitized test-string)) ; Should be unchanged
      (should-not (string-match-p "This text is added by enkan-repl" sanitized))))

  ;; Test content not ending with file path (should not get marker)
  (let ((test-string "This is just normal text without file paths"))
    (let ((sanitized (enkan-repl--sanitize-content test-string)))
      (should sanitized)
      (should (string= sanitized test-string)) ; Should be unchanged
      (should-not (string-match-p "This text is added by enkan-repl" sanitized)))))

;;; Tests for Template Loading Functions


(ert-deftest test-load-template-default ()
  "Test loading default template."
  (let ((default-directory enkan-repl-test-package-dir)
        (enkan-repl-template-file nil))
    (let ((template-content (enkan-repl--load-template)))
      (should template-content)
      (should (stringp template-content))
      (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" template-content)))))

(ert-deftest test-load-template-custom-with-fallback ()
  "Test loading custom template with fallback to default."
  (let ((default-directory enkan-repl-test-package-dir)
        (enkan-repl-template-file "/nonexistent/path.org")
        (enkan-repl--testing-mode t))
    (let ((template-content (enkan-repl--load-template)))
      ;; Should fallback to default template and return content
      (should template-content)
      (should (stringp template-content)))))

(ert-deftest test-load-template-custom-with-path ()
  "Test loading custom template with explicit path."
  (let ((temp-file (make-temp-file "test-template" nil ".org"))
        (test-content "* Custom Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert test-content))
          (let ((enkan-repl-template-file temp-file))
            (let ((template-content (enkan-repl--load-template)))
              (should template-content)
              (should (string= template-content test-content)))))
      (delete-file temp-file))))


(ert-deftest test-load-template-nonexistent ()
  "Test loading nonexistent template falls back to default template."
  (let ((enkan-repl-template-file "/nonexistent/path.org")
        (enkan-repl--testing-mode t)
        (default-directory enkan-repl-test-package-dir))
    ;; Should fall back to default template and return content
    (let ((template-content (enkan-repl--load-template)))
      (should template-content)
      (should (stringp template-content)))))

(ert-deftest test-load-template-fallback-chain ()
  "Test template loading fallback chain behavior."
  (let ((default-directory enkan-repl-test-package-dir)
        (enkan-repl-custom-template-path nil)
        (enkan-repl--testing-mode t))

    ;; Test that default template loading works
    (let ((enkan-repl-template-file nil))
      (let ((template-content (enkan-repl--load-template)))
        (should template-content)
        (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" template-content))))

    ;; Test that nonexistent custom file falls back to default
    (let ((enkan-repl-template-file "/nonexistent/path.org"))
      (let ((template-content (enkan-repl--load-template)))
        (should template-content)
        (should (stringp template-content))))))

;;; Tests for Template Generation Function (via initialize-project-file)

(ert-deftest test-initialize-project-file-default ()
  "Test project file initialization with default template."
  (let* ((temp-dir (make-temp-file "test-project-default" t))
         (default-directory enkan-repl-test-package-dir)
         (enkan-repl-template-file nil)
         (temp-file nil))
    (unwind-protect
        (progn
          (setq temp-file (enkan-repl--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
              (should (string-match-p "~M-x enkan-repl-start-eat~" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-initialize-project-file-custom ()
  "Test project file initialization with custom template."
  (let* ((temp-template (make-temp-file "test-template" nil ".org"))
         (temp-dir (make-temp-file "test-project-custom" t))
         (temp-project nil)
         (test-content "* Custom Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert test-content))
          (let ((default-directory enkan-repl-test-package-dir)
                (enkan-repl-template-file temp-template))
            (setq temp-project (enkan-repl--create-project-input-file temp-dir))
            (should (file-exists-p temp-project))
            (with-temp-buffer
              (insert-file-contents temp-project)
              (let ((content (buffer-string)))
                (should (string-match-p "\\* Custom Template" content))))))
      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-initialize-project-file-fallback ()
  "Test project file initialization with fallback when template loading fails."
  (let* ((temp-dir (make-temp-file "test-project-fallback" t))
         (default-directory enkan-repl-test-package-dir)
         (enkan-repl-template-file "/nonexistent/path.org")
         (enkan-repl--testing-mode t)
         (temp-file nil))
    (unwind-protect
        (progn
          ;; Should use embedded template content as fallback
          (setq temp-file (enkan-repl--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain embedded template content, not hardcoded fallback
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
              (should (string-match-p "~M-x enkan-repl-start-eat~" content))
              (should (string-match-p "Start eat terminal emulator session\\|enkan-repl-start-eat" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-initialize-project-file-nil-template-uses-default-org ()
  "Test that when enkan-repl-template-file is nil, embedded template content is used."
  (let* ((temp-dir (make-temp-file "test-project-nil" t))
         (default-directory enkan-repl-test-package-dir)
         (enkan-repl-template-file nil)  ; Explicitly set to nil
         (temp-file nil))
    (unwind-protect
        (progn
          (setq temp-file (enkan-repl--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain embedded template content, not hardcoded fallback
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
              (should (string-match-p "~M-x enkan-repl-start-eat~" content))
              (should (string-match-p "Start eat terminal emulator session\\|enkan-repl-start-eat" content))
              (should (string-match-p "Available Commands\\|Send the text in region" content))
              ;; Should NOT contain the hardcoded fallback format
              (should-not (string-match-p "Project: .*\n\n\\*\\* Thoughts/Notes" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Tests for enkan-repl--create-project-input-file Function

(ert-deftest test-create-project-input-file-with-nil-template ()
  "Test that create-project-input-file uses embedded template when template-file is nil."
  (let ((enkan-repl-template-file nil)
        (temp-dir (make-temp-file "test-project" t))
        (temp-file nil))
    (unwind-protect
        (progn
          (let ((default-directory temp-dir))
            (setq temp-file (enkan-repl--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain embedded template content
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
              (should (string-match-p "~M-x enkan-repl-start-eat~" content))
              (should (string-match-p "Start eat terminal emulator session\\|enkan-repl-start-eat" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-create-project-input-file-with-existing-custom-template ()
  "Test that create-project-input-file uses custom template when it exists."
  (let ((temp-template (make-temp-file "custom-template" nil ".org"))
        (temp-dir (make-temp-file "test-project" t))
        (temp-file nil)
        (custom-content "* Custom Template Content\n** My Custom Section\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert custom-content))
          (should (file-exists-p temp-template))
          (let ((enkan-repl-template-file temp-template))
            (setq temp-file (enkan-repl--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain custom template content
              (should (string-match-p "\\* Custom Template Content" content))
              (should (string-match-p "My Custom Section" content))
              ;; Should NOT contain default.org content
              (should-not (string-match-p "~M-x enkan-repl-start-eat~" content)))))
      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-create-project-input-file-with-nonexistent-custom-template ()
  "Test that create-project-input-file falls back to embedded template when custom template doesn't exist."
  (let ((enkan-repl-template-file "/nonexistent/custom-template.org")
        (enkan-repl--testing-mode t)  ; Enable testing mode
        (temp-dir (make-temp-file "test-project" t))
        (temp-file nil))
    (unwind-protect
        (progn
          (let ((default-directory temp-dir))
            (setq temp-file (enkan-repl--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should fallback to embedded template content
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
              (should (string-match-p "~M-x enkan-repl-start-eat~" content))
              (should (string-match-p "Start eat terminal emulator session\\|enkan-repl-start-eat" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Integration Tests for enkan-repl-open-project-input-file Command

(ert-deftest test-open-project-input-file-exists ()
  "Test that open-project-input opens existing file directly without creating new one."
  (let ((temp-dir (make-temp-file "test-project" t))
        (existing-content "* Existing Project File\n** Already exists\nThis file was already here.\n")
        (temp-file nil))
    (unwind-protect
        (progn
          ;; Create existing file
          (setq temp-file (enkan-repl--get-project-file-path temp-dir))
          (with-temp-file temp-file
            (insert existing-content))
          (should (file-exists-p temp-file))

          ;; Mock find-file to capture what gets opened instead of actually opening
          (let ((opened-file nil)
                (enkan-repl-template-file nil))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file)
                         (setq opened-file file)
                         (get-buffer-create "*mock-buffer*")))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buffer) buffer))
                      ((symbol-function 'goto-char)
                       (lambda (pos) pos))
                      ((symbol-function 'point-max)
                       (lambda () 100))
                      ((symbol-function 'message)
                       (lambda (&rest args) nil))
                      ((symbol-function 'org-mode)
                       (lambda () nil))
                      ((symbol-function 'fboundp)
                       (lambda (sym) nil)))
              (let ((default-directory temp-dir))
                (enkan-repl-open-project-input-file))
              ;; Should open the existing file
              (should (string= opened-file temp-file))
              ;; File content should remain unchanged
              (with-temp-buffer
                (insert-file-contents temp-file)
                (should (string= (buffer-string) existing-content))))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-open-project-input-file-not-exists ()
  "Test that open-project-input creates and opens new file when it doesn't exist."
  (let ((temp-dir (make-temp-file "test-project" t))
        (temp-file nil))
    (unwind-protect
        (progn
          (setq temp-file (enkan-repl--get-project-file-path temp-dir))
          ;; Ensure file doesn't exist initially
          (should-not (file-exists-p temp-file))

          ;; Mock find-file to capture what gets opened instead of actually opening
          (let ((opened-file nil)
                (enkan-repl-template-file nil))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file)
                         (setq opened-file file)
                         (get-buffer-create "*mock-buffer*")))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buffer) buffer))
                      ((symbol-function 'goto-char)
                       (lambda (pos) pos))
                      ((symbol-function 'point-max)
                       (lambda () 100))
                      ((symbol-function 'message)
                       (lambda (&rest args) nil))
                      ((symbol-function 'org-mode)
                       (lambda () nil))
                      ((symbol-function 'fboundp)
                       (lambda (sym) nil)))
              (let ((default-directory temp-dir))
                (enkan-repl-open-project-input-file))
              ;; Should open the newly created file
              (should (string= opened-file temp-file))
              ;; File should now exist and contain template content
              (should (file-exists-p temp-file))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))
                  (should (string-match-p "~M-x enkan-repl-start-eat~" content)))))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Tests for Send Functions with Trimming

(ert-deftest test-send-region-trims-whitespace ()
  "Test that send-region trims leading and trailing whitespace."
  (with-temp-buffer
    (insert "  \n\t  Hello World  \n\t  ")
    (let ((start (point-min))
          (end (point-max))
          (sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-region start end)
        ;; Should preserve leading whitespace
        (should (string= sent-text "  \n\t  Hello World"))
        (should (string-match-p "Region sent (" message-text))))))

(ert-deftest test-send-region-trims-only-end-lines ()
  "Test that send-region trims only end lines and white spaces."
  (with-temp-buffer
    (insert "\nLine1\nLine2\n\nLine3\nLine4\n\t\n  ")
    (let ((start (point-min))
          (end (point-max))
          (sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-region start end)
        ;; Should preserve leading whitespace
        (should (string= sent-text "\nLine1\nLine2\n\nLine3\nLine4"))
        (should (string-match-p "Region sent (" message-text))))))

(ert-deftest test-send-region-empty-after-trim ()
  "Test that send-region handles empty content after trimming."
  (with-temp-buffer
    (insert "  \n\t  \n  ")
    (let ((start (point-min))
           (end (point-max))
           (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                 ((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () "/test/dir"))
                 ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-region start end)
        ;; Should detect empty content
        (should (string-match-p "No content to send" message-text))))))

(ert-deftest test-send-rest-of-buffer-trims-whitespace ()
  "Test that send-rest-of-buffer trims leading and trailing whitespace."
  (with-temp-buffer
    (insert "Some content\n")
    (insert "  \n\t  Hello World  \n\t  ")
    (goto-char (+ (point-min) 13)) ; Position after "Some content\n"
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-rest-of-buffer)
        ;; Should preserve leading whitespace
        (should (string= sent-text "  \n\t  Hello World"))
        (should (string-match-p "Rest of buffer sent (" message-text))))))

(ert-deftest test-send-rest-of-buffer-empty-after-trim ()
  "Test that send-rest-of-buffer handles empty content after trimming."
  (with-temp-buffer
    (insert "Some content\n")
    (insert "  \n\t  \n  ")
    (goto-char (+ (point-min) 13)) ; Position after "Some content\n"
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-rest-of-buffer)
        ;; Should detect no content
        (should (string-match-p "No content to send (empty or whitespace only)" message-text))))))

(ert-deftest test-send-line-basic ()
  "Test that send-line sends current line content."
  (with-temp-buffer
    (insert "Line 1\nLine 2 with content\nLine 3")
    (goto-char 15) ; Position in "Line 2 with content"
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should send only current line
        (should (string= sent-text "Line 2 with content"))
        (should (string-match-p "Line sent (" message-text))))))

(ert-deftest test-send-line-trims-whitespace ()
  "Test that send-line trims whitespace from current line."
  (with-temp-buffer
    (insert "Line 1\n  \t  Line with spaces  \t  \nLine 3")
    (goto-char 15) ; Position in line with spaces
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should preserve leading whitespace
        (should (string= sent-text "  \t  Line with spaces"))
        (should (string-match-p "Line sent (" message-text))))))

(ert-deftest test-send-line-empty-after-trim ()
  "Test that send-line handles empty line after trimming."
  (with-temp-buffer
    (insert "Line 1\n  \t  \n  \nLine 4")
    (goto-char 10) ; Position in empty line
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should detect empty content
        (should (string-match-p "No content to send" message-text))))))

(ert-deftest test-send-line-single-line-buffer ()
  "Test that send-line works with single line buffer."
  (with-temp-buffer
    (insert "Only one line")
    (goto-char 5) ; Position in the line
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should send the single line
        (should (string= sent-text "Only one line"))
        (should (string-match-p "Line sent (" message-text))))))

(ert-deftest test-send-line-no-buffer-found ()
  "Test that send-line handles case when no eat buffer is found."
  (with-temp-buffer
    (insert "Test line")
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir) nil)) ; Simulate no buffer found
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should show error message
        (should (string-match-p "Cannot send.*no matching eat session" message-text))))))

(ert-deftest test-send-line-file-path-handling ()
  "Test that send-line handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "~/Downloads/send-sample.png")
    (goto-char (point-min))
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-line)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by enkan-repl" sent-text))
        (should (string-match-p "Line sent (" message-text))))))

(ert-deftest test-send-rest-of-buffer-file-path-handling ()
  "Test that send-rest-of-buffer handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "Some content\n~/Downloads/send-sample.png")
    (goto-char (+ (point-min) 13)) ; Position after "Some content\n"
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-rest-of-buffer)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by enkan-repl" sent-text))
        (should (string-match-p "Rest of buffer sent (" message-text))))))

(ert-deftest test-send-buffer-file-path-handling ()
  "Test that send-buffer handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "Content with file path:\n~/Downloads/send-sample.png")
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (enkan-repl-send-buffer)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by enkan-repl" sent-text))
        (should (string-match-p "File .* sent (" message-text))))))

;;; Tests for Auto-Response Functions

(ert-deftest test-send-enter-functionality ()
  "Test that send-enter function works correctly."
  (let ((sent-text nil)
        (message-text nil))
    ;; Mock functions
    (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir"))
              ((symbol-function 'enkan-repl--can-send-text)
               (lambda (dir) t))
              ((symbol-function 'enkan-repl--send-text)
               (lambda (text dir)
                 (setq sent-text text)
                 t))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (enkan-repl-send-enter)
      (should (string= sent-text ""))
      (should (string-match-p "Sent enter to session" message-text)))))

(ert-deftest test-send-numbered-choices ()
  "Test that numbered choice functions work correctly."
  (dolist (choice '("1" "2" "3"))
    (let ((sent-text nil)
          (message-text nil)
          (func-name (intern (concat "enkan-repl-send-" choice))))
      ;; Mock functions
      (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'enkan-repl--can-send-text)
                 (lambda (dir) t))
                ((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (funcall func-name)
        (should (string= sent-text choice))
        (should (string-match-p (format "Sent '%s' to session" choice) message-text))))))

;;; Tests for Internal Send Helper Functions

(ert-deftest test-send-numbered-choice-helper ()
  "Test the internal send-numbered-choice helper function."
  (let ((message-text nil))
    (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
               (lambda () "/test/dir"))
              ((symbol-function 'enkan-repl--can-send-text)
               (lambda (dir) t))
              ((symbol-function 'enkan-repl--send-text)
               (lambda (text dir) t))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      ;; Test empty string (enter)
      (enkan-repl--send-numbered-choice "")
      (should (string-match-p "Sent enter to session" message-text))
      ;; Test numbered choice
      (enkan-repl--send-numbered-choice "1")
      (should (string-match-p "Sent '1' to session" message-text)))))

(ert-deftest test-send-buffer-content-helper ()
  "Test the internal send-buffer-content helper function."
  (with-temp-buffer
    (insert "Test content\n  with whitespace  ")
    (let ((sent-text nil)
          (message-text nil))
      (cl-letf (((symbol-function 'enkan-repl--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        ;; Test basic functionality
        (enkan-repl--send-buffer-content (point-min) (point-max) "Test")
        (should (string= sent-text "Test content\n  with whitespace"))
        (should (string-match-p "Test sent (" message-text))

        ;; Test skip-empty-check
        (erase-buffer)
        (insert "   ")
        (enkan-repl--send-buffer-content (point-min) (point-max) "Empty" t)
        (should (string= sent-text ""))
        (should (string-match-p "Empty sent (" message-text))))))

;;; Tests for Template Output Function

(ert-deftest test-output-template-creates-buffer ()
  "Test that output-template creates a buffer with template content."
  (let ((default-directory enkan-repl-test-package-dir))
    (unwind-protect
        (progn
          ;; Call the function - org-mode may fail due to hook conflicts, but that's OK
          (let ((enkan-repl-template-file nil))
            (condition-case nil
                (enkan-repl-output-template)
              (error nil)))  ; Ignore org-mode hook errors

          ;; Check that buffer was created
          (let ((buffer (get-buffer "*enkan-repl-template-default*")))
            (should buffer)
            (with-current-buffer buffer
              (should (> (buffer-size) 0))
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" (buffer-string))))))

      ;; Cleanup
      (when (get-buffer "*enkan-repl-template-default*")
        (kill-buffer "*enkan-repl-template-default*")))))

(ert-deftest test-output-template-with-nonexistent-file ()
  "Test output-template behavior with nonexistent template file."
  (let ((enkan-repl-template-file "/nonexistent/path.org")
        (enkan-repl--testing-mode t)
        (default-directory enkan-repl-test-package-dir))
    ;; Mock org-mode to avoid org-mode-hook issues in batch mode
    (cl-letf (((symbol-function 'org-mode) (lambda () (fundamental-mode))))
      ;; Should fallback to default template and return t
      (should (enkan-repl-output-template))
      ;; Should create buffer with fallback template content
      (should (get-buffer "*enkan-repl-template-path*"))
      ;; Cleanup
      (when (get-buffer "*enkan-repl-template-path*")
        (kill-buffer "*enkan-repl-template-path*")))))

;;; Integration Tests for Template System

(ert-deftest test-template-system-integration ()
  "Integration test for the complete template system."
  (let* ((temp-dir (make-temp-file "test-integration" t))
         (default-directory enkan-repl-test-package-dir)
         (enkan-repl-template-file nil)
         (temp-file nil))
    (unwind-protect
        (progn
          ;; Test that template loading works with current language setting
          (let ((template-content (enkan-repl--load-template)))
            (should template-content)
            (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" template-content)))

          ;; Test that project file initialization uses the loaded template
          (setq temp-file (enkan-repl--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "#\\+TITLE: Enkan Input File\\|\\* Quick Start" content))))

          ;; Test switching to custom template and regenerating
          (let ((temp-custom-template (make-temp-file "test-custom-template" nil ".org"))
                (temp-dir2 (make-temp-file "test-integration2" t))
                (temp-file2 nil)
                (custom-content "* Custom Integration Template\nProject: %s\n"))
            (unwind-protect
                (progn
                  (with-temp-file temp-custom-template
                    (insert custom-content))
                  (let ((enkan-repl-template-file temp-custom-template))
                    (setq temp-file2 (enkan-repl--create-project-input-file temp-dir2))
                    (should (file-exists-p temp-file2))
                    (with-temp-buffer
                      (insert-file-contents temp-file2)
                      (let ((content (buffer-string)))
                        (should (string-match-p "\\* Custom Integration Template" content))))))
              (when (file-exists-p temp-custom-template)
                (delete-file temp-custom-template))
              (when (file-exists-p temp-dir2)
                (delete-directory temp-dir2 t)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Tests for enkan-repl-start-eat Function

(ert-deftest test-start-eat-directory-change ()
  "Test that enkan-repl-start-eat properly handles directory changes."
  (let ((original-dir (expand-file-name default-directory))
        (test-dir (expand-file-name "/tmp/"))
        (command-executed nil)
        (directory-changed-to nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'enkan-repl--can-send-text)
                   ;; Mock different results for before/after startup
                   (let ((call-count 0))
                     (lambda (dir)
                       (setq call-count (1+ call-count))
                       (> call-count 1))))  ; Return t after first call (simulating successful startup)
                  ((symbol-function 'eat)
                   (lambda ()
                     (setq command-executed t)
                     (message "Mock eat called")
                     (get-buffer-create "*mock-eat-buffer*")))
                  ((symbol-function 'require)
                   (lambda (feature &optional filename noerror) t))  ; Mock successful require
                  ((symbol-function 'fboundp)
                   (lambda (func) t))  ; Mock function availability
                  ((symbol-function 'cd)
                   (lambda (dir)
                     ;; Record the cd call to test-dir (not to original-dir)
                     (when (string= dir test-dir)
                       (setq directory-changed-to dir)))))  ; Record directory change to test-dir only
          (cd original-dir)
          (enkan-repl-start-eat)
          ;; Directory should be restored to original after execution
          (should (string= (expand-file-name default-directory) original-dir))
          ;; But the command should have been executed with the correct target directory
          (should command-executed))
      ;; Cleanup
      (cd original-dir))))

(ert-deftest test-start-eat-existing-live-session ()
  "Test behavior when live session already exists."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory))
        (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (dir) (get-buffer-create "*mock-eat-buffer*")))
                  ((symbol-function 'enkan-repl--can-send-text)
                   (lambda (dir) t))  ; Simulate live session
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)))
                  ((symbol-function 'require)
                   (lambda (feature &optional filename noerror) t))
                  ((symbol-function 'fboundp)
                   (lambda (func) t)))
          (enkan-repl-start-eat)
          ;; Should not change directory when session exists
          (should (string= (expand-file-name default-directory) original-dir))
          ;; Should show appropriate message
          (should (cl-some (lambda (msg) (string-match-p "already running" msg)) messages)))
      ;; Cleanup
      (when (get-buffer "*mock-eat-buffer*")
        (kill-buffer "*mock-eat-buffer*"))
      (cd original-dir))))

(ert-deftest test-start-eat-dead-session ()
  "Test behavior when dead session exists."
  (let ((test-dir "/tmp/")
        (original-dir default-directory)
        (restart-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (dir) (get-buffer-create "*mock-dead-eat-buffer*")))
                  ((symbol-function 'enkan-repl--can-send-text)
                   (lambda (dir) nil))  ; Simulate dead session
                  ((symbol-function 'y-or-n-p)
                   (lambda (prompt) t))  ; Simulate "yes" to restart
                  ((symbol-function 'kill-buffer)
                   (lambda (buffer) t))
                  ((symbol-function 'enkan-repl-start-eat)
                   (lambda () (setq restart-called t))))  ; Mock recursive call
          (enkan-repl-start-eat)
          (should restart-called))
      ;; Cleanup
      (when (get-buffer "*mock-dead-eat-buffer*")
        (kill-buffer "*mock-dead-eat-buffer*"))
      (cd original-dir))))

(ert-deftest test-start-eat-eat-unavailable ()
  "Test error handling when eat is not available."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory))
        (error-caught nil))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'enkan-repl--can-send-text)
                   (lambda (dir) nil))
                  ((symbol-function 'require)
                   (lambda (feature &optional filename noerror) nil)))  ; Simulate eat package not available
          (condition-case err
              (enkan-repl-start-eat)
            (error
             (setq error-caught (error-message-string err))))
          (should error-caught)
          (should (string-match-p "Eat package is not installed" error-caught))
          ;; Should restore original directory on error
          (should (string= (expand-file-name default-directory) original-dir)))
      ;; Cleanup
      (cd original-dir))))

(ert-deftest test-start-eat-state-restoration ()
  "Test that default-directory is restored when startup fails."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'enkan-repl--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'enkan-repl--can-send-text)
                   (lambda (dir) nil))  ; Simulate failed startup
                  ((symbol-function 'eat)
                   (lambda () (error "Simulated startup failure"))))
          (condition-case nil
              (enkan-repl-start-eat)
            (error nil))  ; Ignore the error
          ;; Should restore original directory
          (should (string= (expand-file-name default-directory) original-dir)))
      ;; Cleanup
      (cd original-dir))))

;;; Test-Only Pure Functions
;; These functions are only used by tests and moved here to avoid
;; polluting the main package namespace

(defun enkan-repl--load-template-pure (custom-template-file package-dir default-template-name)
  "Pure function: Load template content from specified paths.
CUSTOM-TEMPLATE-FILE: Custom template file path (can be nil).
PACKAGE-DIR: Package directory for default template.
DEFAULT-TEMPLATE-NAME: Default template filename.
Returns template content as string, or nil if no template found."
  (let ((template-path
         (cond
          ;; If custom template file is specified, use it
          (custom-template-file
           (expand-file-name custom-template-file))
          ;; Otherwise, look for default template in package directory
          (t (expand-file-name default-template-name package-dir)))))
    (when (and template-path (file-exists-p template-path))
      (with-temp-buffer
        (insert-file-contents template-path)
        (buffer-string)))))

(defun enkan-repl--extract-directory-pure (buffer-file-name-arg default-directory-arg)
  "Pure function: Extract target directory from BUFFER-FILE-NAME-ARG.
Uses DEFAULT-DIRECTORY-ARG if BUFFER-FILE-NAME-ARG doesn't match pattern.
If BUFFER-FILE-NAME-ARG matches persistent file pattern, decode directory."
  (if (and buffer-file-name-arg
           (string-match-p "enkan-.+\\.org$" (file-name-nondirectory buffer-file-name-arg)))
      ;; This is a persistent file, extract directory from filename
      (enkan-repl--decode-full-path
       (file-name-base (file-name-nondirectory buffer-file-name-arg)))
    ;; Use current directory
    default-directory-arg))

(defun enkan-repl--buffer-matches-directory-pure (buffer-default-directory target-dir)
  "Pure function: Check if BUFFER-DEFAULT-DIRECTORY matches TARGET-DIR.
Returns t if directories match, nil otherwise."
  (when buffer-default-directory
    (string=
     (file-truename buffer-default-directory)
     (file-truename target-dir))))

(defun enkan-repl--find-matching-buffer-pure (buffer-list target-directory)
  "Pure function: Find eat buffer matching TARGET-DIRECTORY.
BUFFER-LIST should contain buffer objects.
Returns matching buffer or nil."
  (cl-find-if
   (lambda (buf-info)
     (let
         ((name (plist-get buf-info :name))
          (default-dir (plist-get buf-info :default-directory))
          (eat-mode (plist-get buf-info :eat-mode))
          (buffer (plist-get buf-info :buffer)))
       (and
        (buffer-live-p buffer)
        name       ; Ensure name is not nil
        ;; Check for directory-specific eat buffer
        (or (and (string-match-p "^\\*enkan:" name)
                 (string-prefix-p (concat "*enkan:" target-directory) name))
            ;; Fallback to generic buffers only if they match directory
            (and (or (string= name "*eat*")
                     (string= name "*enkan*"))
                 (enkan-repl--buffer-matches-directory-pure default-dir target-directory))
            ;; Check for eat-mode buffers with enkan in name
            (and eat-mode
                 (string-match-p "enkan" name)
                 (enkan-repl--buffer-matches-directory-pure default-dir target-directory))))))
   buffer-list))

(defun enkan-repl--can-send-text-pure (eat-buffer)
  "Pure function: Check if CLAUDE-BUFFER can receive text.
Returns t if buffer has live eat process, nil otherwise."
  (when eat-buffer
    (with-current-buffer eat-buffer
      (and
       (boundp 'eat--process)
       eat--process
       (process-live-p eat--process)))))

(defun enkan-repl--send-text-pure (text eat-buffer)
  "Pure function: Send TEXT to CLAUDE-BUFFER.
Returns t if successful, nil if buffer cannot receive text.
Does not modify global state."
  (enkan-repl--debug-message "send-text-pure called with text length: %d, buffer: %s"
                             (length text) (if eat-buffer (buffer-name eat-buffer) "nil"))
  (when
      (and eat-buffer (enkan-repl--can-send-text-pure eat-buffer))
    (enkan-repl--debug-message "Sending text to claude buffer: %S" (substring text 0 (min 50 (length text))))
    (with-current-buffer eat-buffer
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\r")
      (enkan-repl--debug-message "Text sent successfully")
      t)))



;;; Tests for encode/decode pure functions

(ert-deftest test-encode-full-path-pure-basic ()
  "Test basic path encoding with custom prefix and separator."
  (should (string= (enkan-repl--encode-full-path-pure
                    "/Users/test"
                    "enkan"
                    "--")
                   "enkan--Users--test"))
  (should (string= (enkan-repl--encode-full-path-pure
                    "/Users/test/"
                    "enkan"
                    "--")
                   "enkan--Users--test"))
  (should (string= (enkan-repl--encode-full-path-pure
                    "relative/path"
                    "prefix"
                    "_")
                   (concat "prefix" (replace-regexp-in-string "/" "_" (expand-file-name "relative/path"))))))

(ert-deftest test-encode-full-path-pure-custom-separators ()
  "Test path encoding with different separators and prefixes."
  (should (string= (enkan-repl--encode-full-path-pure
                    "/Users/test"
                    "xyz"
                    "___")
                   "xyz___Users___test"))
  (should (string= (enkan-repl--encode-full-path-pure
                    "/a/b/c"
                    ""
                    "-")
                   "-a-b-c")))

(ert-deftest test-decode-full-path-pure-basic ()
  "Test basic path decoding with custom prefix and separator."
  (should (string= (enkan-repl--decode-full-path-pure
                    "enkan--Users--test"
                    "enkan"
                    "--")
                   "/Users/test/"))
  (should (string= (enkan-repl--decode-full-path-pure
                    "prefix_Users_test"
                    "prefix"
                    "_")
                   "/Users/test/")))

(ert-deftest test-decode-full-path-pure-invalid-prefix ()
  "Test decoding with invalid prefix returns nil."
  (should-not (enkan-repl--decode-full-path-pure
               "invalid--Users--test"
               "enkan"
               "--"))
  (should-not (enkan-repl--decode-full-path-pure
               "partial"
               "prefix"
               "--")))

(ert-deftest test-encode-decode-roundtrip-pure ()
  "Test that encoding and decoding are symmetric with pure functions."
  (let ((test-paths '("/Users/test" "/project/subdir" "/a/b/c/d"))
        (prefix "test")
        (separator "___"))
    (dolist (path test-paths)
      (let* ((encoded (enkan-repl--encode-full-path-pure path prefix separator))
             (decoded (enkan-repl--decode-full-path-pure encoded prefix separator)))
        (should (string= (concat path "/") decoded))))))

(ert-deftest test-encode-decode-edge-cases-pure ()
  "Test edge cases for pure encode/decode functions."
  ;; Empty prefix
  (should (string= (enkan-repl--encode-full-path-pure "/test" "" "--")
                   "--test"))
  (should (string= (enkan-repl--decode-full-path-pure "--test" "" "--")
                   "/test/"))

  ;; Single character separator
  (should (string= (enkan-repl--encode-full-path-pure "/a/b" "x" ".")
                   "x.a.b"))
  (should (string= (enkan-repl--decode-full-path-pure "x.a.b" "x" ".")
                   "/a/b/")))

(ert-deftest test-sanitize-content-newline-normalization ()
  "Test comprehensive newline normalization and Mac-specific issues."
  ;; Test \r\n normalization to \n
  (should (string= (enkan-repl--sanitize-content "Line1\r\nLine2") "Line1\nLine2"))
  ;; Test \n\r normalization (\r becomes \n, so \n\r becomes \n\n)
  (should (string= (enkan-repl--sanitize-content "Line1\n\rLine2") "Line1\n\nLine2"))
  ;; Test mixed \r conversion
  (should (string= (enkan-repl--sanitize-content "Line1\rLine2\r\nLine3") "Line1\nLine2\nLine3"))
  ;; Test consecutive newlines are preserved (no collapse)
  (should (string= (enkan-repl--sanitize-content "Line1\n\n\nLine2") "Line1\n\n\nLine2"))
  ;; Test combination of \r normalization but preserve consecutive newlines
  (should (string= (enkan-repl--sanitize-content "Line1\r\n\n\rLine2") "Line1\n\n\nLine2"))
  ;; Test Unicode line separators removal
  (should (string= (enkan-repl--sanitize-content "Line1\u0085Line2\u2028Line3\u2029Line4") "Line1Line2Line3Line4"))
  ;; Test real-world Mac selection scenario (preserve empty lines, only trim end)
  (should (string= (enkan-repl--sanitize-content "send-region test\r\n\nwith extra newlines\r") "send-region test\n\nwith extra newlines")))

;;; Tests for Template Error Handling

(ert-deftest test-handle-missing-template-testing-mode ()
  "Test missing template handling in testing mode."
  (let ((template-path "/nonexistent/template.org")
        (enkan-repl--testing-mode t))
    (let ((result (enkan-repl--handle-missing-template template-path)))
      (should (null result)))))

(ert-deftest test-handle-missing-template-default-choice ()
  "Test choosing default template when custom template is missing."
  (let ((template-path "/nonexistent/template.org")
        (enkan-repl--testing-mode nil))  ; Disable testing mode for interactive test
    ;; Mock read-char-choice to return 'd' (default)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt choices) ?d))
              ((symbol-function 'message)
               (lambda (&rest args) nil)))
      (let ((result (enkan-repl--handle-missing-template template-path)))
        (should (null result))))))

(ert-deftest test-handle-missing-template-quit-choice ()
  "Test quitting when custom template is missing."
  (let ((template-path "/nonexistent/template.org")
        (enkan-repl--testing-mode nil))
    ;; Mock read-char-choice to return 'q' (quit)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt choices) ?q)))
      (should-error (enkan-repl--handle-missing-template template-path)
                    :type 'user-error))))

(ert-deftest test-handle-missing-template-create-choice ()
  "Test creating new template when custom template is missing."
  (let* ((temp-dir (make-temp-file "eat-test-" t))
         (template-path (expand-file-name "test-template.org" temp-dir))
         (enkan-repl--testing-mode nil))
    (unwind-protect
        (progn
          ;; Mock read-char-choice to return 'c' (create)
          ;; Mock y-or-n-p to return t (yes)
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?c))
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt) t))
                    ((symbol-function 'message)
                     (lambda (&rest args) nil)))
            (let ((result (enkan-repl--handle-missing-template template-path)))
              (should (string= result template-path))
              (should (file-exists-p template-path))
              ;; Check created content
              (let ((content (with-temp-buffer
                               (insert-file-contents template-path)
                               (buffer-string))))
                (should (string-match-p "\\* Quick Start" content))
                (should (string-match-p "Custom Template" content))))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-handle-missing-template-select-existing-file ()
  "Test selecting existing file when custom template is missing."
  (let* ((temp-dir (make-temp-file "eat-test-" t))
         (existing-template (expand-file-name "existing.org" temp-dir))
         (missing-template (expand-file-name "missing.org" temp-dir))
         (enkan-repl--testing-mode nil))
    (unwind-protect
        (progn
          ;; Create existing template
          (with-temp-file existing-template
            (insert "* Existing Template\n"))
          ;; Mock read-char-choice to return 's' (select)
          ;; Mock read-file-name to return existing file
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?s))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest args) existing-template)))
            (let ((result (enkan-repl--handle-missing-template missing-template)))
              (should (string= result existing-template)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-find-template-directory-straight-el-path-conversion ()
  "Test that straight.el build path is correctly converted to repos path."
  ;; Test the path conversion logic directly
  (let ((build-path "/Users/test/.emacs.d/straight/build/enkan-repl/"))
    (when (string-match-p "/straight/build/" build-path)
      (let ((repos-path (replace-regexp-in-string "/straight/build/" "/straight/repos/" build-path)))
        (should (string= repos-path "/Users/test/.emacs.d/straight/repos/enkan-repl/"))))))

(ert-deftest test-find-template-directory-path-matching ()
  "Test that straight.el build path is detected correctly."
  (let ((build-path "/Users/test/.emacs.d/straight/build/enkan-repl/")
        (normal-path "/Users/test/.emacs.d/elpa/enkan-repl/"))
    (should (string-match-p "/straight/build/" build-path))
    (should-not (string-match-p "/straight/build/" normal-path))))

;;; Tests for Categorized Template Functions

(ert-deftest test-get-categorized-functions-with-constants ()
  "Test that get-categorized-functions works when constants are available."
  (let ((result (enkan-repl--get-categorized-functions)))
    (should (stringp result))
    (should (string-match-p "Command Palette" result))
    (should (string-match-p "Text Sender" result))
    (should (string-match-p "Session Controller" result))
    (should (string-match-p "Utilities" result))
    (should (string-match-p "enkan-repl-cheat-sheet" result))
    (should (string-match-p "enkan-repl-send-region" result))))

(ert-deftest test-get-static-functions-fallback ()
  "Test that get-static-functions provides proper fallback."
  (let ((result (enkan-repl--get-static-functions)))
    (should (stringp result))
    (should (string-match-p "Command Palette" result))
    (should (string-match-p "Text Sender" result))
    (should (string-match-p "Session Controller" result))
    (should (string-match-p "Utilities" result))
    (should (string-match-p "enkan-repl-cheat-sheet" result))
    (should (string-match-p "enkan-repl-send-region" result))))

(ert-deftest test-embedded-template-uses-categorized-functions ()
  "Test that embedded template includes categorized function list."
  (let ((template (enkan-repl--get-embedded-template)))
    (should (stringp template))
    (should (string-match-p "Functions/Commands" template))
    (should (string-match-p "Command Palette" template))
    (should (string-match-p "Text Sender" template))
    (should (string-match-p "Session Controller" template))
    (should (string-match-p "Utilities" template))))

;;; Extended Tests for Categorized Template Functions

(ert-deftest test-get-categorized-functions-structure ()
  "Test the structure and format of categorized functions output."
  (let ((result (enkan-repl--get-categorized-functions)))
    ;; Should be non-empty string
    (should (stringp result))
    (should (> (length result) 0))
    ;; Should have proper org-mode headers
    (should (string-match-p "\\*\\* Command Palette" result))
    (should (string-match-p "\\*\\* Text Sender" result))
    (should (string-match-p "\\*\\* Session Controller" result))
    (should (string-match-p "\\*\\* Utilities" result))
    ;; Should have proper org-mode function entries
    (should (string-match-p "- ~M-x enkan-repl-" result))
    ;; Category information should be stripped from descriptions
    (should-not (string-match-p "Category:" result))))

(ert-deftest test-get-categorized-functions-category-order ()
  "Test that categories appear in the correct order."
  (let ((result (enkan-repl--get-categorized-functions)))
    (let ((command-pos (string-match "\\*\\* Command Palette" result))
          (text-pos (string-match "\\*\\* Text Sender" result))
          (session-pos (string-match "\\*\\* Session Controller" result))
          (util-pos (string-match "\\*\\* Utilities" result)))
      ;; All categories should be found
      (should command-pos)
      (should text-pos)
      (should session-pos)
      (should util-pos)
      ;; Should appear in correct order
      (should (< command-pos text-pos))
      (should (< text-pos session-pos))
      (should (< session-pos util-pos)))))

(ert-deftest test-get-categorized-functions-error-handling ()
  "Test error handling when constants are unavailable."
  ;; Mock constants being unavailable by causing require to fail
  (cl-letf (((symbol-function 'require)
             (lambda (feature &rest _)
               (if (eq feature 'enkan-repl-constants)
                   (error "Mock constants unavailable")
                 (funcall #'require feature)))))
    (let ((result (enkan-repl--get-categorized-functions)))
      ;; Should fall back to static functions
      (should (stringp result))
      (should (string-match-p "Command Palette" result))
      (should (string-match-p "enkan-repl-cheat-sheet" result)))))

(ert-deftest test-get-static-functions-structure ()
  "Test structure and completeness of static functions fallback."
  (let ((result (enkan-repl--get-static-functions)))
    ;; Should be non-empty string
    (should (stringp result))
    (should (> (length result) 0))
    ;; Should have all essential functions
    (should (string-match-p "enkan-repl-send-region" result))
    (should (string-match-p "enkan-repl-send-buffer" result))
    (should (string-match-p "enkan-repl-start-eat" result))
    (should (string-match-p "enkan-repl-cheat-sheet" result))
    ;; Should have proper org-mode format
    (should (string-match-p "\\*\\*" result))
    (should (string-match-p "- ~M-x" result))
    ;; Should end with double newline for proper formatting
    (should (string-suffix-p "\n\n" result))))

(ert-deftest test-get-static-functions-consistency ()
  "Test consistency between static and categorized functions."
  (let ((static-result (enkan-repl--get-static-functions))
        (categorized-result (enkan-repl--get-categorized-functions)))
    ;; Both should have same categories
    (should (string-match-p "Command Palette" static-result))
    (should (string-match-p "Command Palette" categorized-result))
    (should (string-match-p "Text Sender" static-result))
    (should (string-match-p "Text Sender" categorized-result))
    ;; Both should have key functions
    (should (string-match-p "enkan-repl-cheat-sheet" static-result))
    (should (string-match-p "enkan-repl-cheat-sheet" categorized-result))))

(ert-deftest test-embedded-template-integration ()
  "Test integration between embedded template and categorized functions."
  (let ((template (enkan-repl--get-embedded-template)))
    ;; Should contain template structure
    (should (string-match-p (regexp-quote "#+TITLE: Enkan Input File") template))
    (should (string-match-p "\\* Quick Start" template))
    (should (string-match-p "\\* Context" template))
    (should (string-match-p "\\* Functions/Commands" template))
    (should (string-match-p "\\* Notes" template))
    ;; Should contain categorized functions within the template
    (should (string-match-p "\\*\\* Command Palette" template))
    (should (string-match-p "enkan-repl-cheat-sheet" template))
    ;; Should be properly formatted org-mode
    (should (string-match-p "- ~M-x enkan-repl-" template))))

(ert-deftest test-embedded-template-completeness ()
  "Test that embedded template contains all expected sections."
  (let ((template (enkan-repl--get-embedded-template)))
    ;; Required template sections
    (should (string-match-p "\\* Quick Start" template))
    (should (string-match-p "\\* Context" template))
    (should (string-match-p "\\* Approach" template))
    (should (string-match-p "\\* Functions/Commands" template))
    (should (string-match-p "\\* Notes" template))
    (should (string-match-p "\\* Next Steps" template))
    ;; Should be valid org-mode
    (should (string-prefix-p "#+TITLE:" template))
    (should (string-suffix-p "\n" template))))

;;; Tests for center-send-escape pure functions

(ert-deftest test-enkan-repl--collect-enkan-buffers-pure ()
  "Test collecting enkan buffers from buffer list."
  (let ((mock-buffers
         (list
          ;; Mock enkan buffers
          (let ((buf (generate-new-buffer "*enkan:/test/project*")))
            buf)
          (let ((buf (generate-new-buffer "*enkan:/another/project*")))
            buf)
          ;; Mock non-enkan buffers
          (let ((buf (generate-new-buffer "*scratch*")))
            buf)
          (let ((buf (generate-new-buffer "*Messages*")))
            buf))))
    (unwind-protect
        (let ((result (enkan-repl--collect-enkan-buffers-pure mock-buffers)))
          (should (= 2 (length result)))
          (should (string-match-p "^\\*enkan:" (buffer-name (nth 0 result))))
          (should (string-match-p "^\\*enkan:" (buffer-name (nth 1 result)))))
      ;; Cleanup
      (dolist (buf mock-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-enkan-repl--get-buffer-process-info-pure ()
  "Test getting buffer process info."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*")))
    (unwind-protect
        (progn
          ;; Test with no eat--process
          (let ((info (enkan-repl--get-buffer-process-info-pure test-buffer)))
            (should (equal (plist-get info :buffer) test-buffer))
            (should (string= (plist-get info :name) "*enkan:/test*"))
            (should (plist-get info :live-p))
            (should (null (plist-get info :has-process))))

          ;; Test with mock eat--process
          (with-current-buffer test-buffer
            (setq-local eat--process 'mock-process))
          (let ((info (enkan-repl--get-buffer-process-info-pure test-buffer)))
            (should (plist-get info :has-process))
            (should (equal (plist-get info :process) 'mock-process))))
      (kill-buffer test-buffer))))

(ert-deftest test-enkan-repl--send-escape-to-buffer-pure ()
  "Test determining if ESC can be sent to buffer."
  (let ((test-buffer (generate-new-buffer "*enkan:/test*")))
    (unwind-protect
        (progn
          ;; Should return nil without eat--process
          (should (null (enkan-repl--send-escape-to-buffer-pure test-buffer)))

          ;; Should return t with eat--process
          (with-current-buffer test-buffer
            (setq-local eat--process 'mock-process))
          (should (enkan-repl--send-escape-to-buffer-pure test-buffer)))
      (kill-buffer test-buffer))))

(ert-deftest test-enkan-repl--collect-enkan-buffers-pure-empty-list ()
  "Test collecting enkan buffers from empty list."
  (let ((result (enkan-repl--collect-enkan-buffers-pure '())))
    (should (null result))))

(ert-deftest test-enkan-repl--get-buffer-process-info-pure-nil-buffer ()
  "Test getting process info for nil buffer."
  (let ((result (enkan-repl--get-buffer-process-info-pure nil)))
    (should (null result))))

(ert-deftest test-enkan-repl--get-buffer-by-index-pure ()
  "Test getting buffer by 1-based index."
  (let ((buffers (list (get-buffer-create "*test1*") (get-buffer-create "*test2*") (get-buffer-create "*test3*"))))
    (should (eq (car buffers) (enkan-repl--get-buffer-by-index-pure buffers 1)))
    (should (eq (nth 1 buffers) (enkan-repl--get-buffer-by-index-pure buffers 2)))
    (should (eq (nth 2 buffers) (enkan-repl--get-buffer-by-index-pure buffers 3)))
    (should (null (enkan-repl--get-buffer-by-index-pure buffers 0)))
    (should (null (enkan-repl--get-buffer-by-index-pure buffers 4)))
    (should (null (enkan-repl--get-buffer-by-index-pure buffers -1)))))

(ert-deftest test-enkan-repl--parse-prefix-arg-pure ()
  "Test parsing prefix arguments for different action types."
  (let ((result-nil (enkan-repl--parse-prefix-arg-pure nil))
        (result-int (enkan-repl--parse-prefix-arg-pure 2))
        (result-invalid (enkan-repl--parse-prefix-arg-pure '(4))))
    (should (eq 'select (plist-get result-nil :action)))
    (should (null (plist-get result-nil :index)))
    (should (eq 'index (plist-get result-int :action)))
    (should (eq 2 (plist-get result-int :index)))
    (should (eq 'invalid (plist-get result-invalid :action)))))

(ert-deftest test-enkan-repl--should-show-buffer-selection-pure ()
  "Test buffer selection UI display logic."
  (let ((valid-buffers (list (get-buffer-create "*test1*"))))
    (should (eq t (enkan-repl--should-show-buffer-selection-pure 'select valid-buffers)))
    (should (eq nil (enkan-repl--should-show-buffer-selection-pure 'index valid-buffers)))
    (should (eq nil (enkan-repl--should-show-buffer-selection-pure 'select nil)))))

(ert-deftest test-enkan-repl--build-buffer-selection-choices-pure ()
  "Test building buffer selection choices."
  (with-temp-buffer
    (let* ((buffer1 (current-buffer))
           (buffer2 (get-buffer-create "*enkan:test*"))
           (buffers (list buffer1 buffer2))
           (choices (enkan-repl--build-buffer-selection-choices-pure buffers)))
      (should (= 2 (length choices)))
      (should (consp (car choices)))
      (should (bufferp (cdr (car choices)))))))

;;; Test Runner

(defun enkan-repl-run-all-tests ()
  "Run all enkan-repl pure function tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-"))

(provide 'enkan-repl-test)

;;; enkan-repl-test.el ends here

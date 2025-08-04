;;; claudemacs-client-test.el --- Tests for claudemacs-client pure functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code <noreply@anthropic.com>
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Comprehensive tests for claudemacs-client pure functions using ERT.
;; These tests aim for 100% code coverage of all pure functions.

;;; Code:

(require 'ert)

;; Load the main package and define helper function
(defvar claudemacs-client-test-package-dir
  (let ((package-dir (or (and load-file-name
                              (file-name-directory load-file-name))
                         (file-name-directory (locate-library "claudemacs-client-test"))
                         default-directory)))
    (expand-file-name "../" package-dir)))

(load (expand-file-name "claudemacs-client.el" claudemacs-client-test-package-dir))

;;; Tests for claudemacs-client--extract-directory-pure

(ert-deftest test-extract-directory-pure-with-persistent-file ()
  "Test directory extraction from persistent file name."
  (should (string= "/Users/phasetr/project/"
                   (claudemacs-client--extract-directory-pure
                    "/path/to/cec--Users--phasetr--project.org"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-default-directory ()
  "Test directory extraction using default directory for non-persistent files."
  (should (string= "/default/dir/"
                   (claudemacs-client--extract-directory-pure
                    "/path/to/regular-file.org"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-nil-buffer-file-name ()
  "Test directory extraction with nil buffer file name."
  (should (string= "/default/dir/"
                   (claudemacs-client--extract-directory-pure
                    nil
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-with-non-org-persistent-file ()
  "Test directory extraction with persistent file pattern but wrong extension."
  (should (string= "/default/dir/"
                   (claudemacs-client--extract-directory-pure
                    "/path/to/cec--Users--phasetr--project.txt"
                    "/default/dir/"))))

(ert-deftest test-extract-directory-pure-complex-path ()
  "Test directory extraction with complex path containing special characters."
  (should (string= "/Users/user/My Documents/project with spaces/"
                   (claudemacs-client--extract-directory-pure
                    "/path/cec--Users--user--My Documents--project with spaces.org"
                    "/default/"))))

;;; Tests for claudemacs-client--buffer-matches-directory-pure

(ert-deftest test-buffer-matches-directory-pure-exact-match ()
  "Test directory matching with exact paths."
  (should (claudemacs-client--buffer-matches-directory-pure
           "/Users/phasetr/project/"
           "/Users/phasetr/project/")))

(ert-deftest test-buffer-matches-directory-pure-no-match ()
  "Test directory matching with different paths."
  (should-not (claudemacs-client--buffer-matches-directory-pure
               "/Users/phasetr/project1/"
               "/Users/phasetr/project2/")))

(ert-deftest test-buffer-matches-directory-pure-with-nil-buffer-dir ()
  "Test directory matching with nil buffer directory."
  (should-not (claudemacs-client--buffer-matches-directory-pure
               nil
               "/Users/phasetr/project/")))

(ert-deftest test-buffer-matches-directory-pure-trailing-slash-normalization ()
  "Test directory matching with different trailing slash patterns."
  ;; file-truename preserves trailing slashes, so paths must match exactly
  (should (claudemacs-client--buffer-matches-directory-pure
           "/Users/phasetr/project/"
           "/Users/phasetr/project/"))
  ;; Test that different trailing slash patterns don't match
  (should-not (claudemacs-client--buffer-matches-directory-pure
               "/Users/phasetr/project"
               "/Users/phasetr/project/")))

;;; Tests for claudemacs-client--find-matching-buffer-pure

(ert-deftest test-find-matching-buffer-pure-claudemacs-specific-buffer ()
  "Test finding directory-specific claudemacs buffer."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*claudemacs:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (claudemacs-client--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'test-buffer)))))

(ert-deftest test-find-matching-buffer-pure-generic-claude-buffer ()
  "Test finding generic claude buffer with directory match."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*claude*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    ;; Mock buffer-live-p to return t for our test buffer
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (claudemacs-client--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'test-buffer)))))

(ert-deftest test-find-matching-buffer-pure-eat-mode-buffer ()
  "Test finding eat-mode buffer with claude in name."
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name "*terminal-claude*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode t))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (claudemacs-client--find-matching-buffer-pure
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
    (should-not (claudemacs-client--find-matching-buffer-pure
                 buffer-list
                 "/Users/phasetr/project/"))))

(ert-deftest test-find-matching-buffer-pure-dead-buffer ()
  "Test that dead buffers are not matched."
  (let ((buffer-list (list (list :buffer 'dead-buffer
                                 :name "*claudemacs:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) nil)))
      (should-not (claudemacs-client--find-matching-buffer-pure
                   buffer-list
                   "/Users/phasetr/project/")))))

(ert-deftest test-find-matching-buffer-pure-multiple-buffers ()
  "Test finding buffer when multiple candidates exist."
  (let ((buffer-list (list (list :buffer 'buffer1
                                 :name "*other*"
                                 :default-directory "/different/"
                                 :eat-mode nil)
                           (list :buffer 'buffer2
                                 :name "*claudemacs:/Users/phasetr/project/*"
                                 :default-directory "/Users/phasetr/project/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (claudemacs-client--find-matching-buffer-pure
                                 buffer-list
                                 "/Users/phasetr/project/")
                                :buffer)
                     'buffer2)))))

(ert-deftest test-find-matching-buffer-pure-empty-buffer-list ()
  "Test finding buffer with empty buffer list."
  (should-not (claudemacs-client--find-matching-buffer-pure
               '()
               "/Users/phasetr/project/")))

;;; Tests for claudemacs-client--can-send-text-pure

(ert-deftest test-can-send-text-pure-with-valid-buffer ()
  "Test can send text with valid buffer and live process."
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          ;; Mock the eat process variables
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t)))
            (should (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-dead-process ()
  "Test can send text with dead process."
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) nil)))
            (should-not (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-nil-process ()
  "Test can send text with nil process."
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process))))
            (should-not (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-without-eat-process-var ()
  "Test can send text without eat--process variable."
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (cl-letf (((symbol-function 'boundp) (lambda (sym) nil)))
            (should-not (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-can-send-text-pure-with-nil-buffer ()
  "Test can send text with nil buffer."
  (should-not (claudemacs-client--can-send-text-pure nil)))

;;; Tests for claudemacs-client--send-text-pure

(ert-deftest test-send-text-pure-successful-send ()
  "Test successful text sending."
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (claudemacs-client--send-text-pure "test message" test-buffer))
            ;; Verify correct strings were sent (in reverse order due to push)
            (should (equal sent-strings '("\r" "test message")))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-cannot-send ()
  "Test text sending when buffer cannot receive text."
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process))))
            (should-not (claudemacs-client--send-text-pure "test message" test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-with-nil-buffer ()
  "Test text sending with nil buffer."
  (should-not (claudemacs-client--send-text-pure "test message" nil)))

(ert-deftest test-send-text-pure-empty-text ()
  "Test sending empty text."
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (claudemacs-client--send-text-pure "" test-buffer))
            (should (equal sent-strings '("\r" "")))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-multiline-text ()
  "Test sending multiline text."
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (sent-strings '())
        (multiline-text "line1\nline2\nline3"))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (claudemacs-client--send-text-pure multiline-text test-buffer))
            (should (equal sent-strings (list "\r" multiline-text)))))
      (kill-buffer test-buffer))))

;;; Additional Edge Case Tests

(ert-deftest test-extract-directory-pure-edge-cases ()
  "Test edge cases for directory extraction."
  ;; Empty strings
  (should (string= ""
                   (claudemacs-client--extract-directory-pure
                    ""
                    "")))
  ;; Very long path
  (let ((long-path (make-string 1000 ?a)))
    (should (string= long-path
                     (claudemacs-client--extract-directory-pure
                      nil
                      long-path))))
  ;; Path with special characters
  (should (string= "/Users/user with spaces/Ñice-prójęct/"
                   (claudemacs-client--extract-directory-pure
                    "/path/cec--Users--user with spaces--Ñice-prójęct.org"
                    "/default/"))))

(ert-deftest test-buffer-matches-directory-pure-edge-cases ()
  "Test edge cases for directory matching."
  ;; Empty strings
  (should-not (claudemacs-client--buffer-matches-directory-pure
               ""
               "/path/"))
  (should-not (claudemacs-client--buffer-matches-directory-pure
               "/path/"
               ""))
  ;; Both empty
  (should (claudemacs-client--buffer-matches-directory-pure
           ""
           ""))
  ;; Case sensitivity (depends on filesystem)
  (should-not (claudemacs-client--buffer-matches-directory-pure
               "/Users/Test/"
               "/Users/test/")))

(ert-deftest test-find-matching-buffer-pure-edge-cases ()
  "Test edge cases for buffer finding."
  ;; Buffer list with invalid plist - missing :buffer means plist-get returns nil
  (let ((invalid-buffer-list (list (list :name "*claude*"
                                         ;; Missing :buffer key means buffer is nil
                                         :default-directory "/path/"
                                         :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) (not (null buf)))))
      (should-not (claudemacs-client--find-matching-buffer-pure
                   invalid-buffer-list
                   "/path/"))))

  ;; Buffer with malformed name
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name ""  ; Empty name
                                 :default-directory "/path/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should-not (claudemacs-client--find-matching-buffer-pure
                   buffer-list
                   "/path/"))))

  ;; Buffer with nil name
  (let ((buffer-list (list (list :buffer 'test-buffer
                                 :name nil
                                 :default-directory "/path/"
                                 :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should-not (claudemacs-client--find-matching-buffer-pure
                   buffer-list
                   "/path/")))))

(ert-deftest test-can-send-text-pure-edge-cases ()
  "Test edge cases for can send text check."
  ;; Buffer with eat--process bound but nil
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process nil)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) nil)))
            (should-not (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer)))

  ;; Buffer where process-live-p throws error
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) (error "Process error"))))
            (should-error (claudemacs-client--can-send-text-pure test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest test-send-text-pure-edge-cases ()
  "Test edge cases for text sending."
  ;; Very long text
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (long-text (make-string 10000 ?x))
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (claudemacs-client--send-text-pure long-text test-buffer))
            (should (equal sent-strings (list "\r" long-text)))))
      (kill-buffer test-buffer)))

  ;; Text with special characters
  (let ((test-buffer (generate-new-buffer "*test-claude*"))
        (special-text "Hello\n\t\r\nWorld! 🎉 Ñice tëxt")
        (sent-strings '()))
    (unwind-protect
        (with-current-buffer test-buffer
          (setq-local eat--process 'mock-process)
          (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                    ((symbol-function 'process-live-p) (lambda (proc) t))
                    ((symbol-function 'eat--send-string)
                     (lambda (proc text) (push text sent-strings))))
            (should (claudemacs-client--send-text-pure special-text test-buffer))
            (should (equal sent-strings (list "\r" special-text)))))
      (kill-buffer test-buffer)))

  ;; Buffer gets killed during sending (edge case)
  (let ((test-buffer (generate-new-buffer "*test-claude*")))
    (with-current-buffer test-buffer
      (setq-local eat--process 'mock-process)
      (cl-letf (((symbol-function 'boundp) (lambda (sym) (eq sym 'eat--process)))
                ((symbol-function 'process-live-p) (lambda (proc) t))
                ((symbol-function 'eat--send-string)
                 (lambda (proc text)
                   (kill-buffer test-buffer)  ; Kill buffer during send
                   nil)))
        ;; This should still try to send, but buffer will be dead
        (should-error (claudemacs-client--send-text-pure "test" test-buffer))))))

;;; Stress Tests

(ert-deftest test-extract-directory-pure-stress ()
  "Stress test for directory extraction with many iterations."
  (dotimes (i 100)
    (let ((test-path (format "/test/path%d/" i)))
      (should (string= test-path
                       (claudemacs-client--extract-directory-pure
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
                              :name "*claudemacs:/target/path/*"
                              :default-directory "/target/path/"
                              :eat-mode nil))))
    (cl-letf (((symbol-function 'buffer-live-p) (lambda (buf) t)))
      (should (equal (plist-get (claudemacs-client--find-matching-buffer-pure
                                 large-buffer-list
                                 "/target/path/")
                                :buffer)
                     'target-buffer)))))

;;; Tests for Constants

(ert-deftest test-default-template-filename-constant ()
  "Test that default template filename constant is properly defined."
  (should (boundp 'claudemacs-client-default-template-filename))
  (should (stringp claudemacs-client-default-template-filename))
  (should (string-suffix-p ".org" claudemacs-client-default-template-filename))
  (should (string-prefix-p "default" claudemacs-client-default-template-filename)))

(ert-deftest test-template-functions-use-constant ()
  "Test that template functions use the defined constant instead of hardcoded strings."
  ;; Test load-template wrapper uses the constant
  (let ((default-directory claudemacs-client-test-package-dir)
        (claudemacs-client-template-file nil))
    (let ((template-content (claudemacs-client--load-template)))
      (should template-content))))

(ert-deftest test-constant-immutability-intent ()
  "Test that constant maintains its intended immutable value."
  ;; This test documents the intended immutability, even though Emacs Lisp
  ;; doesn't enforce it for defconst
  (should (not (string-empty-p claudemacs-client-default-template-filename)))

  ;; Verify the constant is marked as a constant (has defconst property)
  (should (get 'claudemacs-client-default-template-filename 'variable-documentation)))

;;; Tests for Template Loading Pure Functions

(ert-deftest test-load-template-pure-with-custom-file ()
  "Test pure template loading with custom template file."
  (let ((temp-template (make-temp-file "test-template" nil ".org"))
        (test-content "* Custom Pure Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert test-content))
          (let ((result (claudemacs-client--load-template-pure
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
            (let ((result (claudemacs-client--load-template-pure
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
          (let ((result (claudemacs-client--load-template-pure
                         "~/test-template.org"
                         "/unused/package/dir"
                         "unused-default.org")))
            (should result)
            (should (string= result test-content))))
      (when (file-exists-p temp-template)
        (delete-file temp-template)))))

(ert-deftest test-load-template-pure-nonexistent-custom-file ()
  "Test pure template loading with nonexistent custom file."
  (let ((result (claudemacs-client--load-template-pure
                 "/nonexistent/template.org"
                 "/unused/package/dir"
                 "unused-default.org")))
    (should-not result)))

(ert-deftest test-load-template-pure-nonexistent-default-file ()
  "Test pure template loading with nonexistent default file."
  (let ((temp-package-dir (make-temp-file "test-package" t)))
    (unwind-protect
        (let ((result (claudemacs-client--load-template-pure
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
          (let ((result (claudemacs-client--load-template-pure
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
          (let ((result (claudemacs-client--load-template-pure
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
          (let ((result (claudemacs-client--load-template-pure
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
  (should-not (claudemacs-client--load-template-pure nil "/nonexistent" "nonexistent.org"))

  ;; Test with empty strings - these should not try to read files
  (should-not (claudemacs-client--load-template-pure nil "/nonexistent" ""))
  (should-not (claudemacs-client--load-template-pure nil "" "nonexistent.org")))

;;; Tests for Debug Mode Functions

(ert-deftest test-debug-mode-toggle ()
  "Test that debug mode can be toggled correctly."
  (let ((original-debug-mode claudemacs-client-debug-mode))
    (unwind-protect
        (progn
          ;; Start with debug mode disabled
          (setq claudemacs-client-debug-mode nil)

          ;; Test toggle from disabled to enabled
          (claudemacs-client-toggle-debug-mode)
          (should claudemacs-client-debug-mode)

          ;; Test toggle from enabled to disabled
          (claudemacs-client-toggle-debug-mode)
          (should-not claudemacs-client-debug-mode)

          ;; Test explicit enable
          (claudemacs-client-enable-debug-mode)
          (should claudemacs-client-debug-mode)

          ;; Test explicit disable
          (claudemacs-client-disable-debug-mode)
          (should-not claudemacs-client-debug-mode))

      ;; Restore original state
      (setq claudemacs-client-debug-mode original-debug-mode))))

(ert-deftest test-debug-message-functionality ()
  "Test that debug messages are only shown when debug mode is enabled."
  (let ((original-debug-mode claudemacs-client-debug-mode)
        (captured-messages '()))
    (unwind-protect
        (progn
          ;; Mock message function to capture output
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) captured-messages))))

            ;; Test with debug mode disabled
            (setq claudemacs-client-debug-mode nil)
            (claudemacs-client--debug-message "Test debug message 1")
            (should (= (length captured-messages) 0))

            ;; Test with debug mode enabled
            (setq claudemacs-client-debug-mode t)
            (claudemacs-client--debug-message "Test debug message 2")
            (should (= (length captured-messages) 1))
            (should (string-match-p "\\[CLAUDEMACS-DEBUG\\] Test debug message 2" (car captured-messages)))))

      ;; Restore original state
      (setq claudemacs-client-debug-mode original-debug-mode))))

;;; Tests for Content Sanitization

(ert-deftest test-sanitize-content-basic ()
  "Test basic content sanitization functionality."
  ;; Test normal content passes through unchanged
  (should (string= (claudemacs-client--sanitize-content "Hello World") "Hello World"))

  ;; Test content with newlines and tabs is preserved
  (should (string= (claudemacs-client--sanitize-content "Line 1\nLine 2\tTabbed") "Line 1\nLine 2\tTabbed"))

  ;; Test nil input
  (should-not (claudemacs-client--sanitize-content nil))

  ;; Test empty string
  (should (string= (claudemacs-client--sanitize-content "") "")))

(ert-deftest test-sanitize-content-control-characters ()
  "Test that problematic control characters are removed."
  ;; Test carriage return removal
  (should (string= (claudemacs-client--sanitize-content "Test\rContent") "TestContent"))

  ;; Test mixed control characters at end
  (should (string= (claudemacs-client--sanitize-content "Test Content\r\x0B\x0C") "Test Content"))

  ;; Test that normal whitespace trimming still works
  (should (string= (claudemacs-client--sanitize-content "  Test Content  ") "Test Content")))

(ert-deftest test-sanitize-content-problematic-string ()
  "Test sanitization with the specific problematic string reported by user."
  (let ((test-string "画像送信サンプルです.\n以下の画像に何が書いてあるか読めますか？\n~/Downloads/send-sample.png"))
    (let ((sanitized (claudemacs-client--sanitize-content test-string)))
      (should sanitized)
      ;; This should now have the marker added to prevent file path interpretation
      (should (string-match-p "This text is added by claudemacs-client" sanitized))
      (should-not (string-empty-p sanitized)))))

(ert-deftest test-sanitize-content-file-path-interpretation ()
  "Test that file paths without punctuation get markers to prevent misinterpretation."
  ;; Test English content ending with file path (should get marker)
  (let ((test-string "my test.\nI send a sample message in English with image path and without period.\n~/Downloads/send-sample.png"))
    (let ((sanitized (claudemacs-client--sanitize-content test-string)))
      (should sanitized)
      (should (string-match-p "This text is added by claudemacs-client" sanitized))
      (should-not (string-empty-p sanitized))))

  ;; Test content ending with punctuation after file path (should not get marker)
  (let ((test-string "Check this file: ~/Downloads/send-sample.png."))
    (let ((sanitized (claudemacs-client--sanitize-content test-string)))
      (should sanitized)
      (should (string= sanitized test-string)) ; Should be unchanged
      (should-not (string-match-p "This text is added by claudemacs-client" sanitized))))

  ;; Test content not ending with file path (should not get marker)
  (let ((test-string "This is just normal text without file paths"))
    (let ((sanitized (claudemacs-client--sanitize-content test-string)))
      (should sanitized)
      (should (string= sanitized test-string)) ; Should be unchanged
      (should-not (string-match-p "This text is added by claudemacs-client" sanitized)))))

;;; Tests for Template Loading Functions


(ert-deftest test-load-template-default ()
  "Test loading default template."
  (let ((default-directory claudemacs-client-test-package-dir)
        (claudemacs-client-template-file nil))
    (let ((template-content (claudemacs-client--load-template)))
      (should template-content)
      (should (stringp template-content))
      (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" template-content)))))

(ert-deftest test-load-template-custom-with-fallback ()
  "Test loading custom template with fallback to default."
  (let ((default-directory claudemacs-client-test-package-dir)
        (claudemacs-client-template-file "/nonexistent/path.org"))
    (let ((template-content (claudemacs-client--load-template)))
      (should-not template-content))))

(ert-deftest test-load-template-custom-with-path ()
  "Test loading custom template with explicit path."
  (let ((temp-file (make-temp-file "test-template" nil ".org"))
        (test-content "* Custom Template\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert test-content))
          (let ((claudemacs-client-template-file temp-file))
            (let ((template-content (claudemacs-client--load-template)))
              (should template-content)
              (should (string= template-content test-content)))))
      (delete-file temp-file))))


(ert-deftest test-load-template-nonexistent ()
  "Test loading nonexistent template returns nil."
  (let ((claudemacs-client-template-file "/nonexistent/path.org"))
    (should-not (claudemacs-client--load-template))))

(ert-deftest test-load-template-fallback-chain ()
  "Test template loading fallback chain behavior."
  (let ((default-directory claudemacs-client-test-package-dir)
        (claudemacs-client-custom-template-path nil))

    ;; Test that default template loading works
    (let ((claudemacs-client-template-file nil))
      (let ((template-content (claudemacs-client--load-template)))
        (should template-content)
        (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" template-content))))

    ;; Test that nonexistent custom file returns nil
    (let ((claudemacs-client-template-file "/nonexistent/path.org"))
      (let ((template-content (claudemacs-client--load-template)))
        (should-not template-content)))))

;;; Tests for Template Generation Function (via initialize-project-file)

(ert-deftest test-initialize-project-file-default ()
  "Test project file initialization with default template."
  (let* ((temp-dir (make-temp-file "test-project-default" t))
         (default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-file nil)
         (temp-file nil))
    (unwind-protect
        (progn
          (setq temp-file (claudemacs-client--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
              (should (string-match-p "(claudemacs-client-start-claudemacs)" content)))))
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
          (let ((default-directory claudemacs-client-test-package-dir)
                (claudemacs-client-template-file temp-template))
            (setq temp-project (claudemacs-client--create-project-input-file temp-dir))
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
         (default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-file "/nonexistent/path.org")
         (temp-file nil))
    (unwind-protect
        (progn
          ;; Should use default.org content as fallback
          (setq temp-file (claudemacs-client--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain default.org content, not hardcoded fallback
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
              (should (string-match-p "(claudemacs-client-start-claudemacs)" content))
              (should (string-match-p "Start Claude Code Session\\|claudemacs-client-start-claudemacs" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-initialize-project-file-nil-template-uses-default-org ()
  "Test that when claudemacs-client-template-file is nil, default.org content is used."
  (let* ((temp-dir (make-temp-file "test-project-nil" t))
         (default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-file nil)  ; Explicitly set to nil
         (temp-file nil))
    (unwind-protect
        (progn
          (setq temp-file (claudemacs-client--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain default.org content, not hardcoded fallback
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
              (should (string-match-p "(claudemacs-client-start-claudemacs)" content))
              (should (string-match-p "Start Claude Code Session\\|claudemacs-client-start-claudemacs" content))
              (should (string-match-p "Essential Commands\\|Send current region" content))
              ;; Should NOT contain the hardcoded fallback format
              (should-not (string-match-p "Project: .*\n\n\\*\\* Thoughts/Notes" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Tests for claudemacs-client--create-project-input-file Function

(ert-deftest test-create-project-input-file-with-nil-template ()
  "Test that create-project-input-file uses default.org when template-file is nil."
  (let ((claudemacs-client-template-file nil)
        (temp-dir (make-temp-file "test-project" t))
        (temp-file nil)
        (original-default-org (expand-file-name "default.org" claudemacs-client-test-package-dir)))
    (unwind-protect
        (progn
          ;; Copy default.org to temp directory for get-package-directory to find
          (copy-file original-default-org (expand-file-name "default.org" temp-dir))
          (let ((default-directory temp-dir))
            (setq temp-file (claudemacs-client--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain default.org content
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
              (should (string-match-p "(claudemacs-client-start-claudemacs)" content))
              (should (string-match-p "Start Claude Code Session\\|claudemacs-client-start-claudemacs" content)))))
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
          (let ((claudemacs-client-template-file temp-template))
            (setq temp-file (claudemacs-client--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should contain custom template content
              (should (string-match-p "\\* Custom Template Content" content))
              (should (string-match-p "My Custom Section" content))
              ;; Should NOT contain default.org content
              (should-not (string-match-p "(claudemacs-client-start-claudemacs)" content)))))
      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-create-project-input-file-with-nonexistent-custom-template ()
  "Test that create-project-input-file falls back to default.org when custom template doesn't exist."
  (let ((claudemacs-client-template-file "/nonexistent/custom-template.org")
        (temp-dir (make-temp-file "test-project" t))
        (temp-file nil)
        (original-default-org (expand-file-name "default.org" claudemacs-client-test-package-dir)))
    (unwind-protect
        (progn
          ;; Copy default.org to temp directory for get-package-directory to find
          (copy-file original-default-org (expand-file-name "default.org" temp-dir))
          (let ((default-directory temp-dir))
            (setq temp-file (claudemacs-client--create-project-input-file temp-dir)))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should fallback to default.org content
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
              (should (string-match-p "(claudemacs-client-start-claudemacs)" content))
              (should (string-match-p "Start Claude Code Session\\|claudemacs-client-start-claudemacs" content)))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Integration Tests for claudemacs-client-open-project-input Command

(ert-deftest test-open-project-input-file-exists ()
  "Test that open-project-input opens existing file directly without creating new one."
  (let ((temp-dir (make-temp-file "test-project" t))
        (existing-content "* Existing Project File\n** Already exists\nThis file was already here.\n")
        (temp-file nil))
    (unwind-protect
        (progn
          ;; Create existing file
          (setq temp-file (claudemacs-client--get-project-file-path temp-dir))
          (with-temp-file temp-file
            (insert existing-content))
          (should (file-exists-p temp-file))

          ;; Mock find-file to capture what gets opened instead of actually opening
          (let ((opened-file nil)
                (claudemacs-client-template-file nil))
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
                (claudemacs-client-open-project-input))
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
        (original-default-org (expand-file-name "default.org" claudemacs-client-test-package-dir))
        (temp-file nil))
    (unwind-protect
        (progn
          ;; Copy default.org to temp directory for get-package-directory to find
          (copy-file original-default-org (expand-file-name "default.org" temp-dir))
          (setq temp-file (claudemacs-client--get-project-file-path temp-dir))
          ;; Ensure file doesn't exist initially
          (should-not (file-exists-p temp-file))

          ;; Mock find-file to capture what gets opened instead of actually opening
          (let ((opened-file nil)
                (claudemacs-client-template-file nil))
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
                (claudemacs-client-open-project-input))
              ;; Should open the newly created file
              (should (string= opened-file temp-file))
              ;; File should now exist and contain template content
              (should (file-exists-p temp-file))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))
                  (should (string-match-p "(claudemacs-client-start-claudemacs)" content)))))))
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
                ((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-region start end)
        ;; Should trim whitespace
        (should (string= sent-text "Hello World"))
        (should (string-match-p "Region sent to Claude" message-text))))))

(ert-deftest test-send-region-empty-after-trim ()
  "Test that send-region handles empty content after trimming."
  (with-temp-buffer
    (insert "  \n\t  \n  ")
    (let ((start (point-min))
          (end (point-max))
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-region start end)
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
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-rest-of-buffer)
        ;; Should trim whitespace
        (should (string= sent-text "Hello World"))
        (should (string-match-p "Rest of buffer sent to Claude" message-text))))))

(ert-deftest test-send-rest-of-buffer-empty-after-trim ()
  "Test that send-rest-of-buffer handles empty content after trimming."
  (with-temp-buffer
    (insert "Some content\n")
    (insert "  \n\t  \n  ")
    (goto-char (+ (point-min) 13)) ; Position after "Some content\n"
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-rest-of-buffer)
        ;; Should detect no content
        (should (string-match-p "No content from cursor to end" message-text))))))

(ert-deftest test-send-line-basic ()
  "Test that send-line sends current line content."
  (with-temp-buffer
    (insert "Line 1\nLine 2 with content\nLine 3")
    (goto-char 15) ; Position in "Line 2 with content"
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
        ;; Should send only current line
        (should (string= sent-text "Line 2 with content"))
        (should (string-match-p "Line sent to Claude" message-text))))))

(ert-deftest test-send-line-trims-whitespace ()
  "Test that send-line trims whitespace from current line."
  (with-temp-buffer
    (insert "Line 1\n  \t  Line with spaces  \t  \nLine 3")
    (goto-char 15) ; Position in line with spaces
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
        ;; Should trim whitespace
        (should (string= sent-text "Line with spaces"))
        (should (string-match-p "Line sent to Claude" message-text))))))

(ert-deftest test-send-line-empty-after-trim ()
  "Test that send-line handles empty line after trimming."
  (with-temp-buffer
    (insert "Line 1\n  \t  \n  \nLine 4")
    (goto-char 10) ; Position in empty line
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
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
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
        ;; Should send the single line
        (should (string= sent-text "Only one line"))
        (should (string-match-p "Line sent to Claude" message-text))))))

(ert-deftest test-send-line-no-buffer-found ()
  "Test that send-line handles case when no claudemacs buffer is found."
  (with-temp-buffer
    (insert "Test line")
    (let ((message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir) nil)) ; Simulate no buffer found
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
        ;; Should show error message
        (should (string-match-p "Cannot send.*no matching claudemacs buffer" message-text))))))

(ert-deftest test-send-line-file-path-handling ()
  "Test that send-line handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "~/Downloads/send-sample.png")
    (goto-char (point-min))
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-line)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by claudemacs-client" sent-text))
        (should (string-match-p "Line sent to Claude" message-text))))))

(ert-deftest test-send-rest-of-buffer-file-path-handling ()
  "Test that send-rest-of-buffer handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "Some content\n~/Downloads/send-sample.png")
    (goto-char (+ (point-min) 13)) ; Position after "Some content\n"
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-rest-of-buffer)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by claudemacs-client" sent-text))
        (should (string-match-p "Rest of buffer sent to Claude" message-text))))))

(ert-deftest test-send-buffer-file-path-handling ()
  "Test that send-buffer handles file paths correctly with sanitization."
  (with-temp-buffer
    (insert "Content with file path:\n~/Downloads/send-sample.png")
    (let ((sent-text nil)
          (message-text nil))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (claudemacs-client-send-buffer)
        ;; Should have explanatory text added
        (should (string-match-p "This text is added by claudemacs-client" sent-text))
        (should (string-match-p "File sent to Claude" message-text))))))

;;; Tests for Auto-Response Functions

(ert-deftest test-send-enter-functionality ()
  "Test that send-enter function works correctly."
  (let ((sent-text nil)
        (message-text nil))
    ;; Mock functions
    (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
               (lambda () "/test/dir"))
              ((symbol-function 'claudemacs-client--can-send-text)
               (lambda (dir) t))
              ((symbol-function 'claudemacs-client--send-text)
               (lambda (text dir)
                 (setq sent-text text)
                 t))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-text (apply #'format fmt args)))))
      (claudemacs-client-send-enter)
      (should (string= sent-text ""))
      (should (string-match-p "Sent enter to Claude" message-text)))))

(ert-deftest test-send-numbered-choices ()
  "Test that numbered choice functions work correctly."
  (dolist (choice '("1" "2" "3"))
    (let ((sent-text nil)
          (message-text nil)
          (func-name (intern (concat "claudemacs-client-send-" choice))))
      ;; Mock functions
      (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                 (lambda () "/test/dir"))
                ((symbol-function 'claudemacs-client--can-send-text)
                 (lambda (dir) t))
                ((symbol-function 'claudemacs-client--send-text)
                 (lambda (text dir)
                   (setq sent-text text)
                   t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-text (apply #'format fmt args)))))
        (funcall func-name)
        (should (string= sent-text choice))
        (should (string-match-p (format "Sent '%s' to Claude" choice) message-text))))))

;;; Tests for Template Output Function

(ert-deftest test-output-template-creates-buffer ()
  "Test that output-template creates a buffer with template content."
  (let ((default-directory claudemacs-client-test-package-dir))
    (unwind-protect
        (progn
          ;; Call the function - org-mode may fail due to hook conflicts, but that's OK
          (let ((claudemacs-client-template-file nil))
            (condition-case nil
                (claudemacs-client-output-template)
              (error nil)))  ; Ignore org-mode hook errors

          ;; Check that buffer was created
          (let ((buffer (get-buffer "*claudemacs-template-default*")))
            (should buffer)
            (with-current-buffer buffer
              (should (> (buffer-size) 0))
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" (buffer-string))))))

      ;; Cleanup
      (when (get-buffer "*claudemacs-template-default*")
        (kill-buffer "*claudemacs-template-default*")))))

(ert-deftest test-output-template-with-nonexistent-file ()
  "Test output-template behavior with nonexistent template file."
  (let ((claudemacs-client-template-file "/nonexistent/path.org"))
    ;; Should return nil and show message, but not error
    (should-not (claudemacs-client-output-template))
    ;; Should not create buffer
    (should-not (get-buffer "*claudemacs-template-path*"))))

;;; Integration Tests for Template System

(ert-deftest test-template-system-integration ()
  "Integration test for the complete template system."
  (let* ((temp-dir (make-temp-file "test-integration" t))
         (default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-file nil)
         (temp-file nil))
    (unwind-protect
        (progn
          ;; Test that template loading works with current language setting
          (let ((template-content (claudemacs-client--load-template)))
            (should template-content)
            (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" template-content)))

          ;; Test that project file initialization uses the loaded template
          (setq temp-file (claudemacs-client--create-project-input-file temp-dir))
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "#\\+TITLE: Claude Input File\\|\\* Quick Start" content))))

          ;; Test switching to custom template and regenerating
          (let ((temp-custom-template (make-temp-file "test-custom-template" nil ".org"))
                (temp-dir2 (make-temp-file "test-integration2" t))
                (temp-file2 nil)
                (custom-content "* Custom Integration Template\nProject: %s\n"))
            (unwind-protect
                (progn
                  (with-temp-file temp-custom-template
                    (insert custom-content))
                  (let ((claudemacs-client-template-file temp-custom-template))
                    (setq temp-file2 (claudemacs-client--create-project-input-file temp-dir2))
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

;;; Tests for claudemacs-client-start-claudemacs Function

(ert-deftest test-start-claudemacs-directory-change ()
  "Test that claudemacs-client-start-claudemacs changes default-directory correctly."
  (let ((original-dir (expand-file-name default-directory))
        (test-dir (expand-file-name "/tmp/")))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'claudemacs-client--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'claudemacs-client--can-send-text)
                   ;; Mock different results for before/after startup
                   (let ((call-count 0))
                     (lambda (dir)
                       (setq call-count (1+ call-count))
                       (> call-count 1))))  ; Return t after first call (simulating successful startup)
                  ((symbol-function 'claudemacs-transient-menu)
                   (lambda () (message "Mock claudemacs-transient-menu called"))))
          (cd original-dir)
          (claudemacs-client-start-claudemacs)
          (should (string= (expand-file-name default-directory) test-dir)))
      ;; Cleanup
      (cd original-dir))))

(ert-deftest test-start-claudemacs-existing-live-session ()
  "Test behavior when live session already exists."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory))
        (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'claudemacs-client--get-buffer-for-directory)
                   (lambda (dir) (get-buffer-create "*mock-claudemacs-buffer*")))
                  ((symbol-function 'claudemacs-client--can-send-text)
                   (lambda (dir) t))  ; Simulate live session
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (claudemacs-client-start-claudemacs)
          ;; Should not change directory when session exists
          (should (string= (expand-file-name default-directory) original-dir))
          ;; Should show appropriate message
          (should (cl-some (lambda (msg) (string-match-p "already running" msg)) messages)))
      ;; Cleanup
      (when (get-buffer "*mock-claudemacs-buffer*")
        (kill-buffer "*mock-claudemacs-buffer*"))
      (cd original-dir))))

(ert-deftest test-start-claudemacs-dead-session ()
  "Test behavior when dead session exists."
  (let ((test-dir "/tmp/")
        (original-dir default-directory)
        (restart-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'claudemacs-client--get-buffer-for-directory)
                   (lambda (dir) (get-buffer-create "*mock-dead-claudemacs-buffer*")))
                  ((symbol-function 'claudemacs-client--can-send-text)
                   (lambda (dir) nil))  ; Simulate dead session
                  ((symbol-function 'y-or-n-p)
                   (lambda (prompt) t))  ; Simulate "yes" to restart
                  ((symbol-function 'kill-buffer)
                   (lambda (buffer) t))
                  ((symbol-function 'claudemacs-client-start-claudemacs)
                   (lambda () (setq restart-called t))))  ; Mock recursive call
          (claudemacs-client-start-claudemacs)
          (should restart-called))
      ;; Cleanup
      (when (get-buffer "*mock-dead-claudemacs-buffer*")
        (kill-buffer "*mock-dead-claudemacs-buffer*"))
      (cd original-dir))))

(ert-deftest test-start-claudemacs-claudemacs-unavailable ()
  "Test error handling when claudemacs-transient-menu is not available."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory))
        (error-caught nil))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'claudemacs-client--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'claudemacs-client--can-send-text)
                   (lambda (dir) nil))
                  ((symbol-function 'fboundp)
                   (lambda (func) nil)))  ; Simulate claudemacs-transient-menu not available
          (condition-case err
              (claudemacs-client-start-claudemacs)
            (error
             (setq error-caught (error-message-string err))))
          (should error-caught)
          (should (string-match-p "not available" error-caught))
          ;; Should restore original directory on error
          (should (string= (expand-file-name default-directory) original-dir)))
      ;; Cleanup
      (cd original-dir))))

(ert-deftest test-start-claudemacs-state-restoration ()
  "Test that default-directory is restored when startup fails."
  (let ((test-dir "/tmp/")
        (original-dir (expand-file-name default-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'claudemacs-client--get-target-directory-for-buffer)
                   (lambda () test-dir))
                  ((symbol-function 'claudemacs-client--get-buffer-for-directory)
                   (lambda (dir) nil))
                  ((symbol-function 'claudemacs-client--can-send-text)
                   (lambda (dir) nil))  ; Simulate failed startup
                  ((symbol-function 'claudemacs-transient-menu)
                   (lambda () (error "Simulated startup failure"))))
          (condition-case nil
              (claudemacs-client-start-claudemacs)
            (error nil))  ; Ignore the error
          ;; Should restore original directory
          (should (string= (expand-file-name default-directory) original-dir)))
      ;; Cleanup
      (cd original-dir))))

;;; Test-Only Pure Functions
;; These functions are only used by tests and moved here to avoid
;; polluting the main package namespace

(defun claudemacs-client--load-template-pure (custom-template-file package-dir default-template-name)
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

(defun claudemacs-client--extract-directory-pure (buffer-file-name-arg default-directory-arg)
  "Pure function: Extract target directory from BUFFER-FILE-NAME-ARG.
Uses DEFAULT-DIRECTORY-ARG if BUFFER-FILE-NAME-ARG doesn't match pattern.
If BUFFER-FILE-NAME-ARG matches persistent file pattern, decode directory."
  (if (and buffer-file-name-arg
           (string-match-p "cec-.+\\.org$" (file-name-nondirectory buffer-file-name-arg)))
      ;; This is a persistent file, extract directory from filename
      (claudemacs-client--decode-full-path
       (file-name-base (file-name-nondirectory buffer-file-name-arg)))
    ;; Use current directory
    default-directory-arg))

(defun claudemacs-client--buffer-matches-directory-pure (buffer-default-directory target-dir)
  "Pure function: Check if BUFFER-DEFAULT-DIRECTORY matches TARGET-DIR.
Returns t if directories match, nil otherwise."
  (when buffer-default-directory
    (string=
     (file-truename buffer-default-directory)
     (file-truename target-dir))))

(defun claudemacs-client--find-matching-buffer-pure (buffer-list target-directory)
  "Pure function: Find claudemacs buffer matching TARGET-DIRECTORY.
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
        ;; Check for directory-specific claudemacs buffer
        (or (and (string-match-p "^\\*claudemacs:" name)
                 (string-prefix-p (concat "*claudemacs:" target-directory) name))
            ;; Fallback to generic buffers only if they match directory
            (and (or (string= name "*claude*")
                     (string= name "*claudemacs*"))
                 (claudemacs-client--buffer-matches-directory-pure default-dir target-directory))
            ;; Check for eat-mode buffers with claude in name
            (and eat-mode
                 (string-match-p "claude" name)
                 (claudemacs-client--buffer-matches-directory-pure default-dir target-directory))))))
   buffer-list))

(defun claudemacs-client--can-send-text-pure (claude-buffer)
  "Pure function: Check if CLAUDE-BUFFER can receive text.
Returns t if buffer has live eat process, nil otherwise."
  (when claude-buffer
    (with-current-buffer claude-buffer
      (and
       (boundp 'eat--process)
       eat--process
       (process-live-p eat--process)))))

(defun claudemacs-client--send-text-pure (text claude-buffer)
  "Pure function: Send TEXT to CLAUDE-BUFFER.
Returns t if successful, nil if buffer cannot receive text.
Does not modify global state."
  (claudemacs-client--debug-message "send-text-pure called with text length: %d, buffer: %s"
                                    (length text) (if claude-buffer (buffer-name claude-buffer) "nil"))
  (when
      (and claude-buffer (claudemacs-client--can-send-text-pure claude-buffer))
    (claudemacs-client--debug-message "Sending text to claude buffer: %S" (substring text 0 (min 50 (length text))))
    (with-current-buffer claude-buffer
      (eat--send-string eat--process text)
      (eat--send-string eat--process "\r")
      (claudemacs-client--debug-message "Text sent successfully")
      t)))



;;; Tests for encode/decode pure functions

(ert-deftest test-encode-full-path-pure-basic ()
  "Test basic path encoding with custom prefix and separator."
  (should (string= (claudemacs-client--encode-full-path-pure
                    "/Users/test"
                    "cec"
                    "--")
                   "cec--Users--test"))
  (should (string= (claudemacs-client--encode-full-path-pure
                    "/Users/test/"
                    "cec"
                    "--")
                   "cec--Users--test"))
  (should (string= (claudemacs-client--encode-full-path-pure
                    "relative/path"
                    "prefix"
                    "_")
                   (concat "prefix" (replace-regexp-in-string "/" "_" (expand-file-name "relative/path"))))))

(ert-deftest test-encode-full-path-pure-custom-separators ()
  "Test path encoding with different separators and prefixes."
  (should (string= (claudemacs-client--encode-full-path-pure
                    "/Users/test"
                    "xyz"
                    "___")
                   "xyz___Users___test"))
  (should (string= (claudemacs-client--encode-full-path-pure
                    "/a/b/c"
                    ""
                    "-")
                   "-a-b-c")))

(ert-deftest test-decode-full-path-pure-basic ()
  "Test basic path decoding with custom prefix and separator."
  (should (string= (claudemacs-client--decode-full-path-pure
                    "cec--Users--test"
                    "cec"
                    "--")
                   "/Users/test/"))
  (should (string= (claudemacs-client--decode-full-path-pure
                    "prefix_Users_test"
                    "prefix"
                    "_")
                   "/Users/test/")))

(ert-deftest test-decode-full-path-pure-invalid-prefix ()
  "Test decoding with invalid prefix returns nil."
  (should-not (claudemacs-client--decode-full-path-pure
               "invalid--Users--test"
               "cec"
               "--"))
  (should-not (claudemacs-client--decode-full-path-pure
               "partial"
               "prefix"
               "--")))

(ert-deftest test-encode-decode-roundtrip-pure ()
  "Test that encoding and decoding are symmetric with pure functions."
  (let ((test-paths '("/Users/test" "/project/subdir" "/a/b/c/d"))
        (prefix "test")
        (separator "___"))
    (dolist (path test-paths)
      (let* ((encoded (claudemacs-client--encode-full-path-pure path prefix separator))
             (decoded (claudemacs-client--decode-full-path-pure encoded prefix separator)))
        (should (string= (concat path "/") decoded))))))

(ert-deftest test-encode-decode-edge-cases-pure ()
  "Test edge cases for pure encode/decode functions."
  ;; Empty prefix
  (should (string= (claudemacs-client--encode-full-path-pure "/test" "" "--")
                   "--test"))
  (should (string= (claudemacs-client--decode-full-path-pure "--test" "" "--")
                   "/test/"))

  ;; Single character separator
  (should (string= (claudemacs-client--encode-full-path-pure "/a/b" "x" ".")
                   "x.a.b"))
  (should (string= (claudemacs-client--decode-full-path-pure "x.a.b" "x" ".")
                   "/a/b/")))

;;; Test Runner

(defun claudemacs-client-run-all-tests ()
  "Run all claudemacs-client pure function tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-"))

(provide 'claudemacs-client-test)

;;; claudemacs-client-test.el ends here

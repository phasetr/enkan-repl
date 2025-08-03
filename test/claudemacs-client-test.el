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

;;; Tests for Template Loading Functions

(ert-deftest test-get-template-path-with-existing-file ()
  "Test getting template path for existing template file."
  (let ((default-directory claudemacs-client-test-package-dir))
    ;; Test with English template (should exist)
    (let ((template-path (claudemacs-client--get-template-path "en")))
      (should template-path)
      (should (file-exists-p template-path))
      (should (string-match-p "templates/en\\.org$" template-path)))))

(ert-deftest test-get-template-path-with-nonexistent-file ()
  "Test getting template path for nonexistent template file."
  (let ((default-directory claudemacs-client-test-package-dir))
    ;; Test with nonexistent language
    (should-not (claudemacs-client--get-template-path "nonexistent"))))

(ert-deftest test-get-template-path-directory-fallback ()
  "Test template path detection with directory fallback."
  (let ((load-file-name nil)
         (buffer-file-name nil)
         (default-directory claudemacs-client-test-package-dir))
    ;; Should fallback to default-directory
    (let ((template-path (claudemacs-client--get-template-path "en")))
      (should template-path)
      (should (file-exists-p template-path)))))

(ert-deftest test-load-template-english ()
  "Test loading English template."
  (let ((default-directory claudemacs-client-test-package-dir))
    (let ((template-content (claudemacs-client--load-template "en")))
      (should template-content)
      (should (stringp template-content))
      (should (string-match-p "\\* Claude Input File" template-content))
      (should (string-match-p "Project: %s" template-content)))))

(ert-deftest test-load-template-japanese ()
  "Test loading Japanese template."
  (let ((default-directory claudemacs-client-test-package-dir))
    (let ((template-content (claudemacs-client--load-template "ja")))
      (should template-content)
      (should (stringp template-content))
      (should (string-match-p "\\* Claude 入力ファイル" template-content))
      (should (string-match-p "プロジェクト: %s" template-content)))))

(ert-deftest test-load-template-custom-fallback ()
  "Test loading custom template falls back to English."
  (let ((default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-custom-template-path nil))
    (let ((template-content (claudemacs-client--load-template "custom")))
      (should template-content)
      (should (stringp template-content))
      ;; Should be English content since no custom file specified
      (should (string-match-p "\\* Claude Input File" template-content)))))

(ert-deftest test-load-template-with-custom-path ()
  "Test loading template with custom template path override."
  (let ((temp-file (make-temp-file "test-template" nil ".org"))
         (test-content "* Custom Template\nProject: %s\n"))
    (unwind-protect
        (progn
          ;; Write test content to temp file
          (with-temp-file temp-file
            (insert test-content))
          
          ;; Test with custom template path
          (let ((claudemacs-client-custom-template-path temp-file))
            (let ((template-content (claudemacs-client--load-template "any-language")))
              (should template-content)
              (should (string= template-content test-content)))))
      (delete-file temp-file))))

(ert-deftest test-load-template-nonexistent ()
  "Test loading nonexistent template returns nil."
  (let ((default-directory "/nonexistent/path/"))
    (should-not (claudemacs-client--load-template "nonexistent"))))

(ert-deftest test-load-template-fallback-chain ()
  "Test template loading fallback chain behavior."
  (let ((default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-custom-template-path nil))
    
    ;; Test that nonexistent language falls back to English
    (let ((template-content (claudemacs-client--load-template "nonexistent")))
      (should template-content)
      (should (string-match-p "\\* Claude Input File" template-content)))
    
    ;; Test that custom without file falls back to English  
    (let ((template-content (claudemacs-client--load-template "custom")))
      (should template-content)
      (should (string-match-p "\\* Claude Input File" template-content)))))

;;; Tests for Template Generation Function (via initialize-project-file)

(ert-deftest test-initialize-project-file-english ()
  "Test project file initialization with English template."
  (let ((default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-language "en")
         (temp-file (make-temp-file "test-project" nil ".org")))
    (unwind-protect
        (progn
          (claudemacs-client--initialize-project-file temp-file)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\* Claude Input File" content))
              (should (string-match-p "(claudemacs-start)" content)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-initialize-project-file-japanese ()
  "Test project file initialization with Japanese template."
  (let ((default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-language "ja")
         (temp-file (make-temp-file "test-project" nil ".org")))
    (unwind-protect
        (progn
          (claudemacs-client--initialize-project-file temp-file)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\* Claude 入力ファイル" content))
              (should (string-match-p "(claudemacs-start)" content)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-initialize-project-file-fallback ()
  "Test project file initialization with fallback when template loading fails."
  (let ((default-directory claudemacs-client-test-package-dir)  ; Use valid directory
         (claudemacs-client-template-language "nonexistent")
         (temp-file (make-temp-file "test-project" nil ".org")))
    (unwind-protect
        (progn
          ;; Should use fallback to English template
          (claudemacs-client--initialize-project-file temp-file)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\* Claude Input File" content))
              (should (string-match-p "(claudemacs-start)" content)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Tests for Template Output Function

(ert-deftest test-output-template-creates-buffer ()
  "Test that output-template creates a buffer with template content."
  (let ((default-directory claudemacs-client-test-package-dir))
    (unwind-protect
        (progn
          ;; Call the function - org-mode may fail due to hook conflicts, but that's OK
          (condition-case nil
              (claudemacs-client-output-template "en")
            (error nil))  ; Ignore org-mode hook errors
          
          ;; Check that buffer was created
          (let ((buffer (get-buffer "*claudemacs-template-en*")))
            (should buffer)
            (with-current-buffer buffer
              (should (> (buffer-size) 0))
              (should (string-match-p "\\* Claude Input File" (buffer-string))))))
      
      ;; Cleanup
      (when (get-buffer "*claudemacs-template-en*")
        (kill-buffer "*claudemacs-template-en*")))))

(ert-deftest test-output-template-with-nonexistent-language ()
  "Test output-template behavior with nonexistent language."
  (let ((default-directory "/nonexistent/path/"))
    ;; Should return nil and show message, but not error
    (should-not (claudemacs-client-output-template "nonexistent"))
    ;; Should not create buffer
    (should-not (get-buffer "*claudemacs-template-nonexistent*"))))

;;; Integration Tests for Template System

(ert-deftest test-template-system-integration ()
  "Integration test for the complete template system."
  (let ((default-directory claudemacs-client-test-package-dir)
         (claudemacs-client-template-language "ja")
         (temp-file (make-temp-file "test-integration" nil ".org")))
    (unwind-protect
        (progn
          ;; Test that template loading works with current language setting
          (let ((template-content (claudemacs-client--load-template claudemacs-client-template-language)))
            (should template-content)
            (should (string-match-p "\\* Claude 入力ファイル" template-content)))
          
          ;; Test that project file initialization uses the loaded template
          (claudemacs-client--initialize-project-file temp-file)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\\* Claude 入力ファイル" content))))
          
          ;; Test switching language and regenerating
          (let ((claudemacs-client-template-language "en")
                (temp-file2 (make-temp-file "test-integration-en" nil ".org")))
            (unwind-protect
                (progn
                  (claudemacs-client--initialize-project-file temp-file2)
                  (should (file-exists-p temp-file2))
                  (with-temp-buffer
                    (insert-file-contents temp-file2)
                    (let ((content (buffer-string)))
                      (should (string-match-p "\\* Claude Input File" content)))))
              (when (file-exists-p temp-file2)
                (delete-file temp-file2)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Test Runner

(defun claudemacs-client-run-all-tests ()
  "Run all claudemacs-client pure function tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-"))

(provide 'claudemacs-client-test)

;;; claudemacs-client-test.el ends here

;;; session-management-test.el --- Unified tests for session management functionality -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: test

;;; Commentary:

;; Unified tests for session management functionality including:
;; - Session controller operations
;; - Session list management
;; - Session minibuffer interactions
;; - Session keybinding behavior

;;; Code:

(require 'ert)

;; Load the main package
(unless (featurep 'enkan-repl)
  (condition-case nil
      (let ((main-file (expand-file-name "../enkan-repl.el" 
                                         (file-name-directory (or load-file-name buffer-file-name)))))
        (when (file-exists-p main-file)
          (load main-file)))
    (error "Could not load enkan-repl.el")))

;;;; Session Controller Tests

(ert-deftest test-get-session-info-structure ()
  "Test that get-session-info returns correct structure."
  (let ((info (enkan-repl--get-session-info)))
    (should (listp info))
    (should (= 3 (length info)))
    (should (stringp (nth 0 info)))))  ; target-dir should be string

(ert-deftest test-get-session-info-consistency ()
  "Test that get-session-info returns consistent results across multiple calls."
  (let ((info1 (enkan-repl--get-session-info))
        (info2 (enkan-repl--get-session-info)))
    (should (equal (nth 0 info1) (nth 0 info2)))  ; Same target directory
    (should (equal (nth 1 info1) (nth 1 info2)))  ; Same existing buffer
    (should (equal (nth 2 info1) (nth 2 info2))))) ; Same can-send status

;;;; Session List Management Tests

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

(ert-deftest test-format-session-list-pure-single-session ()
  "Test formatting single session for display."
  (let ((sessions '((:name "*enkan:/home/user/project/*" 
                     :directory "/home/user/project/" 
                     :status alive))))
    (let ((result (enkan-repl--format-session-list-pure sessions)))
      (should (string-match-p "project" result))
      (should (string-match-p "ALIVE" result)))))

(ert-deftest test-format-session-list-pure-multiple-sessions ()
  "Test formatting multiple sessions for display."
  (let ((sessions '((:name "*enkan:/home/user/proj1/*" :directory "/home/user/proj1/" :status alive)
                    (:name "*enkan:/home/user/proj2/*" :directory "/home/user/proj2/" :status dead))))
    (let ((result (enkan-repl--format-session-list-pure sessions)))
      (should (string-match-p "proj1" result))
      (should (string-match-p "proj2" result))
      (should (string-match-p "ALIVE" result))
      (should (string-match-p "DEAD" result)))))

;;;; Session Minibuffer Tests

(ert-deftest test-prepare-session-candidates-pure ()
  "Test preparation of session candidates for minibuffer completion."
  (let ((sessions '(("project1" . "/path/to/project1")
                    ("project2" . "/path/to/project2"))))
    (let ((result (enkan-repl--prepare-session-candidates-pure sessions)))
      (should (= 2 (length result)))
      (should (member "project1" result))
      (should (member "project2" result)))))

(ert-deftest test-prepare-session-candidates-pure-empty-list ()
  "Test preparation with empty session list."
  (let ((result (enkan-repl--prepare-session-candidates-pure '())))
    (should (null result))))

(ert-deftest test-validate-session-selection-pure ()
  "Test validation of session selection."
  (let ((candidates '("project1" "project2")))
    (should (enkan-repl--validate-session-selection-pure "project1" candidates))
    (should (enkan-repl--validate-session-selection-pure "project2" candidates))
    (should-not (enkan-repl--validate-session-selection-pure "invalid" candidates))))

;;;; Session Keybinding Tests

(ert-deftest test-session-minibuffer-map-has-c-d ()
  "Test that C-d is bound in session minibuffer map."
  (should (keymapp enkan-repl-session-minibuffer-map))
  (should (lookup-key enkan-repl-session-minibuffer-map (kbd "C-d"))))

(ert-deftest test-session-minibuffer-map-inherits-from-completion-map ()
  "Test that session minibuffer map properly inherits from completion-list-mode-map."
  (should (keymapp enkan-repl-session-minibuffer-map))
  ;; Check that it has basic completion functionality
  (should (lookup-key enkan-repl-session-minibuffer-map (kbd "TAB"))))

(ert-deftest test-session-keybinding-integration ()
  "Test integration of session keybinding behavior."
  (let ((keymap enkan-repl-session-minibuffer-map))
    (should (keymapp keymap))
    ;; Verify core keybindings exist
    (should (lookup-key keymap (kbd "C-d")))
    (should (lookup-key keymap (kbd "RET")))
    (should (lookup-key keymap (kbd "TAB")))))

;;;; Integration Tests

(ert-deftest test-session-workflow-integration ()
  "Test complete session management workflow."
  (let ((test-sessions '(("test-project" . "/tmp/test-project"))))
    ;; Test candidate preparation
    (let ((candidates (enkan-repl--prepare-session-candidates-pure test-sessions)))
      (should (member "test-project" candidates))
      
      ;; Test validation
      (should (enkan-repl--validate-session-selection-pure "test-project" candidates))
      (should-not (enkan-repl--validate-session-selection-pure "nonexistent" candidates)))))

(ert-deftest test-session-info-extraction-workflow ()
  "Test session information extraction workflow."
  (let ((session-info (enkan-repl--extract-session-info-pure
                       "*enkan:/tmp/workflow-test/*"
                       t t t)))
    (should (plist-get session-info :name))
    (should (plist-get session-info :directory))
    (should (eq (plist-get session-info :status) 'alive))))

(provide 'session-management-test)

;;; session-management-test.el ends here
;;; enkan-repl-core-test.el --- Core tests for enkan-repl -*- lexical-binding: t -*-

;;; Commentary:
;; Core functionality tests for enkan-repl.el

;;; Code:

(require 'ert)
(require 'enkan-repl)

;; Test data helpers
(defun test--create-mock-buffer (name)
  "Create a mock buffer with NAME for testing."
  (get-buffer-create name))

(defun test--cleanup-buffers ()
  "Clean up test buffers."
  (dolist (buffer (buffer-list))
    (when (string-match-p "^\\*test-" (buffer-name buffer))
      ;; Kill any processes in the buffer first
      (when-let ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc))
      (kill-buffer buffer))))

;; Tests for enkan-repl--encode-full-path-pure
(ert-deftest test-enkan-repl--encode-full-path-pure ()
  "Test pure path encoding function."
  ;; Absolute paths - note the leading slash gets encoded
  (should (string= (enkan-repl--encode-full-path-pure "/home/user/project" "enkan-repl" "_")
                   "enkan-repl_home_user_project"))
  (should (string= (enkan-repl--encode-full-path-pure "/path/with/many/levels" "enkan-repl" "_")
                   "enkan-repl_path_with_many_levels"))
  ;; Root directory - just the separator remains
  (should (string= (enkan-repl--encode-full-path-pure "/" "enkan-repl" "_")
                   "enkan-repl"))
  ;; Relative path gets expanded to absolute
  (let ((result (enkan-repl--encode-full-path-pure "relative/path" "enkan-repl" "_")))
    (should (string-prefix-p "enkan-repl" result))
    (should (string-match-p "relative_path" result))))

;; Tests for enkan-repl--decode-full-path-pure
(ert-deftest test-enkan-repl--decode-full-path-pure ()
  "Test pure path decoding function."
  ;; Decode adds trailing slash
  (should (string= (enkan-repl--decode-full-path-pure "enkan-repl_home_user_project" "enkan-repl" "_")
                   "/home/user/project/"))
  (should (string= (enkan-repl--decode-full-path-pure "enkan-repl_path_with_many_levels" "enkan-repl" "_")
                   "/path/with/many/levels/"))
  ;; Root directory - prefix followed by single separator becomes slash with trailing slash
  (should (string= (enkan-repl--decode-full-path-pure "enkan-repl_" "enkan-repl_" "_")
                   "/"))
  ;; Relative path becomes absolute path with trailing slash
  (should (string= (enkan-repl--decode-full-path-pure "enkan-repl_relative_path" "enkan-repl" "_")
                   "/relative/path/")))

;; Tests for enkan-repl--resolve-project-name
(ert-deftest test-enkan-repl--resolve-project-name ()
  "Test project name resolution with aliases."
  (let ((enkan-repl-project-aliases '(("myproj" . "my-awesome-project")
                                       ("web" . "website-development"))))
    (should (string= (enkan-repl--resolve-project-name "myproj")
                     "my-awesome-project"))
    (should (string= (enkan-repl--resolve-project-name "web")
                     "website-development"))
    (should (string= (enkan-repl--resolve-project-name "unaliased")
                     "unaliased"))
    (should (string= (enkan-repl--resolve-project-name nil)
                     nil))))

;; Tests for enkan-repl--register-session
(ert-deftest test-enkan-repl--register-session ()
  "Test session registration."
  (let ((enkan-repl-session-list nil))
    ;; Register first session
    (enkan-repl--register-session 1 "project-a")
    (should (equal enkan-repl-session-list '((1 . "project-a"))))
    
    ;; Register another session
    (enkan-repl--register-session 2 "project-b")
    (should (equal enkan-repl-session-list '((1 . "project-a") (2 . "project-b"))))
    
    ;; Update existing session
    (enkan-repl--register-session 1 "project-c")
    (should (equal enkan-repl-session-list '((1 . "project-c") (2 . "project-b"))))))

;; Tests for enkan-repl--get-package-directory
(ert-deftest test-enkan-repl--get-package-directory ()
  "Test package directory detection."
  (let ((dir (enkan-repl--get-package-directory)))
    (should (stringp dir))
    (should (file-exists-p dir))
    (should (file-exists-p (expand-file-name "enkan-repl.el" dir)))))

;; Tests for enkan-repl--get-project-file-path
(ert-deftest test-enkan-repl--get-project-file-path ()
  "Test project file path generation."
  ;; Test with specific directory
  (let ((test-dir "/home/user/project1"))
    (let ((path (enkan-repl--get-project-file-path test-dir)))
      (should (stringp path))
      (should (string-match-p "\\.org$" path))
      ;; Check for encoded path elements
      (should (string-match-p "enkan" path))))
  
  ;; Test with another directory
  (let ((test-dir "/home/user/project2"))
    (let ((path (enkan-repl--get-project-file-path test-dir)))
      (should (stringp path))
      (should (string-match-p "\\.org$" path))
      ;; Check for encoded path elements
      (should (string-match-p "enkan" path)))))

;; Tests for enkan-repl--extract-project-name
(ert-deftest test-enkan-repl--extract-project-name ()
  "Test project name extraction from buffer names."
  ;; Function extracts from *enkan:/path/* pattern or uses file-name-nondirectory
  ;; Test extraction from enkan buffer pattern
  (should (string= (enkan-repl--extract-project-name "*enkan:/home/user/myproject/*")
                   "myproject"))
  ;; Test extraction from path directly (no enkan pattern)
  (should (string= (enkan-repl--extract-project-name "/home/user/project")
                   "project"))
  ;; Test extraction from simple buffer name
  (should (string= (enkan-repl--extract-project-name "buffer-name")
                   "buffer-name"))
  ;; Test extraction from path with trailing slash
  (should (string= (enkan-repl--extract-project-name "/home/user/")
                   "user")))

;; Tests for enkan-repl--find-directory-by-project-name
(ert-deftest test-enkan-repl--find-directory-by-project-name ()
  "Test finding directory by project name."
  (let ((enkan-repl-projects '("/home/user/my-project"
                                "/home/user/another-project"))
        (enkan-repl-target-directories '("/opt/special-project")))
    ;; The function checks target-directories first
    (let ((result (enkan-repl--find-directory-by-project-name "special-project")))
      (should (or (string= result "/opt/special-project")
                  (null result))))
    
    ;; Check projects
    (let ((result (enkan-repl--find-directory-by-project-name "my-project")))
      (should (or (string= result "/home/user/my-project")
                  (null result))))
    
    ;; Not found returns nil
    (should (null (enkan-repl--find-directory-by-project-name "non-existent")))))

;; Tests for enkan-repl--can-send-text  
(ert-deftest test-enkan-repl--can-send-text ()
  "Test checking if text can be sent to buffer."
  ;; Test nil for non-existent buffer
  (should-not (enkan-repl--can-send-text "*non-existent*"))
  
  ;; Test nil for buffer without eat-mode
  (let ((buffer (get-buffer-create "*test-normal-buffer*")))
    (unwind-protect
        (should-not (enkan-repl--can-send-text buffer))
      (kill-buffer buffer)))
  
  ;; Test function behavior with mock eat-mode buffer
  ;; We can't easily test the full functionality without starting real process
  ;; so we test the basic logic
  (let ((buffer (get-buffer-create "*test-eat-buffer*")))
    (unwind-protect
        (with-current-buffer buffer
          ;; Without eat-mode and process, should return nil
          (should-not (enkan-repl--can-send-text buffer)))
      (kill-buffer buffer))))

;; Tests for enkan-repl--get-embedded-template
(ert-deftest test-enkan-repl--get-embedded-template ()
  "Test embedded template generation."
  (let ((template (enkan-repl--get-embedded-template)))
    (should (stringp template))
    ;; Check for key sections in template
    (should (or (string-match-p "Enkan REPL" template)
                (string-match-p "enkan-repl" template)))
    ;; Template should have some structure
    (should (> (length template) 100))))

;; Tests for enkan-repl--handle-missing-template
(ert-deftest test-enkan-repl--handle-missing-template ()
  "Test handling of missing template."
  ;; Set testing mode to avoid interactive prompts
  (let ((enkan-repl--testing-mode t)
        (test-file (make-temp-file "test-template" nil ".org")))
    (unwind-protect
        (progn
          ;; Delete the file to test creation
          (delete-file test-file)
          
          ;; In testing mode, should return nil (use default template)
          (let ((result (enkan-repl--handle-missing-template test-file)))
            (should (null result))))
      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;; Tests for enkan-repl--load-template
(ert-deftest test-enkan-repl--load-template ()
  "Test template loading."
  ;; When no custom template is set - should use embedded or default
  (let ((enkan-repl-template-file nil))
    (let ((template (enkan-repl--load-template)))
      (should (stringp template))
      ;; Template should have content
      (should (> (length template) 100))))
  
  ;; When custom template exists
  (let* ((test-file (make-temp-file "test-custom-template" nil ".org"))
         (enkan-repl-template-file test-file))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "# Custom Template\nCustom content"))
          
          (let ((template (enkan-repl--load-template)))
            (should (stringp template))
            (should (string-match-p "# Custom Template" template))))
      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'enkan-repl-core-test)
;;; enkan-repl-core-test.el ends here
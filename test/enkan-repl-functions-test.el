;;; enkan-repl-functions-test.el --- Tests for functions -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for functions in enkan-repl

;;; Code:

(require 'ert)
(require 'enkan-repl)

;; Tests for enkan-repl--get-project-info-from-directories
(ert-deftest test-enkan-repl--get-project-info-from-directories ()
  "Test getting project info from directories."
  ;; Test with sample target directories
  (let ((target-directories '(("proj1" "Project One" . "/home/user/project1")
                              ("proj2" "Project Two" . "/home/user/project2")
                              ("test" "Test Project" . "/opt/test"))))
    ;; Test existing alias
    (should (equal (enkan-repl--get-project-info-from-directories "proj1" target-directories)
                   '("Project One" . "/home/user/project1")))
    (should (equal (enkan-repl--get-project-info-from-directories "proj2" target-directories)
                   '("Project Two" . "/home/user/project2")))
    (should (equal (enkan-repl--get-project-info-from-directories "test" target-directories)
                   '("Test Project" . "/opt/test")))
    ;; Test non-existing alias
    (should (null (enkan-repl--get-project-info-from-directories "nonexistent" target-directories)))
    ;; Test with empty directories
    (should (null (enkan-repl--get-project-info-from-directories "proj1" '())))
    ;; Test with nil directories
    (should (null (enkan-repl--get-project-info-from-directories "proj1" nil)))))

;; Tests for enkan-repl--resolve-send-target
(ert-deftest test-enkan-repl--resolve-send-target ()
  "Test resolving send target with prefix-arg and alias."
  ;; Test with no current project and no buffers
  (let ((result (enkan-repl--resolve-send-target
                 nil nil nil nil nil)))
    (should (equal (plist-get result :status) 'no-buffers))
    (should (string-match-p "No active enkan sessions" (plist-get result :message))))
  
  ;; Test with prefix-arg resolving to buffer
  (let* ((buffer1 (get-buffer-create "*enkan:/path/1*"))
         (buffer2 (get-buffer-create "*enkan:/path/2*"))
         (result (enkan-repl--resolve-send-target
                  1 nil "test-project"
                  '(("test-project" . ("alias1" "alias2")))
                  '(("alias1" "Project One" . "/path/1")
                    ("alias2" "Project Two" . "/path/2")))))
    (should (equal (plist-get result :status) 'selected))
    (should (equal (plist-get result :buffer) buffer1))
    (kill-buffer buffer1)
    (kill-buffer buffer2))
  
  ;; Test with alias resolving to buffer
  (let* ((buffer1 (get-buffer-create "*enkan:/path/to/proj1*"))
         (result (enkan-repl--resolve-send-target
                  nil "alias1" "test-project" 
                  '(("test-project" . ("alias1")))
                  '(("alias1" "Project One" . "/path/to/proj1")))))
    (should (equal (plist-get result :status) 'selected))
    (should (equal (plist-get result :buffer) buffer1))
    (kill-buffer buffer1))
  
  ;; Test with alias not found (but buffer exists for other aliases)
  (let* ((buffer1 (get-buffer-create "*enkan:/path/to/proj1*"))
         (result (enkan-repl--resolve-send-target
                  nil "nonexistent" "test-project"
                  '(("test-project" . ("alias1")))
                  '(("alias1" "Project One" . "/path/to/proj1")))))
    (should (equal (plist-get result :status) 'invalid))
    (should (string-match-p "No buffer found for alias" (plist-get result :message)))
    (kill-buffer buffer1))
  
  ;; Test with single buffer auto-selection
  (let* ((buffer1 (get-buffer-create "*enkan:/single/path*"))
         (result (enkan-repl--resolve-send-target
                  nil nil "test-project"
                  '(("test-project" . ("alias1")))
                  '(("alias1" "Project One" . "/single/path")))))
    (should (equal (plist-get result :status) 'single))
    (should (equal (plist-get result :buffer) buffer1))
    (kill-buffer buffer1))
  
  ;; Test requiring interactive selection
  (let* ((buffer1 (get-buffer-create "*enkan:/path/1*"))
         (buffer2 (get-buffer-create "*enkan:/path/2*"))
         (result (enkan-repl--resolve-send-target
                  nil nil "test-project"
                  '(("test-project" . ("alias1" "alias2")))
                  '(("alias1" "Project One" . "/path/1")
                    ("alias2" "Project Two" . "/path/2")))))
    (should (equal (plist-get result :status) 'needs-selection))
    (should (equal (length (plist-get result :buffers)) 2))
    (kill-buffer buffer1)
    (kill-buffer buffer2)))

;; Tests for enkan-repl--target-directory-info
(ert-deftest test-enkan-repl--target-directory-info ()
  "Test unified project selection handling."
  ;; Test with no current project
  (let ((result (enkan-repl--target-directory-info 
                 nil 
                 '(("proj1" . ("alias1")))
                 '(("alias1" "Project One" . "/path/to/proj1"))
                 "Select project:"
                 nil)))
    (should (equal (plist-get result :status) 'no-project))
    (should (string-match-p "No current project" (plist-get result :message))))
  
  ;; Test with current project but no paths found
  (let ((result (enkan-repl--target-directory-info 
                 "test-project" 
                 '(("test-project" . ("nonexistent")))
                 '(("alias1" "Project One" . "/path/to/proj1"))
                 "Select project:"
                 nil)))
    (should (equal (plist-get result :status) 'no-paths))
    (should (string-match-p "No projects found" (plist-get result :message))))
  
  ;; Test with single path found
  (let ((result (enkan-repl--target-directory-info 
                 "test-project" 
                 '(("test-project" . ("alias1")))
                 '(("alias1" "Project One" . "/path/to/proj1"))
                 "Select project:"
                 nil)))
    (should (equal (plist-get result :status) 'single))
    (should (equal (plist-get result :path) "/path/to/proj1"))
    (should (equal (plist-get result :alias) "alias1")))
  
  ;; Test with validation failure
  (let ((result (enkan-repl--target-directory-info 
                 "test-project" 
                 '(("test-project" . ("alias1")))
                 '(("alias1" "Project One" . "/invalid/path"))
                 "Select project:"
                 (lambda (path) (string-prefix-p "/valid" path)))))
    (should (equal (plist-get result :status) 'invalid))
    (should (equal (plist-get result :path) "/invalid/path"))))

;; Tests for enkan-repl--select-project
(ert-deftest test-enkan-repl--select-project ()
  "Test project selection handling."
  ;; Test with no projects
  (let ((result (enkan-repl--select-project
                 '() "test-project" "Select:"
                 nil nil)))
    (should (equal (plist-get result :status) 'no-projects))
    (should (string-match-p "No projects found" (plist-get result :message))))

  ;; Test with single project - no validation
  (let ((result (enkan-repl--select-project
                 '(("alias1" . "/path/to/project1"))
                 "test-project"
                 "Select:"
                 nil nil)))
    (should (equal (plist-get result :status) 'single))
    (should (equal (plist-get result :path) "/path/to/project1"))
    (should (equal (plist-get result :alias) "alias1")))

  ;; Test with single project - validation fails
  (let ((result (enkan-repl--select-project
                 '(("alias1" . "/invalid/path"))
                 "test-project"
                 "Select:"
                 nil
                 (lambda (path) (string-prefix-p "/valid" path)))))
    (should (equal (plist-get result :status) 'invalid))
    (should (equal (plist-get result :path) "/invalid/path"))
    (should (string-match-p "Invalid path" (plist-get result :message)))))

;; Tests for enkan-repl--get-current-session-state-info
(ert-deftest test-enkan-repl--get-current-session-state-info ()
  "Test getting current session state information."
  ;; Test with sample state values
  (let ((current-project '("alias1" "alias2"))
        (session-list '((1 . "project1") (2 . "project2")))
        (session-counter 3)
        (project-aliases '(("alias1" . "Project One") ("alias2" . "Project Two"))))
    ;; Call the pure function with parameters
    (let ((state-info (enkan-repl--get-current-session-state-info
                       current-project session-list session-counter project-aliases)))
      ;; Verify the returned alist structure
      (should (equal (cdr (assoc 'current-project state-info)) current-project))
      (should (equal (cdr (assoc 'session-list state-info)) session-list))
      (should (equal (cdr (assoc 'session-counter state-info)) session-counter))
      (should (equal (cdr (assoc 'project-aliases state-info)) project-aliases))))

  ;; Test with nil values
  (let ((state-info (enkan-repl--get-current-session-state-info nil nil nil nil)))
    (should (null (cdr (assoc 'current-project state-info))))
    (should (null (cdr (assoc 'session-list state-info))))
    (should (null (cdr (assoc 'session-counter state-info))))
    (should (null (cdr (assoc 'project-aliases state-info)))))

  ;; Test with empty lists
  (let ((state-info (enkan-repl--get-current-session-state-info '() '() 0 '())))
    (should (equal (cdr (assoc 'current-project state-info)) '()))
    (should (equal (cdr (assoc 'session-list state-info)) '()))
    (should (equal (cdr (assoc 'session-counter state-info)) 0))
    (should (equal (cdr (assoc 'project-aliases state-info)) '()))))

(provide 'enkan-repl-functions-test)
;;; enkan-repl-functions-test.el ends here

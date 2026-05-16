;;; enkan-repl-workspace-create-test.el --- Test for workspace creation with project association -*- lexical-binding: t -*-

(require 'ert)
(require 'enkan-repl)

;; Test for Step A: Setup standardization

(ert-deftest test-enkan-repl--setup-create-workspace-with-project-standard-file ()
  "Test workspace creation for standard file mode."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (default-directory "/path/to/myproject/"))
    ;; Call function with standard file mode
    (let ((new-id (enkan-repl--setup-create-workspace-with-project t nil)))
      ;; Verify new workspace ID is created
      (should (string= new-id "01"))
      ;; Verify workspace is set as current
      (should (string= enkan-repl--current-workspace "01"))
      ;; Verify workspace exists in global state
      (should (assoc "01" enkan-repl--workspaces))
      ;; Verify project name is set correctly
      (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces))))
        (should (string= (plist-get ws-state :current-project) "myproject"))))))

(ert-deftest test-enkan-repl--setup-create-workspace-with-project-center-file ()
  "Test workspace creation for center file mode."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("TestProject" "alias1" "alias2"))))
    ;; Call function with center file mode
    (let ((new-id (enkan-repl--setup-create-workspace-with-project nil "TestProject")))
      ;; Verify new workspace ID is created
      (should (string= new-id "01"))
      ;; Verify workspace is set as current
      (should (string= enkan-repl--current-workspace "01"))
      ;; Verify workspace exists in global state
      (should (assoc "01" enkan-repl--workspaces))
      ;; Verify project name and aliases are set correctly
      (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces))))
        (should (string= (plist-get ws-state :current-project) "TestProject"))
        (should (equal (plist-get ws-state :project-aliases) '("alias1" "alias2")))))))

(ert-deftest test-enkan-repl--setup-create-workspace-with-project-multiple-workspaces ()
  "Test workspace ID increments correctly for multiple workspaces."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (default-directory "/path/to/project1/"))
    ;; Create first workspace
    (let ((id1 (enkan-repl--setup-create-workspace-with-project t nil)))
      (should (string= id1 "01"))
      ;; Create second workspace
      (let ((default-directory "/path/to/project2/"))
        (let ((id2 (enkan-repl--setup-create-workspace-with-project t nil)))
          (should (string= id2 "02"))
          ;; Verify both workspaces exist
          (should (= (length enkan-repl--workspaces) 2))
          ;; Verify current workspace is the latest
          (should (string= enkan-repl--current-workspace "02")))))))

(ert-deftest test-enkan-repl-setup-creates-workspace-standard-file ()
  "Test that enkan-repl-setup creates workspace for standard file."
  ;; Mock functions to avoid side effects
  (cl-letf* ((enkan-repl--workspaces nil)
             (enkan-repl--current-workspace nil)
             (buffer-file-name-result "/path/to/file.input.txt")
             (default-directory "/path/to/project/")
             (layout-called nil)
             ((symbol-function 'buffer-file-name) (lambda () buffer-file-name-result))
             ((symbol-function 'enkan-repl--is-standard-file-path) (lambda (file dir) t))
             ((symbol-function 'enkan-repl--initialize-default-workspace) (lambda ()))
             ((symbol-function 'delete-other-windows) (lambda ()))
             ((symbol-function 'split-window-right) (lambda ()))
             ((symbol-function 'other-window) (lambda (n)))
             ((symbol-function 'enkan-repl-start-session) (lambda (&optional force)))
             ((symbol-function 'enkan-repl-open-project-input-file) (lambda ()))
             ((symbol-function 'enkan-repl-setup-current-project-layout)
              (lambda ()
                (setq layout-called
                      (list enkan-repl--current-workspace
                            enkan-repl--current-project))))
             ((symbol-function 'message) (lambda (&rest args) nil)))
    ;; Execute enkan-repl-setup
    (enkan-repl-setup)
    ;; Verify workspace was created
    (should (string= enkan-repl--current-workspace "01"))
    (should (assoc "01" enkan-repl--workspaces))
    ;; Verify project name from directory
    (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces))))
      (should (string= (plist-get ws-state :current-project) "project")))
    (should (equal layout-called '("01" "project")))))

(ert-deftest test-enkan-repl-setup-creates-workspace-center-file ()
  "Test that enkan-repl-setup creates workspace for center file."
  ;; Mock functions to avoid side effects
  (cl-letf* ((enkan-repl--workspaces nil)
             (enkan-repl--current-workspace nil)
             (enkan-repl--current-project nil)  ; Reset current project
             (enkan-repl-project-aliases nil)   ; Reset project aliases
             (enkan-repl-center-file "hmenu")
             (enkan-repl-projects '(("MyProject" "alias1")))
             (buffer-file-name-result nil)
             (layout-called nil)
             ((symbol-function 'buffer-file-name) (lambda () buffer-file-name-result))
             ((symbol-function 'enkan-repl--is-standard-file-path) (lambda (file dir) nil))
             ((symbol-function 'enkan-repl--is-center-file-path) (lambda (file projects) t))
             ((symbol-function 'enkan-repl--initialize-default-workspace) (lambda ()))
             ((symbol-function 'hmenu) (lambda (prompt choices) "MyProject"))
             ((symbol-function 'with-output-to-temp-buffer) (lambda (name &rest body) (eval `(progn ,@body))))
             ((symbol-function 'princ) (lambda (&rest args) nil))
             ((symbol-function 'enkan-repl--setup-log-state) (lambda (&rest args) nil))
             ((symbol-function 'enkan-repl--setup-enable-global-mode) (lambda (buf) nil))
             ((symbol-function 'enkan-repl--setup-reset-config) (lambda (buf) nil))
             ((symbol-function 'enkan-repl--setup-set-project-aliases) (lambda (name aliases buf) nil))
             ((symbol-function 'enkan-repl--setup-start-sessions)
              (lambda (aliases buf)
                (list :success-count 1 :failure-count 0)))
             ((symbol-function 'enkan-repl-setup-current-project-layout)
              (lambda ()
                (setq layout-called
                      (list enkan-repl--current-workspace
                            enkan-repl--current-project))))
             ((symbol-function 'enkan-repl--ws-session-list) (lambda () nil))
             ((symbol-function 'enkan-repl--ws-session-counter) (lambda () 0))
             ((symbol-function 'message) (lambda (&rest args) nil)))
    ;; Execute enkan-repl-setup
    (enkan-repl-setup)
    ;; Verify workspace was created
    (should (string= enkan-repl--current-workspace "01"))
    (should (assoc "01" enkan-repl--workspaces))
    ;; Verify project name and aliases
    (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces))))
      (should (string= (plist-get ws-state :current-project) "MyProject"))
      (should (equal (plist-get ws-state :project-aliases) '("alias1"))))
    (should (equal layout-called '("01" "MyProject")))))

(ert-deftest test-enkan-repl-workspace-switch-runs-current-project-layout ()
  "Workspace switch should apply the current project's layout after loading."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (layout-called nil))
    (setq enkan-repl--workspaces
          (enkan-repl--add-workspace enkan-repl--workspaces "01"))
    (setq enkan-repl--current-workspace "01")
    (setq enkan-repl-session-list '((1 . "project-a")))
    (setq enkan-repl--session-counter 1)
    (setq enkan-repl--current-project "project-a")
    (enkan-repl--save-workspace-state "01")
    (setq enkan-repl--workspaces
          (enkan-repl--add-workspace enkan-repl--workspaces "02"))
    (setq enkan-repl--current-workspace "02")
    (setq enkan-repl-session-list '((1 . "project-b") (2 . "project-b")))
    (setq enkan-repl--session-counter 2)
    (setq enkan-repl--current-project "project-b")
    (enkan-repl--save-workspace-state "02")
    (setq enkan-repl--current-workspace "01")
    (enkan-repl--load-workspace-state "01")
    (cl-letf (((symbol-function 'hmenu)
               (lambda (prompt choices)
                 (car choices)))
              ((symbol-function 'enkan-repl-setup-current-project-layout)
               (lambda ()
                 (setq layout-called
                       (list enkan-repl--current-workspace
                             enkan-repl--current-project
                             enkan-repl-session-list))))
              ((symbol-function 'message) (lambda (&rest args) nil)))
      (enkan-repl-workspace-switch)
      (should (equal enkan-repl--current-workspace "02"))
      (should (equal layout-called
                     '("02" "project-b"
                       ((1 . "project-b") (2 . "project-b"))))))))

(ert-deftest test-enkan-repl--setup-start-sessions-reports-failures ()
  "Session setup should report failures instead of looking successful."
  (let ((buffer-name "*enkan-repl-test-setup-start*")
        (enkan-repl-target-directories nil))
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (let* ((standard-output (current-buffer))
                 (result (enkan-repl--setup-start-sessions
                          '("junk")
                          buffer-name)))
            (should (= 0 (plist-get result :success-count)))
            (should (= 1 (plist-get result :failure-count)))
            (should (string-match-p "Project alias .junk. not found"
                                    (buffer-string)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest test-enkan-repl--setup-window-terminal-buffer-pure-workspace-context ()
  "Test that window eat buffer name includes correct workspace ID."
  ;; Load window-layouts if exists
  (when (file-exists-p "/Users/sekine/dev/self/enkan-repl/examples/window-layouts.el")
    (load "/Users/sekine/dev/self/enkan-repl/examples/window-layouts.el" t t))
  
  ;; Test with workspace 01
  (let ((enkan-repl--current-workspace "01")
        (session-list '((1 . "project-a")))
        ;; project-registry format: ((alias . (project-name . project-path)) ...)
        (project-registry '(("alias1" . ("project-a" . "/path/to")))))
    (when (fboundp 'enkan-repl--setup-window-terminal-buffer-pure)
      (let ((result (enkan-repl--setup-window-terminal-buffer-pure
                     'dummy-window 1 session-list project-registry)))
        (when result
          ;; Buffer name should include ws:01
          (should (string-match-p "\\*ws:01 " (cdr result)))))))
  
  ;; Test with workspace 02
  (let ((enkan-repl--current-workspace "02")
        (session-list '((1 . "project-a")))
        ;; project-registry format: ((alias . (project-name . project-path)) ...)
        (project-registry '(("alias1" . ("project-a" . "/path/to")))))
    (when (fboundp 'enkan-repl--setup-window-terminal-buffer-pure)
      (let ((result (enkan-repl--setup-window-terminal-buffer-pure
                     'dummy-window 1 session-list project-registry)))
        (when result
          ;; Buffer name should include ws:02
          (should (string-match-p "\\*ws:02 " (cdr result))))))))

(provide 'enkan-repl-workspace-create-test)
;;; enkan-repl-workspace-create-test.el ends here

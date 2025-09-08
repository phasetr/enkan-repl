;;; enkan-repl-workspace-list-test.el --- Tests for workspace list display -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for workspace list display functionality

;;; Code:

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-workspace-list)

(ert-deftest test-workspace-list-format-info ()
  "Test formatting of workspace information."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "01")
         (enkan-repl-projects
          '(("config-a" . ("alias-a"))
            ("config-b" . ("alias-b"))))
         (enkan-repl-target-directories
          '(("alias-a" . ("project-a" . "/home/user/project-a/"))
            ("alias-b" . ("project-b" . "/home/user/project-b/"))))
         (test-state '(:current-project "config-a"
                       :project-aliases ("alias-a")
                       :session-list (("eat-1" . "*eat-1*"))
                       :session-counter 3)))
    ;; Setup workspace
    (setq enkan-repl--workspaces
          (list (cons "01" test-state)
                (cons "02" '(:current-project "config-b"
                             :project-aliases ("alias-b")
                             :session-list ()
                             :session-counter 0))))
    
    ;; Test formatting for current workspace
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "01" enkan-repl--workspaces "01" enkan-repl-target-directories)))
      (should (string-match-p "▶" formatted))
      ;; Check proper alignment
      (should (string-match-p "  ▶ 01" formatted))
      (should (string-match-p "config-a" formatted))
      (should (string-match-p "/home/user/project-a/" formatted)))
    
    ;; Test formatting for inactive workspace
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "02" enkan-repl--workspaces "01" enkan-repl-target-directories)))
      (should-not (string-match-p "▶" formatted))
      ;; Check proper alignment for inactive workspace
      (should (string-match-p "    02" formatted))
      (should (string-match-p "config-b" formatted))
      (should (string-match-p "/home/user/project-b/" formatted)))))

(ert-deftest test-workspace-list-buffer-creation ()
  "Test creation of workspace list buffer."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "01")
         (enkan-repl-projects
          '(("config-a" . ("alias-a"))))
         (enkan-repl-target-directories
          '(("alias-a" . ("project-a" . "/home/user/project-a/")))))
    ;; Setup workspace
    (setq enkan-repl--workspaces
          (list (cons "01" '(:current-project "config-a"
                             :project-aliases ("alias-a")
                             :session-list ()
                             :session-counter 1))))
    
    ;; Create list buffer
    (enkan-repl-workspace-list)
    
    (with-current-buffer "*Enkan Workspace List*"
      ;; Check buffer content
      (should (string-match-p "Enkan REPL Workspaces" (buffer-string)))
      (should (string-match-p "01" (buffer-string)))
      (should (string-match-p "config-a" (buffer-string)))
      
      ;; Check mode
      (should (eq major-mode 'enkan-repl-workspace-list-mode))
      (should buffer-read-only))
    
    ;; Clean up
    (kill-buffer "*Enkan Workspace List*")))

(ert-deftest test-workspace-list-keybindings ()
  "Test keybindings in workspace list mode."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "01")
         (enkan-repl-target-directories nil))
    ;; Setup workspace
    (setq enkan-repl--workspaces
          (list (cons "01" '(:current-project nil
                             :project-aliases nil
                             :session-list ()
                             :session-counter 0))))
    
    ;; Create list buffer
    (enkan-repl-workspace-list)
    
    (with-current-buffer "*Enkan Workspace List*"
      ;; Check keybindings are defined
      (should (keymapp enkan-repl-workspace-list-mode-map))
      (should (lookup-key enkan-repl-workspace-list-mode-map (kbd "RET")))
      (should (lookup-key enkan-repl-workspace-list-mode-map (kbd "g")))
      (should (lookup-key enkan-repl-workspace-list-mode-map (kbd "q"))))
    
    ;; Clean up
    (kill-buffer "*Enkan Workspace List*")))

(ert-deftest test-workspace-list-get-workspace-at-point ()
  "Test getting workspace ID from text property."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "01")
         (enkan-repl-target-directories nil))
    ;; Setup workspaces
    (setq enkan-repl--workspaces
          (list (cons "01" '(:current-project nil
                             :project-aliases nil
                             :session-list ()
                             :session-counter 0))
                (cons "02" '(:current-project nil
                             :project-aliases nil
                             :session-list ()
                             :session-counter 0))))
    
    ;; Create list buffer
    (enkan-repl-workspace-list)
    
    (with-current-buffer "*Enkan Workspace List*"
      ;; Move to first workspace line
      (goto-char (point-min))
      (forward-line 5)
      
      ;; Check we can get workspace ID
      (let ((ws-id (enkan-repl-workspace-list--get-workspace-at-point)))
        (should (stringp ws-id))
        (should (member ws-id '("01" "02")))))
    
    ;; Clean up
    (kill-buffer "*Enkan Workspace List*")))

(ert-deftest test-workspace-list-empty ()
  "Test workspace list when no workspaces exist."
  (let ((enkan-repl--workspaces nil))
    ;; Try to create list buffer
    (enkan-repl-workspace-list)
    
    ;; Should not create buffer when no workspaces
    (should-not (get-buffer "*Enkan Workspace List*"))))

(ert-deftest test-workspace-list-multiple-targets ()
  "Test formatting with multiple target directories in a project."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "01")
         (enkan-repl-projects
          '(("multi-project" . ("alias-a" "alias-b" "alias-c"))))
         (enkan-repl-target-directories
          '(("alias-a" . ("project-a" . "/home/user/project-a/"))
            ("alias-b" . ("project-b" . "/home/user/project-b/"))
            ("alias-c" . ("project-c" . "/home/user/project-c/")))))
    ;; Setup workspace with multiple targets
    (setq enkan-repl--workspaces
          (list (cons "01" '(:current-project "multi-project"
                             :project-aliases ("alias-a" "alias-b" "alias-c")
                             :session-list ()
                             :session-counter 0))))
    
    ;; Test formatting shows all paths
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "01" enkan-repl--workspaces "01" enkan-repl-target-directories)))
      (should (string-match-p "multi-project" formatted))
      (should (string-match-p "/home/user/project-a/" formatted))
      (should (string-match-p "/home/user/project-b/" formatted))
      (should (string-match-p "/home/user/project-c/" formatted)))))

(provide 'enkan-repl-workspace-list-test)

;;; enkan-repl-workspace-list-test.el ends here
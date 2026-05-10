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
      ;; Check format matches user's modification
      (should (string-match-p "▶ 01" formatted))
      (should (string-match-p "config-a" formatted))
      (should (string-match-p "/home/user/project-a/" formatted)))
    
    ;; Test formatting for inactive workspace
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "02" enkan-repl--workspaces "01" enkan-repl-target-directories)))
      (should-not (string-match-p "▶" formatted))
      ;; Check format for inactive workspace
      (should (string-match-p "   02" formatted))
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

(ert-deftest test-workspace-list-uses-project-alias-fallback ()
  "Workspace list should resolve paths from aliases when project name fails."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "05")
         (enkan-repl-projects nil)
         (enkan-repl-target-directories
          '(("er" . ("enkan-repl" . "/Users/me/dev/enkan-repl/")))))
    (setq enkan-repl--workspaces
          (list (cons "05" '(:current-project "er"
                             :project-aliases (("er" . "er"))
                             :session-list ()
                             :session-counter 0))))
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "05" enkan-repl--workspaces "05"
                      enkan-repl-target-directories)))
      (should (string-match-p "er" formatted))
      (should (string-match-p "/Users/me/dev/enkan-repl/" formatted))
      (should-not (string-match-p "<not found>" formatted)))))

(ert-deftest test-workspace-list-uses-state-target-directories ()
  "Workspace list should use target directories imported into workspace state."
  (let* ((enkan-repl--workspaces nil)
         (enkan-repl--current-workspace "02")
         (enkan-repl-projects nil)
         (enkan-repl-target-directories nil))
    (setq enkan-repl--workspaces
          (list (cons "02" '(:current-project "lat"
                             :project-aliases (("lat" . "lat"))
                             :target-directories
                             (("lat" . ("lat" . "/Users/me/dev/lat/")))
                             :session-list ()
                             :session-counter 0))))
    (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                      "02" enkan-repl--workspaces "02"
                      enkan-repl-target-directories)))
      (should (string-match-p "lat" formatted))
      (should (string-match-p "/Users/me/dev/lat/" formatted))
      (should-not (string-match-p "<not found>" formatted)))))

(ert-deftest test-workspace-list-keeps-imported-paths-after-repeated-switches ()
  "Repeated workspace switching should not make imported paths disappear."
  (let ((enkan-repl--workspaces
         '(("02" . (:current-project "lat"
                    :project-aliases (("lat" . "lat"))
                    :target-directories
                    (("lat" . ("lat" . "/Users/me/dev/lat/")))
                    :session-list ((1 . "lat"))
                    :session-counter 1))
           ("05" . (:current-project "er"
                    :project-aliases (("er" . "er"))
                    :target-directories
                    (("er" . ("er" . "/Users/me/dev/enkan-repl/")))
                    :session-list ((1 . "er"))
                    :session-counter 1))))
        (enkan-repl--current-workspace "05")
        (enkan-repl--current-project nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-project-aliases nil)
        (enkan-repl-target-directories nil))
    (dotimes (_ 3)
      (enkan-repl--load-workspace-state "05")
      (enkan-repl--save-workspace-state "05")
      (setq enkan-repl--current-workspace "02")
      (enkan-repl--load-workspace-state "02")
      (enkan-repl--save-workspace-state "02")
      (setq enkan-repl--current-workspace "05"))
    (dolist (workspace-id '("02" "05"))
      (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                        workspace-id enkan-repl--workspaces "05" nil)))
        (should-not (string-match-p "<not found>" formatted))))))

(ert-deftest test-workspace-list-resolves-normalized-persisted-alias-paths ()
  "Workspace list should resolve reattached paths by persisted project alias."
  (let ((enkan-repl--workspaces
         '(("02" . (:current-project "lat"
                    :project-aliases (("lat" . "lat"))
                    :target-directories
                    (("lat" . ("lat" . "/Users/me/dev/lattice-system"))
                     ("lattice-system" .
                      ("lattice-system" . "/Users/me/dev/lattice-system")))
                    :session-list ((1 . "lat"))
                    :session-counter 0))
           ("05" . (:current-project "er"
                    :project-aliases (("er" . "enkan-repl"))
                    :target-directories
                    (("er" . ("enkan-repl" . "/Users/me/dev/enkan-repl"))
                     ("enkan-repl" .
                      ("enkan-repl" . "/Users/me/dev/enkan-repl")))
                    :session-list ((1 . "enkan-repl"))
                    :session-counter 0))))
        (enkan-repl-projects nil))
    (dolist (workspace-id '("02" "05"))
      (let ((formatted (enkan-repl-workspace-list--format-workspace-info
                        workspace-id enkan-repl--workspaces "05" nil)))
        (should-not (string-match-p "<not found>" formatted))
        (should (string-match-p "/Users/me/dev/" formatted))))))

(provide 'enkan-repl-workspace-list-test)

;;; enkan-repl-workspace-list-test.el ends here

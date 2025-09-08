;;; enkan-repl-workspace-real-scenario-test.el --- Test real scenario -*- lexical-binding: t -*-

;;; Commentary:
;; Test the exact scenario user reported

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)

(ert-deftest test-real-scenario-ws02-zero-sessions ()
  "Test the exact scenario: WS 02 shows 0 sessions after switch."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (default-directory "/path/to/er/"))
    
    (message "\n=== User creates WS 01 ===")
    ;; User runs enkan-repl-setup first time
    (enkan-repl--setup-create-workspace-with-project t nil)
    (message "WS 01 created, ID: %s" enkan-repl--current-workspace)
    
    ;; User starts eat session with C-M-s
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
               (lambda () (generate-new-buffer "*eat*")))
              ((symbol-function 'require)
               (lambda (_) t)))
      (enkan-repl-start-eat))
    (message "WS 01 sessions: %S" enkan-repl-session-list)
    
    (message "\n=== User creates WS 02 ===")
    ;; User runs enkan-repl-setup second time
    (enkan-repl--setup-create-workspace-with-project t nil)
    (message "WS 02 created, ID: %s" enkan-repl--current-workspace)
    
    ;; User starts eat session with C-M-s
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
               (lambda () (generate-new-buffer "*eat*")))
              ((symbol-function 'require)
               (lambda (_) t)))
      (enkan-repl-start-eat))
    (message "WS 02 sessions: %S" enkan-repl-session-list)
    
    (message "\n=== User switches to WS 02 ===")
    ;; User switches workspace (maybe not using the proper command)
    (setq enkan-repl--current-workspace "02")
    
    (message "\n=== User runs enkan-repl-workspace-list ===")
    ;; This is what happens when user runs workspace-list
    (enkan-repl--save-workspace-state) ; workspace-list does this
    
    ;; Check what workspace-list would show
    (dolist (ws-id '("01" "02"))
      (let* ((state (enkan-repl--get-workspace-state enkan-repl--workspaces ws-id))
             (session-count (length (plist-get state :session-list))))
        (message "WS %s: Sessions=%d" ws-id session-count)
        ;; Both should have 1 session
        (should (= 1 session-count))))))

(ert-deftest test-workspace-switch-timing-issue ()
  "Test if there's a timing issue with workspace creation and session start."
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-session-list nil)
        (enkan-repl--session-counter 0)
        (enkan-repl--current-project nil)
        (default-directory "/path/to/er/"))
    
    ;; Create WS 01
    (enkan-repl--setup-create-workspace-with-project t nil)
    
    ;; Problem: What if user creates WS 02 BEFORE starting eat in WS 01?
    (enkan-repl--setup-create-workspace-with-project t nil)
    
    ;; Now user starts eat in WS 02
    (cl-letf (((symbol-function 'enkan-repl--backend-eat-start)
               (lambda () (generate-new-buffer "*eat*")))
              ((symbol-function 'require)
               (lambda (_) t)))
      (enkan-repl-start-eat))
    
    ;; Check states
    (let ((ws01-state (enkan-repl--get-workspace-state enkan-repl--workspaces "01"))
          (ws02-state (enkan-repl--get-workspace-state enkan-repl--workspaces "02")))
      (message "WS 01 sessions: %d" (length (plist-get ws01-state :session-list)))
      (message "WS 02 sessions: %d" (length (plist-get ws02-state :session-list)))
      ;; WS 01 should have 0, WS 02 should have 1
      (should (= 0 (length (plist-get ws01-state :session-list))))
      (should (= 1 (length (plist-get ws02-state :session-list)))))))

(provide 'enkan-repl-workspace-real-scenario-test)
;;; enkan-repl-workspace-real-scenario-test.el ends here
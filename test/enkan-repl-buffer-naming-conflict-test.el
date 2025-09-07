;;; enkan-repl-buffer-naming-conflict-test.el --- Test for buffer naming conflicts with same path -*- lexical-binding: t -*-

(require 'ert)
(require 'enkan-repl)
(require 'enkan-repl-utils)

(ert-deftest test-enkan-repl-buffer-naming-conflict-same-path ()
  "Test that buffers for the same path in different workspaces get unique names."
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (test-path "/path/to/enkan-repl/"))
    ;; Create first workspace and generate buffer name
    (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                             :session-list nil
                                             :session-counter 0
                                             :project-aliases nil))))
    (setq enkan-repl--current-workspace "01")
    (let ((buffer-name-1 (enkan-repl--path->buffer-name test-path)))
      ;; Buffer name should be for workspace 01
      (should (string= buffer-name-1 "*ws:01 enkan:/path/to/enkan-repl/*"))
      
      ;; Create second workspace and generate buffer name for same path
      (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))
                                      ("02" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))))
      (setq enkan-repl--current-workspace "02")
      (let ((buffer-name-2 (enkan-repl--path->buffer-name test-path)))
        ;; Buffer name should be for workspace 02
        (should (string= buffer-name-2 "*ws:02 enkan:/path/to/enkan-repl/*"))
        ;; The two buffer names should be different
        (should-not (string= buffer-name-1 buffer-name-2))))))

(ert-deftest test-enkan-repl-buffer-creation-with-multiple-workspaces ()
  "Test that eat buffers are created with correct names for multiple workspaces."
  ;; This test simulates the scenario where two workspaces have the same project
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (buffer-names '()))
    ;; Simulate creating first workspace
    (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                             :session-list nil
                                             :session-counter 0
                                             :project-aliases nil))))
    (setq enkan-repl--current-workspace "01")
    ;; Create buffer name for first workspace
    (let ((name1 (enkan-repl--path->buffer-name "/path/to/enkan-repl/")))
      (push name1 buffer-names)
      
      ;; Simulate creating second workspace
      (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))
                                      ("02" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))))
      (setq enkan-repl--current-workspace "02")
      ;; Create buffer name for second workspace
      (let ((name2 (enkan-repl--path->buffer-name "/path/to/enkan-repl/")))
        (push name2 buffer-names)
        
        ;; Verify both names are unique
        (should (= (length buffer-names) 2))
        (should (= (length (delete-dups (copy-sequence buffer-names))) 2))
        ;; Verify correct format
        (should (string-match-p "\\*ws:01 enkan:/path/to/enkan-repl/\\*" name1))
        (should (string-match-p "\\*ws:02 enkan:/path/to/enkan-repl/\\*" name2))))))

(ert-deftest test-enkan-repl-buffer-naming-with-workspace-context ()
  "Test that buffer naming respects current workspace context."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "project-a"))
                                   ("02" . (:current-project "project-b"))
                                   ("03" . (:current-project "project-c"))))
        (enkan-repl--current-workspace nil))
    ;; Test with workspace 01
    (setq enkan-repl--current-workspace "01")
    (should (string-match-p "\\*ws:01 " (enkan-repl--path->buffer-name "/some/path/")))
    
    ;; Test with workspace 02
    (setq enkan-repl--current-workspace "02")
    (should (string-match-p "\\*ws:02 " (enkan-repl--path->buffer-name "/some/path/")))
    
    ;; Test with workspace 03
    (setq enkan-repl--current-workspace "03")
    (should (string-match-p "\\*ws:03 " (enkan-repl--path->buffer-name "/some/path/")))))

(ert-deftest test-actual-workspace-buffer-naming-problem ()
  "Test the actual problem: existing eat buffers cannot be found after workspace switch."
  
  (let ((enkan-repl--workspaces '())
        (enkan-repl--current-workspace nil)
        (enkan-repl-projects '(("er" . ("er-alias"))))
        (enkan-repl-target-directories '(("er-alias" . ("er" . "/path/to/er"))))
        (buffer1 nil)
        (buffer2 nil))
    
    (unwind-protect
        (progn
          ;; Setup workspace 01
          (setq enkan-repl--workspaces 
                (enkan-repl--add-workspace enkan-repl--workspaces "01"))
          (setq enkan-repl--current-workspace "01")
          (setq enkan-repl-session-list '((1 . "er")))
          (setq enkan-repl--session-counter 1)
          (setq enkan-repl--current-project "er")
          (setq enkan-repl-project-aliases '("er-alias"))
          (enkan-repl--save-workspace-state "01")
          
          ;; Create actual eat buffer for ws:01
          ;; Note: The path might end with "/" or not - this is the issue!
          (setq buffer1 (generate-new-buffer "*ws:01 enkan:/path/to/er/*"))
          (should (buffer-live-p buffer1))
          
          ;; Setup workspace 02
          (setq enkan-repl--workspaces 
                (enkan-repl--add-workspace enkan-repl--workspaces "02"))
          (setq enkan-repl--current-workspace "02")
          (setq enkan-repl-session-list '((1 . "er")))
          (setq enkan-repl--session-counter 1)
          (setq enkan-repl--current-project "er")
          (setq enkan-repl-project-aliases '("er-alias"))
          (enkan-repl--save-workspace-state "02")
          
          ;; Create actual eat buffer for ws:02
          (setq buffer2 (generate-new-buffer "*ws:02 enkan:/path/to/er/*"))
          (should (buffer-live-p buffer2))
          
          ;; Switch back to ws:01 and load state
          (setq enkan-repl--current-workspace "01")
          (enkan-repl--load-workspace-state "01")
          
          ;; Test: setup-window-eat-buffer-pure should find the correct buffer
          (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
            (let ((result (enkan-repl--setup-window-eat-buffer-pure
                           'test-window 1
                           enkan-repl-session-list
                           enkan-repl-target-directories)))
              (should result)
              (should (equal "*ws:01 enkan:/path/to/er/*" (cdr result)))
              ;; The buffer should be findable
              (should (get-buffer (cdr result)))))
          
          ;; Switch to ws:02 and load state
          (setq enkan-repl--current-workspace "02")
          (enkan-repl--load-workspace-state "02")
          
          ;; Test: setup-window-eat-buffer-pure should find the correct buffer
          (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
            (let ((result (enkan-repl--setup-window-eat-buffer-pure
                           'test-window 1
                           enkan-repl-session-list
                           enkan-repl-target-directories)))
              (should result)
              (should (equal "*ws:02 enkan:/path/to/er/*" (cdr result)))
              ;; The buffer should be findable
              (should (get-buffer (cdr result))))))
      
      ;; Clean up
      (when (buffer-live-p buffer1) (kill-buffer buffer1))
      (when (buffer-live-p buffer2) (kill-buffer buffer2)))))

(provide 'enkan-repl-buffer-naming-conflict-test)
;;; enkan-repl-buffer-naming-conflict-test.el ends here
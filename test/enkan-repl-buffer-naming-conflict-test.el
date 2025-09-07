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


(provide 'enkan-repl-buffer-naming-conflict-test)
;;; enkan-repl-buffer-naming-conflict-test.el ends here

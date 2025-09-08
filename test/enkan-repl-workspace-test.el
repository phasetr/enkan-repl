;;; enkan-repl-workspace-test.el --- Tests for workspace utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for enkan-repl-workspace.el pure functions

;;; Code:

(require 'ert)
(require 'enkan-repl-workspace)

(ert-deftest test-enkan-repl--generate-next-workspace-id ()
  "Test workspace ID generation."
  ;; Test with nil workspaces
  (should (string= "01" (enkan-repl--generate-next-workspace-id nil)))
  
  ;; Test with empty list
  (should (string= "01" (enkan-repl--generate-next-workspace-id '())))
  
  ;; Test with single workspace
  (should (string= "02" (enkan-repl--generate-next-workspace-id '(("01" . nil)))))
  
  ;; Test with multiple workspaces
  (should (string= "04" (enkan-repl--generate-next-workspace-id
                         '(("01" . nil) ("02" . nil) ("03" . nil)))))
  
  ;; Test with non-sequential IDs
  (should (string= "06" (enkan-repl--generate-next-workspace-id
                         '(("01" . nil) ("03" . nil) ("05" . nil))))))

(ert-deftest test-enkan-repl--add-workspace ()
  "Test adding workspace to alist."
  (let ((workspaces '()))
    ;; Add to empty list
    (setq workspaces (enkan-repl--add-workspace workspaces "01"))
    (should (equal 1 (length workspaces)))
    (should (assoc "01" workspaces))
    (let ((state (cdr (assoc "01" workspaces))))
      (should (plist-member state :current-project))
      (should (equal nil (plist-get state :current-project))))
    
    ;; Add another workspace
    (setq workspaces (enkan-repl--add-workspace workspaces "02"))
    (should (equal 2 (length workspaces)))
    (should (assoc "02" workspaces))))

(ert-deftest test-enkan-repl--get-workspace-state ()
  "Test retrieving workspace state."
  (let ((workspaces '(("01" :current-project "project1"
                            :session-list ("session1")
                            :session-counter 1
                            :project-aliases nil)
                      ("02" :current-project nil
                            :session-list nil
                            :session-counter 0
                            :project-aliases nil))))
    ;; Get existing workspace
    (should (equal '(:current-project "project1"
                     :session-list ("session1")
                     :session-counter 1
                     :project-aliases nil)
                   (enkan-repl--get-workspace-state workspaces "01")))
    
    ;; Get non-existing workspace
    (should (null (enkan-repl--get-workspace-state workspaces "03")))))

(ert-deftest test-enkan-repl--can-delete-workspace ()
  "Test workspace deletion validation."
  (let ((workspaces '(("01" . nil) ("02" . nil))))
    ;; Can delete when multiple workspaces exist
    (should (enkan-repl--can-delete-workspace "01" workspaces))
    (should (enkan-repl--can-delete-workspace "02" workspaces))
    
    ;; Cannot delete non-existing workspace
    (should-not (enkan-repl--can-delete-workspace "03" workspaces))
    
    ;; Cannot delete when only one workspace
    (let ((single-ws '(("01" . nil))))
      (should-not (enkan-repl--can-delete-workspace "01" single-ws)))))

(ert-deftest test-enkan-repl--delete-workspace ()
  "Test workspace deletion."
  (let ((workspaces '(("01" . nil) ("02" . nil) ("03" . nil))))
    ;; Delete middle workspace
    (setq workspaces (enkan-repl--delete-workspace workspaces "02"))
    (should (equal 2 (length workspaces)))
    (should-not (assoc "02" workspaces))
    (should (assoc "01" workspaces))
    (should (assoc "03" workspaces))
    
    ;; Delete first workspace
    (setq workspaces (enkan-repl--delete-workspace workspaces "01"))
    (should (equal 1 (length workspaces)))
    (should-not (assoc "01" workspaces))
    (should (assoc "03" workspaces))))

(ert-deftest test-enkan-repl--list-workspace-ids ()
  "Test listing workspace IDs."
  ;; Test with nil
  (should (null (enkan-repl--list-workspace-ids nil)))
  
  ;; Test with empty list
  (should (null (enkan-repl--list-workspace-ids '())))
  
  ;; Test with workspaces in order
  (should (equal '("01" "02" "03")
                 (enkan-repl--list-workspace-ids
                  '(("01" . nil) ("02" . nil) ("03" . nil)))))
  
  ;; Test with workspaces out of order
  (should (equal '("01" "02" "03")
                 (enkan-repl--list-workspace-ids
                  '(("03" . nil) ("01" . nil) ("02" . nil))))))

(provide 'enkan-repl-workspace-test)
;;; enkan-repl-workspace-test.el ends here
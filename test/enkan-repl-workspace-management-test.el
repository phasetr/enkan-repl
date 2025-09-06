;;; enkan-repl-workspace-management-test.el --- Tests for workspace management functions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for workspace creation, switching, and deletion functions

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;; Pure function tests

(ert-deftest test-enkan-repl--generate-next-workspace-id ()
  "Test workspace ID generation."
  ;; Empty workspaces should return "01"
  (should (equal "01" (enkan-repl--generate-next-workspace-id nil)))
  
  ;; With existing workspaces, should increment
  (should (equal "02" (enkan-repl--generate-next-workspace-id 
                       '(("01" . (:current-project nil))))))
  
  ;; Should handle gaps and return next sequential
  (should (equal "04" (enkan-repl--generate-next-workspace-id 
                       '(("01" . (:current-project nil))
                         ("03" . (:current-project nil))))))
  
  ;; Should handle out-of-order alist
  (should (equal "04" (enkan-repl--generate-next-workspace-id 
                       '(("03" . (:current-project nil))
                         ("01" . (:current-project nil))
                         ("02" . (:current-project nil)))))))

(ert-deftest test-enkan-repl--create-workspace-state ()
  "Test workspace state creation."
  (let ((new-id (enkan-repl--create-workspace-state nil)))
    ;; Should return "01" for empty workspaces
    (should (equal "01" new-id)))
  
  (let* ((existing '(("01" . (:current-project nil))))
         (new-id (enkan-repl--create-workspace-state existing)))
    ;; Should return "02" when "01" exists
    (should (equal "02" new-id)))
  
  ;; Should create empty state structure
  (let* ((workspaces nil)
         (new-id (enkan-repl--create-workspace-state workspaces))
         (updated (enkan-repl--add-workspace workspaces new-id)))
    (should (equal new-id "01"))
    (let ((state (cdr (assoc new-id updated))))
      (should (plist-member state :current-project))
      (should (equal nil (plist-get state :current-project)))
      (should (equal nil (plist-get state :session-list)))
      (should (equal 0 (plist-get state :session-counter)))
      (should (equal nil (plist-get state :project-aliases))))))

(ert-deftest test-enkan-repl--add-workspace ()
  "Test adding workspace to alist."
  ;; Add to empty alist
  (let ((workspaces nil)
        (new-id "01"))
    (let ((updated (enkan-repl--add-workspace workspaces new-id)))
      (should (equal 1 (length updated)))
      (should (assoc new-id updated))))
  
  ;; Add to existing alist
  (let ((workspaces '(("01" . (:current-project nil))))
        (new-id "02"))
    (let ((updated (enkan-repl--add-workspace workspaces new-id)))
      (should (equal 2 (length updated)))
      (should (assoc new-id updated)))))

(ert-deftest test-enkan-repl--switch-workspace ()
  "Test workspace switching logic."
  ;; Test switching between workspaces
  (let ((workspaces '(("01" . (:current-project "proj1"
                               :session-list ((1 . "proj1"))
                               :session-counter 1
                               :project-aliases (("p1" . "proj1"))))
                     ("02" . (:current-project "proj2"
                               :session-list ((2 . "proj2"))
                               :session-counter 2
                               :project-aliases (("p2" . "proj2"))))))
        (current-id "01")
        (target-id "02"))
    ;; Should return plist for loading
    (let ((to-load (enkan-repl--get-workspace-state workspaces target-id)))
      (should (equal "proj2" (plist-get to-load :current-project)))
      (should (equal '((2 . "proj2")) (plist-get to-load :session-list)))
      (should (equal 2 (plist-get to-load :session-counter)))
      (should (equal '(("p2" . "proj2")) (plist-get to-load :project-aliases))))))

(ert-deftest test-enkan-repl--can-delete-workspace ()
  "Test workspace deletion validation."
  ;; Cannot delete if only one workspace
  (should-not (enkan-repl--can-delete-workspace "01" '(("01" . (:current-project nil)))))
  
  ;; Can delete if multiple workspaces exist
  (should (enkan-repl--can-delete-workspace 
           "01" 
           '(("01" . (:current-project nil))
             ("02" . (:current-project nil)))))
  
  ;; Cannot delete non-existent workspace
  (should-not (enkan-repl--can-delete-workspace 
                "03" 
                '(("01" . (:current-project nil))
                  ("02" . (:current-project nil))))))

(ert-deftest test-enkan-repl--delete-workspace ()
  "Test workspace deletion."
  (let ((workspaces '(("01" . (:current-project "proj1"))
                     ("02" . (:current-project "proj2"))
                     ("03" . (:current-project "proj3"))))
        (to-delete "02"))
    (let ((updated (enkan-repl--delete-workspace workspaces to-delete)))
      ;; Should remove the workspace
      (should (equal 2 (length updated)))
      (should-not (assoc to-delete updated))
      ;; Should keep others
      (should (assoc "01" updated))
      (should (assoc "03" updated)))))

(ert-deftest test-enkan-repl--list-workspace-ids ()
  "Test listing workspace IDs."
  ;; Empty list
  (should (equal nil (enkan-repl--list-workspace-ids nil)))
  
  ;; Single workspace
  (should (equal '("01") 
                 (enkan-repl--list-workspace-ids 
                  '(("01" . (:current-project nil))))))
  
  ;; Multiple workspaces (should be sorted)
  (should (equal '("01" "02" "03")
                 (enkan-repl--list-workspace-ids 
                  '(("02" . (:current-project nil))
                    ("01" . (:current-project nil))
                    ("03" . (:current-project nil)))))))

(ert-deftest test-enkan-repl--get-workspace-state ()
  "Test getting workspace state."
  (let ((workspaces '(("01" . (:current-project "proj1"
                               :session-list ((1 . "proj1"))
                               :session-counter 1
                               :project-aliases nil)))))
    ;; Should return state for existing workspace
    (let ((state (enkan-repl--get-workspace-state workspaces "01")))
      (should (equal "proj1" (plist-get state :current-project)))
      (should (equal '((1 . "proj1")) (plist-get state :session-list))))
    
    ;; Should return nil for non-existent workspace
    (should-not (enkan-repl--get-workspace-state workspaces "02"))))

(ert-deftest test-enkan-repl--initialize-default-workspace ()
  "Test default workspace initialization."
  ;; Test initialization when no workspaces exist
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-project nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-session-list nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects '(("project1" ("alias1" . "/path1"))
                              ("project2" ("alias2" . "/path2")))))
    ;; Should initialize with first project
    (should (enkan-repl--initialize-default-workspace))
    (should enkan-repl--workspaces)
    (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces #'string=))))
      (should ws-state)
      (should (equal "project1" (plist-get ws-state :current-project)))))
  
  ;; Test when workspaces already exist
  (let ((enkan-repl--workspaces '(("01" :current-project "existing"))))
    (should-not (enkan-repl--initialize-default-workspace)))
  
  ;; Test when no projects defined
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-project nil)
        (enkan-repl--session-counter 0)
        (enkan-repl-session-list nil)
        (enkan-repl-project-aliases nil)
        (enkan-repl-projects nil))
    (should (enkan-repl--initialize-default-workspace))
    (should enkan-repl--workspaces)
    (let ((ws-state (cdr (assoc "01" enkan-repl--workspaces #'string=))))
      (should ws-state)
      (should (null (plist-get ws-state :current-project))))))

(provide 'enkan-repl-workspace-management-test)
;;; enkan-repl-workspace-management-test.el ends here
;; test/project-path-lookup-test.el
;; Test project path lookup functionality

(require 'ert)
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'enkan-repl)

(ert-deftest test-project-path-from-registry-lookup ()
  "Test that enkan-repl--get-project-path-from-registry works correctly."
  (let ((test-registry '(("alias1" . ("Project One" . "/path/to/project1"))
                         ("alias2" . ("Project Two" . "/path/to/project2"))
                         ("alias3" . ("Another Project" . "/path/to/project3")))))
    
    ;; Test successful lookup
    (should (string= "/path/to/project1"
                     (enkan-repl--get-project-path-from-registry "Project One" test-registry)))
    (should (string= "/path/to/project2"
                     (enkan-repl--get-project-path-from-registry "Project Two" test-registry)))
    (should (string= "/path/to/project3"
                     (enkan-repl--get-project-path-from-registry "Another Project" test-registry)))
    
    ;; Test failed lookup
    (should (null (enkan-repl--get-project-path-from-registry "Nonexistent Project" test-registry)))))

(ert-deftest test-center-finish-project-path-resolution ()
  "Test that center finish function correctly resolves project paths from project names."
  (let ((enkan-repl-center-project-registry 
         '(("er" . ("enkan-repl" . "/home/user/enkan-repl"))
           ("pt" . ("pt-tools" . "/home/user/pt-tools"))))
        (enkan-repl-session-list '((1 . "enkan-repl") (2 . "pt-tools")))
        (resolved-paths '()))
    
    ;; Mock enkan-repl--get-buffer-for-directory to capture resolved paths
    (cl-letf (((symbol-function 'enkan-repl--get-buffer-for-directory)
               (lambda (directory)
                 (push directory resolved-paths)
                 ;; Return mock buffer
                 (get-buffer-create (format "*mock-%s*" directory))))
              ((symbol-function 'kill-buffer)
               (lambda (buffer) t))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t)))
      
      ;; Execute the function
      (enkan-repl-center-finish-all-sessions)
      
      ;; Verify correct project paths were resolved and used
      (should (member "/home/user/enkan-repl" resolved-paths))
      (should (member "/home/user/pt-tools" resolved-paths))
      (should (= 2 (length resolved-paths))))))

(provide 'project-path-lookup-test)
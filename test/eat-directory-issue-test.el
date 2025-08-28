;;; eat-directory-issue-test.el --- Test eat directory initialization issue

;;; Code:

(require 'ert)
(load "./enkan-repl.el")

(ert-deftest test-get-target-directory-center-file-issue ()
  "Test that center file current buffer doesn't affect target directory determination."
  (let* ((center-file-path "/Users/sekine/dev/self/enkan-repl/test-center.org")
         (pt-tools-path "/Users/sekine/pt-tools")
         (enkan-repl-center-file center-file-path))
    
    ;; Create temporary center file for testing
    (with-temp-file center-file-path
      (insert "# Test center file"))
    
    (unwind-protect
        (with-temp-buffer
          ;; Open the center file buffer
          (find-file center-file-path)
          
          ;; Verify we're in center file buffer
          (should (string= (expand-file-name buffer-file-name)
                          (expand-file-name center-file-path)))
          
          ;; The issue: current directory is center file's directory
          (should (string= default-directory "/Users/sekine/dev/self/enkan-repl/"))
          
          ;; Test current behavior - this demonstrates the bug
          (let ((target-dir (enkan-repl--get-target-directory-for-buffer)))
            ;; Current buggy behavior: returns center file directory
            (should (string= target-dir "/Users/sekine/dev/self/enkan-repl/"))))
      
      ;; Clean up temporary file
      (when (file-exists-p center-file-path)
        (delete-file center-file-path)))))

(ert-deftest test-get-target-directory-should-use-project-context ()
  "Test that target directory should be determined by project context, not current buffer."
  ;; This test demonstrates what the correct behavior should be
  ;; When we're in a multi-project session, target directory should be
  ;; determined by the project being set up, not by current buffer
  
  (let* ((center-file-path "/tmp/test-center.org")
         (pt-tools-path "/Users/sekine/pt-tools")
         (enkan-repl-center-file center-file-path)
         (enkan-repl-project-aliases '(("pt" . "pt-tools")))
         (enkan-repl-center-project-registry '(("pt" . ("pt-tools" . "/Users/sekine/pt-tools")))))
    
    ;; Create temporary center file
    (with-temp-file center-file-path
      (insert "# Test center file"))
    
    (unwind-protect
        (progn
          ;; Test in a clean buffer context
          (with-temp-buffer
            ;; Set up as if we're in center file context
            (setq buffer-file-name center-file-path)
            (setq default-directory (file-name-directory center-file-path))
            
            ;; Current behavior: center file takes precedence
            (let ((target-dir-center (enkan-repl--get-target-directory-for-buffer)))
              (should (string= target-dir-center (file-name-directory center-file-path))))
            
            ;; When setting up pt-tools session, we should temporarily override
            ;; target directory to be pt-tools path, not center file path
            (let ((default-directory pt-tools-path))
              ;; This should return pt-tools path when default-directory is overridden
              (let ((target-dir (enkan-repl--get-target-directory-for-buffer)))
                ;; Expected behavior: should return pt-tools path when default-directory is set
                (should (string= target-dir pt-tools-path))))))
      
      ;; Clean up
      (when (file-exists-p center-file-path)
        (delete-file center-file-path)))))

;;; eat-directory-issue-test.el ends here
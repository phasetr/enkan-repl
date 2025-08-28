;;; alias-resolution-fix-test.el --- Test exact match alias resolution

;;; Code:

(require 'ert)
(load "./enkan-repl.el")

(ert-deftest test-alias-resolution-exact-match ()
  "Test that alias resolution uses exact match instead of partial match."
  (let ((test-buffer-er (generate-new-buffer "*enkan:/Users/sekine/dev/self/enkan-repl*"))
        (test-buffer-pt (generate-new-buffer "*enkan:/Users/sekine/pt-tools*"))
        (enkan-repl-project-aliases '(("er" . "enkan-repl") ("pt" . "pt-tools"))))
    
    (unwind-protect
        (let ((enkan-buffers (list test-buffer-er test-buffer-pt)))
          ;; Test er alias should resolve to enkan-repl buffer exactly
          (let ((resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure "er" enkan-buffers)))
            (should (eq resolved-buffer test-buffer-er))
            (should (string= (buffer-name resolved-buffer) "*enkan:/Users/sekine/dev/self/enkan-repl*")))
          
          ;; Test pt alias should resolve to pt-tools buffer exactly
          (let ((resolved-buffer (enkan-repl-center--resolve-alias-to-buffer-pure "pt" enkan-buffers)))
            (should (eq resolved-buffer test-buffer-pt))
            (should (string= (buffer-name resolved-buffer) "*enkan:/Users/sekine/pt-tools*")))
          
          ;; Test non-existent alias
          (should (null (enkan-repl-center--resolve-alias-to-buffer-pure "nonexistent" enkan-buffers))))
      
      ;; Clean up
      (kill-buffer test-buffer-er)
      (kill-buffer test-buffer-pt))))

(ert-deftest test-extract-project-name-functionality ()
  "Test that enkan-repl--extract-project-name works correctly."
  (should (string= (enkan-repl--extract-project-name "*enkan:/Users/sekine/dev/self/enkan-repl*") "enkan-repl"))
  (should (string= (enkan-repl--extract-project-name "*enkan:/Users/sekine/pt-tools*") "pt-tools"))
  (should (string= (enkan-repl--extract-project-name "*enkan:/path/to/some-project*") "some-project")))

;;; alias-resolution-fix-test.el ends here
;;; enkan-repl-eat-buffer-creation-test.el --- Test for eat buffer creation with workspaces -*- lexical-binding: t -*-

(require 'ert)
(require 'enkan-repl)

(ert-deftest test-enkan-repl-eat-buffer-naming-with-same-project ()
  "Test that eat buffers for same project in different workspaces have correct names."
  ;; Mock eat and rename-buffer functions to track actual behavior
  (let ((enkan-repl--workspaces nil)
        (enkan-repl--current-workspace nil)
        (created-buffers '())
        (renamed-buffers '())
        (existing-buffers '()))
    ;; Mock functions
    (cl-letf* (((symbol-function 'eat)
                (lambda ()
                  (let ((buf (generate-new-buffer "*eat*")))
                    (push buf created-buffers)
                    buf)))
               ((symbol-function 'rename-buffer)
                (lambda (name &optional unique)
                  (push (list (current-buffer) name unique) renamed-buffers)
                  ;; Simulate Emacs behavior: if unique is t and buffer exists, add <N>
                  (if unique
                      (if (member name existing-buffers)
                          (let ((n 2)
                                (new-name (format "%s<%d>" name n)))
                            (while (member new-name existing-buffers)
                              (setq n (1+ n))
                              (setq new-name (format "%s<%d>" name n)))
                            (push new-name existing-buffers)
                            new-name)
                        (progn
                          (push name existing-buffers)
                          name))
                    name)))
               ((symbol-function 'require) (lambda (&rest _) nil)))
      
      ;; Setup first workspace
      (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))))
      (setq enkan-repl--current-workspace "01")
      
      ;; Start eat in first workspace
      (let ((default-directory "/path/to/enkan-repl/"))
        (enkan-repl-start-eat))
      
      ;; Check first buffer name
      (should (= (length renamed-buffers) 1))
      (let ((first-call (car renamed-buffers)))
        (should (string= (nth 1 first-call) "*ws:01 enkan:/path/to/enkan-repl/*"))
        (should (eq (nth 2 first-call) t)) ; unique flag should be t
        )
      
      ;; Setup second workspace  
      (setq enkan-repl--workspaces '(("01" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))
                                      ("02" . (:current-project "enkan-repl"
                                               :session-list nil
                                               :session-counter 0
                                               :project-aliases nil))))
      (setq enkan-repl--current-workspace "02")
      
      ;; Start eat in second workspace
      (let ((default-directory "/path/to/enkan-repl/"))
        (enkan-repl-start-eat))
      
      ;; Check second buffer name - THE PROBLEM!
      (should (= (length renamed-buffers) 2))
      (let ((second-call (car renamed-buffers)))
        (should (string= (nth 1 second-call) "*ws:02 enkan:/path/to/enkan-repl/*"))
        (should (eq (nth 2 second-call) t))) ; unique flag should be t
      
      ;; The ACTUAL problem: With unique=t, if "*ws:01 enkan:/path/to/enkan-repl/*" exists,
      ;; and we try to rename to "*ws:02 enkan:/path/to/enkan-repl/*", it should work!
      ;; But with current implementation, it creates "*ws:01 enkan:/path/to/enkan-repl/*<2>"
      ;; because the buffer name is generated BEFORE workspace switch!
      (should-not (member "*ws:01 enkan:/path/to/enkan-repl/*<2>" existing-buffers))
      
      ;; Clean up created buffers
      (dolist (buf created-buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest test-enkan-repl-buffer-should-not-have-unique-suffix ()
  "Test that buffers with different workspace IDs should not need <N> suffix."
  (let ((buffer-names '()))
    ;; Create two buffer names for same path but different workspaces
    (let ((enkan-repl--current-workspace "01"))
      (push (enkan-repl--path->buffer-name "/path/to/project/") buffer-names))
    (let ((enkan-repl--current-workspace "02"))  
      (push (enkan-repl--path->buffer-name "/path/to/project/") buffer-names))
    
    ;; Both names should be different without needing <N> suffix
    (should (string= (nth 1 buffer-names) "*ws:01 enkan:/path/to/project/*"))
    (should (string= (nth 0 buffer-names) "*ws:02 enkan:/path/to/project/*"))
    (should-not (string= (nth 0 buffer-names) (nth 1 buffer-names)))
    ;; Neither should contain angle brackets (no <2> suffix)
    (should-not (string-match-p "<" (nth 0 buffer-names)))
    (should-not (string-match-p "<" (nth 1 buffer-names)))))

(ert-deftest test-enkan-repl-rename-buffer-behavior ()
  "Test the actual rename-buffer behavior with unique flag."
  ;; This test documents the problem: rename-buffer with t creates <N> suffixes
  (let ((buf1 (generate-new-buffer "*test-buffer*"))
        (buf2 (generate-new-buffer "*another*")))
    (unwind-protect
        (progn
          ;; First rename succeeds
          (with-current-buffer buf1
            (rename-buffer "*ws:01 enkan:/test/*" t))
          (should (string= (buffer-name buf1) "*ws:01 enkan:/test/*"))
          
          ;; Second rename with same name but unique=t adds suffix
          (with-current-buffer buf2
            (rename-buffer "*ws:01 enkan:/test/*" t))
          ;; This is the problem: Emacs adds <2> even though we don't want it
          (should (string-match-p "<2>" (buffer-name buf2))))
      ;; Clean up
      (kill-buffer buf1)
      (kill-buffer buf2))))

(provide 'enkan-repl-eat-buffer-creation-test)
;;; enkan-repl-eat-buffer-creation-test.el ends here
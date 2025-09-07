;;; debug-actual-buffer-names.el --- Debug actual buffer naming issue -*- lexical-binding: t -*-

;;; Commentary:
;; Debug test to find the exact buffer name mismatch

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load required files
(load (expand-file-name "enkan-repl.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(when (file-exists-p (expand-file-name "examples/window-layouts.el" default-directory))
  (load (expand-file-name "examples/window-layouts.el" default-directory) nil t))

(ert-deftest debug-actual-buffer-name-mismatch ()
  "Debug the actual buffer name mismatch issue."
  
  ;; Test various path formats to see what gets generated
  (let ((test-paths '("/Users/sekine/dev/self/er"
                      "/Users/sekine/dev/self/er/"
                      "~/dev/self/er"
                      "~/dev/self/er/"))
        (enkan-repl--current-workspace "01"))
    
    (message "\n=== Testing path formats for buffer names ===")
    (dolist (path test-paths)
      (let* ((expanded (expand-file-name path))
             (buffer-name (enkan-repl--path->buffer-name path)))
        (message "Input path: %s" path)
        (message "  Expanded: %s" expanded)
        (message "  Buffer name: %s" buffer-name)
        (message ""))))
  
  ;; Test what enkan-repl--setup-window-eat-buffer-pure generates
  (let ((enkan-repl--current-workspace "01")
        (session-list '((1 . "er")))
        (target-directories '(("er-alias" . ("er" . "/Users/sekine/dev/self/er"))))
        (target-directories2 '(("er-alias" . ("er" . "/Users/sekine/dev/self/er/"))))
        (target-directories3 '(("er-alias" . ("er" . "~/dev/self/er"))))
        (target-directories4 '(("er-alias" . ("er" . "~/dev/self/er/")))))
    
    (message "\n=== Testing setup-window-eat-buffer-pure with different path formats ===")
    
    (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
      ;; Test without trailing slash
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1 session-list target-directories)))
        (message "Target dir without trailing slash: %s" 
                 (cdr (assoc "er-alias" target-directories)))
        (message "  Generated buffer name: %s" (cdr result))
        (message ""))
      
      ;; Test with trailing slash
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1 session-list target-directories2)))
        (message "Target dir with trailing slash: %s" 
                 (cdr (assoc "er-alias" target-directories2)))
        (message "  Generated buffer name: %s" (cdr result))
        (message ""))
      
      ;; Test with ~ notation
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1 session-list target-directories3)))
        (message "Target dir with ~: %s" 
                 (cdr (assoc "er-alias" target-directories3)))
        (message "  Generated buffer name: %s" (cdr result))
        (message ""))
      
      ;; Test with ~ and trailing slash
      (let ((result (enkan-repl--setup-window-eat-buffer-pure
                     'test-window 1 session-list target-directories4)))
        (message "Target dir with ~ and trailing slash: %s" 
                 (cdr (assoc "er-alias" target-directories4)))
        (message "  Generated buffer name: %s" (cdr result))
        (message "")))))

(ert-deftest test-actual-eat-buffer-creation-and-lookup ()
  "Test the actual eat buffer creation and lookup process."
  
  (let ((enkan-repl--current-workspace "01")
        (default-directory "/Users/sekine/dev/self/er/")
        (buffer nil))
    
    (unwind-protect
        (progn
          ;; Create buffer as enkan-repl-start-eat would
          (let ((buffer-name (enkan-repl--path->buffer-name default-directory)))
            (message "Creating buffer with name: %s" buffer-name)
            (setq buffer (generate-new-buffer buffer-name))
            
            ;; Verify buffer exists
            (should (buffer-live-p buffer))
            (should (get-buffer buffer-name))
            (message "Buffer created successfully: %s" (buffer-name buffer))
            
            ;; Now test if setup-window-eat-buffer-pure can find it
            (let* ((session-list '((1 . "er")))
                   (target-directories '(("er-alias" . ("er" . "/Users/sekine/dev/self/er"))))
                   (result (when (fboundp 'enkan-repl--setup-window-eat-buffer-pure)
                            (enkan-repl--setup-window-eat-buffer-pure
                             'test-window 1 session-list target-directories))))
              
              (message "setup-window-eat-buffer-pure returned: %s" (cdr result))
              
              ;; Check if the generated name matches
              (should result)
              (let ((generated-name (cdr result)))
                (message "Comparing:")
                (message "  Created buffer: %s" buffer-name)
                (message "  Generated name: %s" generated-name)
                (message "  Are they equal? %s" (string= buffer-name generated-name))
                
                ;; This is the critical test
                (should (string= buffer-name generated-name))
                (should (get-buffer generated-name))))))
      
      ;; Clean up
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'debug-actual-buffer-names)
;;; debug-actual-buffer-names.el ends here
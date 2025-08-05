;;; test-cheatsheet-interactive.el --- Interactive test for cheatsheet -*- lexical-binding: t -*-

(add-to-list 'load-path ".")
(require 'claudemacs-repl)

;; Simulate the cheatsheet function preparation (without actual interactive call)
(let* ((current-file (expand-file-name "claudemacs-repl.el"))
       (functions-info (claudemacs-repl-utils--extract-function-info current-file))
       (interactive-functions (cl-remove-if-not
                              (lambda (f) (plist-get f :interactive))
                              functions-info))
       (candidates (mapcar (lambda (func)
                            (cons (plist-get func :name)
                                  (or (plist-get func :docstring) "No description")))
                          interactive-functions)))
  (message "Cheatsheet would show %d commands:" (length candidates))
  (dolist (candidate candidates)
    (message "  %s — %s" (car candidate) 
             (if (> (length (cdr candidate)) 60)
                 (concat (substring (cdr candidate) 0 60) "...")
               (cdr candidate)))))

(message "Cheatsheet simulation completed successfully!")

;;; test-cheatsheet-interactive.el ends here
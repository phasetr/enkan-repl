;;; debug-extraction.el --- Debug function extraction -*- lexical-binding: t -*-

(add-to-list 'load-path ".")
(require 'claudemacs-repl-utils)

(let* ((test-file "claudemacs-repl.el")
       (functions-info (claudemacs-repl-utils--extract-function-info test-file)))
  (message "Total functions found: %d" (length functions-info))
  (dolist (func functions-info)
    (message "Function: %s, Interactive: %s, Docstring: %s"
             (plist-get func :name)
             (plist-get func :interactive)
             (substring (or (plist-get func :docstring) "No docstring") 0 (min 50 (length (or (plist-get func :docstring) "No docstring"))))))
  (let ((interactive-functions (cl-remove-if-not
                               (lambda (f) (plist-get f :interactive))
                               functions-info)))
    (message "Interactive functions: %d" (length interactive-functions))))

;;; debug-extraction.el ends here
;;; test-minor-mode.el --- Test script for claudemacs-repl minor mode -*- lexical-binding: t -*-

(require 'cl-lib)

;; Add current directory to load path
(add-to-list 'load-path ".")

(require 'claudemacs-repl)

;; Test 1: Check if minor mode is defined
(unless (fboundp 'claudemacs-repl-mode)
  (error "claudemacs-repl-mode is not defined"))

;; Test 2: Check if cheatsheet function is defined
(unless (fboundp 'claudemacs-repl-cheatsheet)
  (error "claudemacs-repl-cheatsheet is not defined"))

;; Test 3: Check if keymap is defined
(unless (keymapp claudemacs-repl-mode-map)
  (error "claudemacs-repl-mode-map is not a keymap"))

;; Test 4: Check if keymap has the cheatsheet binding
(unless (eq (lookup-key claudemacs-repl-mode-map (kbd "C-c C-h")) 'claudemacs-repl-cheatsheet)
  (error "C-c C-h is not bound to claudemacs-repl-cheatsheet"))

;; Test 5: Check emulation-mode-map-alists entry
(unless (assq 'claudemacs-repl-mode (car emulation-mode-map-alists))
  (error "claudemacs-repl-mode not found in emulation-mode-map-alists"))

;; Test 6: Test function extraction (mock test)
(let* ((test-file (expand-file-name "claudemacs-repl.el"))
       (functions-info (claudemacs-repl-utils--extract-function-info test-file))
       (interactive-functions (cl-remove-if-not
                              (lambda (f) (plist-get f :interactive))
                              functions-info)))
  (unless (> (length interactive-functions) 10)
    (error "Expected more than 10 interactive functions, got %d" (length interactive-functions))))

(message "All minor mode tests PASSED!")
(message "Found %d interactive functions" 
         (length (cl-remove-if-not
                 (lambda (f) (plist-get f :interactive))
                 (claudemacs-repl-utils--extract-function-info "claudemacs-repl.el"))))

;;; test-minor-mode.el ends here
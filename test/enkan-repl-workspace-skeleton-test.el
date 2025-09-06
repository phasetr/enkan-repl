(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'ert)
(require 'enkan-repl)

(ert-deftest enkan-ws-skeleton-default-id ()
  "Default workspace ID is a zero-padded 2-digit numeric string (e.g., \"01\")."
  (should (string-match-p "^[0-9]\\{2\\}$" (enkan-repl--ws-id)))
  (should (string= (enkan-repl--ws-token) (concat "ws:" (enkan-repl--ws-id)))))

(ert-deftest enkan-ws-skeleton-save-load-roundtrip ()
  "Saving and loading workspace state round-trips current globals."
  (let* ((orig-project enkan-repl--current-project)
         (orig-list enkan-repl-session-list)
         (orig-counter enkan-repl--session-counter)
         (orig-aliases enkan-repl-project-aliases))
    (unwind-protect
        (progn
          ;; Set globals to known values
          (setq enkan-repl--current-project "proj-X")
          (setq enkan-repl-session-list '((1 . "alpha") (2 . "beta")))
          (setq enkan-repl--session-counter 7)
          (setq enkan-repl-project-aliases '(("a" . "alpha") ("b" . "beta")))
          ;; Save
          (let ((saved (enkan-repl--save-workspace-state)))
            (should (plist-get saved :current-project))
            ;; Mutate globals
            (setq enkan-repl--current-project nil)
            (setq enkan-repl-session-list nil)
            (setq enkan-repl--session-counter 0)
            (setq enkan-repl-project-aliases nil)
            ;; Load
            (let ((loaded (enkan-repl--load-workspace-state)))
              (should loaded)
              (should (string= enkan-repl--current-project "proj-X"))
              (should (equal enkan-repl-session-list '((1 . "alpha") (2 . "beta"))))
              (should (= enkan-repl--session-counter 7))
              (should (equal enkan-repl-project-aliases '(("a" . "alpha") ("b" . "beta")))))))
      ;; restore
      (setq enkan-repl--current-project orig-project)
      (setq enkan-repl-session-list orig-list)
      (setq enkan-repl--session-counter orig-counter)
      (setq enkan-repl-project-aliases orig-aliases))))

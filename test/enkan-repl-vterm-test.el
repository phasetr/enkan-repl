;;; enkan-repl-vterm-test.el --- Direct tmux views -*- lexical-binding: t; -*-

;;; Commentary:
;; Deterministic tests; no native module or user tmux server is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'enkan-repl)

(defvar enkan-repl-tmux-display-method)

(ert-deftest enkan-vterm-auto-platforms-and-fallback ()
  "Auto uses vterm on Mac/Linux, and retains mirrors elsewhere."
  (let ((enkan-repl-tmux-display-method 'auto)
        (enkan-repl-terminal-backend 'tmux))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
               (lambda () t)))
      (dolist (platform '(darwin gnu/linux))
        (let ((system-type platform))
          (should (enkan-repl--terminal-tmux--use-vterm-p))))
      (let ((system-type 'windows-nt))
        (should-not (enkan-repl--terminal-tmux--use-vterm-p)))
      (let ((enkan-repl-tmux-display-method 'mirror))
        (should-not (enkan-repl--terminal-tmux--use-vterm-p))))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
               (lambda () nil)))
      (should-not (enkan-repl--terminal-tmux--use-vterm-p))
      (let ((enkan-repl-tmux-display-method 'vterm))
        (should-error (enkan-repl--terminal-tmux--use-vterm-p)
                      :type 'user-error)))))

(ert-deftest enkan-vterm-probe-never-loads-vterm-without-native-module ()
  "Avoid vterm's automatic native-module build path."
  (let ((module-file-suffix ".so") calls)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (push feature calls)
                 nil)))
      (should-not (enkan-repl--terminal-tmux--vterm-available-p))
      (should (equal calls '(vterm-module))))))

(ert-deftest enkan-vterm-probe-handles-broken-module ()
  "An installed but unloadable module is treated as unavailable."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) (error "Wrong architecture"))))
    (should-not (enkan-repl--terminal-tmux--vterm-available-p))))

(defmacro enkan-test-with-project-vterms (&rest body)
  "Run BODY with native vterm and tmux control replaced by local stubs."
  (declare (indent 0) (debug t))
  `(let ((enkan-repl-terminal-backend 'tmux)
         (enkan-repl-tmux-display-method 'vterm)
         (enkan-repl-tmux-bell-notify nil)
         (enkan-repl--current-workspace "91")
         created calls)
     (cl-letf (((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p)
                (lambda () t))
               ((symbol-function 'enkan-repl--terminal-tmux--call)
                (lambda (args &optional capture)
                  (push args calls)
                  (if capture "@12" t)))
               ((symbol-function 'enkan-repl--terminal-tmux--vterm-live-p)
                (lambda (_) t))
               ((symbol-function 'vterm-mode)
                (lambda ()
                  (setq major-mode 'vterm-mode)
                  (use-local-map (make-sparse-keymap)))))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer created)
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest enkan-vterm-replaces-project-buffers-without-changing-identity ()
  "Two existing project slots remain two named buffers and send targets."
  (enkan-test-with-project-vterms
   (let* ((one (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
          (two (enkan-repl--terminal-tmux--mirror-make "enkan-91:two|%2" t "/tmp/two/")))
     (setq created (list one two))
     (should-not (eq one two))
     (should (eq (buffer-local-value 'major-mode one) 'vterm-mode))
     (should (equal (buffer-name one) "*ws:91 enkan:/tmp/one/*"))
     (should (equal (buffer-name two) "*ws:91 enkan:/tmp/two/*"))
     (should (equal (enkan-repl--buffer-tmux-id one) "enkan-91:one|%1"))
     (should (equal (enkan-repl--terminal--coerce-id one) "enkan-91:one|%1"))
     (should (= 2 (enkan-repl--get-workspace-buffer-count-pure created "91")))
     (should (equal (enkan-repl--get-available-buffers created) created))
     (should (eq one (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))))))

(ert-deftest enkan-vterm-uses-independent-views-for-projects-in-same-workspace ()
  "Each project gets an independent tmux selection without cloning its CLI."
  (enkan-test-with-project-vterms
   (let* ((one (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
          (two (enkan-repl--terminal-tmux--mirror-make "enkan-91:two|%2" t "/tmp/two/")))
     (setq created (list one two))
     (let ((first (buffer-local-value 'enkan-repl--tmux-vterm-session one))
           (second (buffer-local-value 'enkan-repl--tmux-vterm-session two)))
       (should (stringp first))
       (should (stringp second))
       (should-not (equal first second))
       (should-not (string-prefix-p enkan-repl-tmux-session-prefix first))
       (should (cl-some (lambda (args)
                          (and (equal (car args) "new-session")
                               (member "link-window" args)
                               (member "@12" args))) calls))))))

(ert-deftest enkan-vterm-keeps-configured-keys-in-global-mode ()
  "Vterm buffers must inherit example keys and obey mode toggling/customizing."
  (enkan-test-with-project-vterms
   (let ((enkan-repl-global-minor-bindings nil)
         (enkan-repl-global-minor-mode nil)
         (enkan-repl-global-minor-mode-map (make-sparse-keymap))
         (minor-mode-map-alist (copy-tree minor-mode-map-alist))
         (enkan-repl-center-file nil)
         (load-path (cons (expand-file-name "examples") load-path)))
     (load (expand-file-name "examples/keybinding.el") nil t)
     (let ((buffer (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/")))
       (push buffer created)
       (with-current-buffer buffer
         (dolist (binding enkan-repl-global-minor-bindings)
           (should (eq (key-binding (kbd (car binding))) (cdr binding))))
         (let ((enkan-repl-global-minor-mode nil))
           (should-not (eq (key-binding (kbd "C-M-w")) #'enkan-repl-workspace-switch)))
         (setopt enkan-repl-global-minor-bindings '(("C-M-w" . ignore)))
         (should (eq (key-binding (kbd "C-M-w")) #'ignore)))))))

(ert-deftest enkan-vterm-example-layout-keeps-project-columns-and-center-file ()
  "C-M-l still arranges separate project buffers beside the center file."
  (enkan-test-with-project-vterms
   (let* ((load-path (cons (expand-file-name "examples") load-path))
          (enkan-repl--current-project "suite")
          (enkan-repl-session-list '((1 . "one") (2 . "two")))
          (enkan-repl-target-directories
           '(("one" . ("one" . "/tmp/one/")) ("two" . ("two" . "/tmp/two/"))))
          (enkan-repl-center-file (make-temp-file "enkan-layout-" nil ".org"))
          (one (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
          (two (enkan-repl--terminal-tmux--mirror-make "enkan-91:two|%2" t "/tmp/two/")))
     (setq created (list one two))
     (require 'window-layouts)
     (unwind-protect
         (save-window-excursion
           (enkan-repl-setup-current-project-layout)
           (should (= 3 (length (window-list))))
           (should (get-buffer-window one))
           (should (get-buffer-window two))
           (should (equal (buffer-file-name (window-buffer (selected-window))) enkan-repl-center-file)))
       (when-let* ((buffer (get-file-buffer enkan-repl-center-file))) (kill-buffer buffer))
       (delete-file enkan-repl-center-file)))))

(ert-deftest enkan-vterm-workspace-switch-uses-existing-layout-command ()
  "Workspace switching must continue to invoke the user's layout workflow."
  (let ((enkan-repl-terminal-backend 'tmux)
        (enkan-repl-tmux-display-method 'vterm)
        (enkan-repl--workspaces '(("01" :current-project "one") ("02" :current-project "two")))
        (enkan-repl--current-workspace "01")
        (enkan-repl--current-project "two") shown)
    (cl-letf (((symbol-function 'hmenu) (lambda (&rest _) "02 [two]"))
              ((symbol-function 'enkan-repl--save-workspace-state) #'ignore)
              ((symbol-function 'enkan-repl--load-workspace-state) #'ignore)
              ((symbol-function 'enkan-repl--restore-current-workspace-sessions-from-live-terminals) #'ignore)
              ((symbol-function 'enkan-repl-setup-current-project-layout)
               (lambda () (setq shown enkan-repl--current-workspace)))
              ((symbol-function 'enkan-repl--terminal-tmux--vterm-available-p) (lambda () t)))
      (enkan-repl-workspace-switch)
      (should (equal shown "02")))))

(ert-deftest enkan-vterm-capture-never-overwrites-native-terminal ()
  "Late mirror capture callbacks must not write to a converted vterm."
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (setq-local enkan-repl--tmux-mirror-id "enkan-01:project")
    (insert "untouched")
    (let ((enkan-repl-tmux-display-method 'mirror))
      (enkan-repl--terminal-tmux--mirror-refresh (current-buffer) t)
      (enkan-repl--terminal-tmux--mirror-apply-content
       (current-buffer) enkan-repl--tmux-mirror-id (current-time) "wrong" 0)
      (should (equal (buffer-string) "untouched")))))

(ert-deftest enkan-vterm-layout-converts-open-project-mirror-in-place ()
  "An already open project mirror is replaced without changing its slot/name."
  (enkan-test-with-project-vterms
   (let ((enkan-repl-session-list '((1 . "one")))
         (enkan-repl-target-directories '(("one" . ("one" . "/tmp/one/"))))
         (load-path (cons (expand-file-name "examples") load-path)))
     (require 'window-layouts)
     (let* ((old (enkan-repl--terminal-tmux--legacy-mirror-make
                  "enkan-91:one|%1" t "/tmp/one/"))
            (name (buffer-name old))
            (buffers (enkan-repl--registered-session-buffers)))
       (setq created buffers)
       (should (= 1 (length buffers)))
       (should-not (buffer-live-p old))
       (should (equal (buffer-name (car buffers)) name))
       (should (eq (buffer-local-value 'major-mode (car buffers)) 'vterm-mode))))))

(ert-deftest enkan-vterm-closing-project-view-preserves-owner-session ()
  "Only the private view session is killed when its project buffer closes."
  (enkan-test-with-project-vterms
   (let* ((buffer (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
          (view (buffer-local-value 'enkan-repl--tmux-vterm-session buffer)))
     (push buffer created)
     (setq calls nil)
     (kill-buffer buffer)
     (should (equal calls (list (list "kill-session" "-t" (concat "=" view))))))))

(ert-deftest enkan-vterm-workspace-deletion-closes-all-its-project-views ()
  "Linked views cannot keep project processes alive after workspace deletion."
  (enkan-test-with-project-vterms
   (let ((one (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
         (two (enkan-repl--terminal-tmux--mirror-make "enkan-91:two|%2" t "/tmp/two/"))
         (other (enkan-repl--terminal-tmux--mirror-make "enkan-92:one|%3" t "/tmp/one/")))
     (setq created (list one two other))
     (should (= 2 (plist-get (enkan-repl--stop-workspace-terminals "91") :buffers-killed)))
     (should-not (buffer-live-p one))
     (should-not (buffer-live-p two))
     (should (buffer-live-p other))
     (should (member '("kill-session" "-t" "enkan-91") calls)))))

(ert-deftest enkan-vterm-creation-failure-cleans-private-session-and-buffer ()
  "Failed native initialization leaves the original tmux project untouched."
  (enkan-test-with-project-vterms
   (cl-letf (((symbol-function 'vterm-mode) (lambda () (error "Native initialization failed"))))
     (should-error (enkan-repl--terminal-tmux--mirror-make "enkan-91:one|%1" t "/tmp/one/"))
     (should-not (get-buffer "*ws:91 enkan:/tmp/one/*"))
     (should (equal (caar calls) "kill-session"))
     (should (string-prefix-p "=__enkan_vterm_" (nth 2 (car calls)))))))

(provide 'enkan-repl-vterm-test)
;;; enkan-repl-vterm-test.el ends here

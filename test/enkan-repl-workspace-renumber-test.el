;;; enkan-repl-workspace-renumber-test.el --- Tests for workspace renumbering -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for renumbering an existing workspace into a free (gap) id, and for
;; keeping the tmux session / buffer names / tmux mirror ids in sync.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'enkan-repl)

;;; Pure function tests: enkan-repl-workspace.el

(ert-deftest test-enkan-repl--can-rename-workspace ()
  "Renaming requires the source to exist and the target to be free."
  (let ((workspaces '(("01" . (:current-project nil))
                       ("03" . (:current-project nil)))))
    (should (enkan-repl--can-rename-workspace workspaces "03" "02"))
    (should-not (enkan-repl--can-rename-workspace workspaces "99" "02"))
    (should-not (enkan-repl--can-rename-workspace workspaces "03" "01"))
    (should-not (enkan-repl--can-rename-workspace workspaces "03" "03"))))

(ert-deftest test-enkan-repl--rename-workspace-id ()
  "Renaming preserves state and the position of the other workspaces."
  (let ((workspaces '(("01" . (:current-project "a"))
                       ("03" . (:current-project "b"))
                       ("04" . (:current-project "c")))))
    (should (equal '(("01" . (:current-project "a"))
                      ("02" . (:current-project "b"))
                      ("04" . (:current-project "c")))
                   (enkan-repl--rename-workspace-id workspaces "03" "02")))))

;;; Pure function tests: enkan-repl-terminal.el

(ert-deftest test-enkan-repl--terminal-tmux--id-with-session ()
  "Session component is swapped; window/pane are preserved."
  (should (equal "enkan-02:lat"
                 (enkan-repl--terminal-tmux--id-with-session
                  "enkan-01:lat" "enkan-02")))
  (should (equal "enkan-02:lat|%29"
                 (enkan-repl--terminal-tmux--id-with-session
                  "enkan-01:lat|%29" "enkan-02"))))

;;; Pure function tests: enkan-repl-utils.el

(ert-deftest test-enkan-repl--buffer-name-with-workspace-id ()
  "Only the leading ws:NN segment is replaced; suffixes survive."
  (should (equal "*ws:02 enkan:/repo/*"
                 (enkan-repl--buffer-name-with-workspace-id
                  "*ws:01 enkan:/repo/*" "02")))
  (should (equal "*ws:02 enkan:/repo/*<2>"
                 (enkan-repl--buffer-name-with-workspace-id
                  "*ws:01 enkan:/repo/*<2>" "02")))
  (should-not (enkan-repl--buffer-name-with-workspace-id
               "*scratch*" "02")))

;;; Integration test: enkan-repl.el orchestrator

(ert-deftest test-enkan-repl--renumber-workspace-updates-state-and-buffers ()
  "Renumbering updates the workspaces alist, current-workspace, tmux session,
and every live buffer (including tmux mirror ids) belonging to the workspace."
  (let* ((enkan-repl--workspaces
          '(("01" . (:current-project "a"))
            ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "03")
         (enkan-repl-tmux-session-prefix "enkan-")
         (rename-calls nil)
         (eat-buffer (generate-new-buffer "*ws:03 enkan:/repo/b/*"))
         (mirror-buffer (generate-new-buffer "*ws:03 enkan:/repo/c/*")))
    (unwind-protect
        (progn
          (with-current-buffer mirror-buffer
            (setq-local enkan-repl--tmux-mirror-id "enkan-03:lat|%29"))
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
                     (lambda (old-id new-id)
                       (push (cons old-id new-id) rename-calls)
                       t))
                    ((symbol-function 'enkan-repl-state-save)
                     (lambda (&optional _file) t)))
            (enkan-repl--renumber-workspace "03" "02"))

          (should (equal '(("01" . (:current-project "a"))
                            ("02" . (:current-project "b")))
                         enkan-repl--workspaces))
          (should (equal "02" enkan-repl--current-workspace))
          (should (equal '(("03" . "02")) rename-calls))
          (should (equal "*ws:02 enkan:/repo/b/*" (buffer-name eat-buffer)))
          (should (equal "*ws:02 enkan:/repo/c/*" (buffer-name mirror-buffer)))
          (should (equal "enkan-02:lat|%29"
                         (buffer-local-value 'enkan-repl--tmux-mirror-id
                                              mirror-buffer))))
      (when (buffer-live-p eat-buffer) (kill-buffer eat-buffer))
      (when (buffer-live-p mirror-buffer) (kill-buffer mirror-buffer)))))

(ert-deftest test-enkan-repl--renumber-workspace-rejects-invalid-target ()
  "Renumbering into an occupied, nonexistent, or malformed id signals a
user-error."
  (let ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                   ("02" . (:current-project "b")))))
    (should-error (enkan-repl--renumber-workspace "01" "02") :type 'user-error)
    (should-error (enkan-repl--renumber-workspace "99" "05") :type 'user-error)
    (should-error (enkan-repl--renumber-workspace "01" "100") :type 'user-error)
    (should-error (enkan-repl--renumber-workspace "01" "-1") :type 'user-error)))

(ert-deftest test-enkan-repl--renumber-workspace-skips-tmux-without-mirrors ()
  "On the eat backend with no tmux mirror buffers, tmux is never called."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "01")
         (enkan-repl-terminal-backend 'eat)
         (calls 0)
         (eat-buffer (generate-new-buffer "*ws:03 enkan:/repo/b/*")))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
                   (lambda (_old-id _new-id) (setq calls (1+ calls)) t))
                  ((symbol-function 'enkan-repl-state-save)
                   (lambda (&optional _file) t)))
          (enkan-repl--renumber-workspace "03" "02")
          (should (equal 0 calls))
          (should (equal "*ws:02 enkan:/repo/b/*" (buffer-name eat-buffer))))
      (when (buffer-live-p eat-buffer) (kill-buffer eat-buffer)))))

(ert-deftest test-enkan-repl--renumber-workspace-aborts-on-tmux-failure ()
  "When the live tmux session refuses to rename, no other state changes."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "03")
         (enkan-repl-terminal-backend 'tmux)
         (eat-buffer (generate-new-buffer "*ws:03 enkan:/repo/b/*")))
    (unwind-protect
        (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
                   (lambda (_old-id _new-id) (user-error "tmux refused"))))
          (should-error (enkan-repl--renumber-workspace "03" "02")
                        :type 'user-error)
          (should (equal '(("01" . (:current-project "a"))
                            ("03" . (:current-project "b")))
                         enkan-repl--workspaces))
          (should (equal "03" enkan-repl--current-workspace))
          (should (equal "*ws:03 enkan:/repo/b/*" (buffer-name eat-buffer))))
      (when (buffer-live-p eat-buffer) (kill-buffer eat-buffer)))))

(ert-deftest test-enkan-repl--renumber-workspace-renames-fallback-mirror-buffer ()
  "A tmux mirror buffer still under its raw `*tmux <id>*' fallback name (no
cwd-based name yet) is renamed to match the new session too."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "01")
         (enkan-repl-tmux-session-prefix "enkan-")
         (fallback-buffer (generate-new-buffer "*tmux enkan-03:lat|%1*")))
    (unwind-protect
        (progn
          (with-current-buffer fallback-buffer
            (setq-local enkan-repl--tmux-mirror-id "enkan-03:lat|%1"))
          (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
                     (lambda (_old-id _new-id) t))
                    ((symbol-function 'enkan-repl-state-save)
                     (lambda (&optional _file) t)))
            (enkan-repl--renumber-workspace "03" "02"))
          (should (equal "*tmux enkan-02:lat|%1*" (buffer-name fallback-buffer)))
          (should (equal "enkan-02:lat|%1"
                         (buffer-local-value 'enkan-repl--tmux-mirror-id
                                              fallback-buffer))))
      (when (buffer-live-p fallback-buffer) (kill-buffer fallback-buffer)))))

(ert-deftest test-enkan-repl--renumber-workspace-warns-on-save-failure ()
  "A failed disk persist is reported, not silently swallowed as success."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "01")
         (warned nil))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
               (lambda (_old-id _new-id) t))
              ((symbol-function 'enkan-repl-state-save)
               (lambda (&optional _file) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq warned (apply #'format fmt args)))))
      (let ((result (enkan-repl--renumber-workspace "03" "02")))
        (should (equal "02" (plist-get result :new-id)))
        (should-not (plist-get result :saved)))
      (should (stringp warned))
      (should (string-match-p "failed to persist" warned)))))

(ert-deftest test-enkan-repl-workspace-renumber-reports-save-failure ()
  "The interactive command must not claim success when persisting failed;
the final minibuffer message is what the user actually reads, so an
internal warning that gets overwritten by an unconditional success message
is a real UX bug, not just an internal detail."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "01")
         (final-message nil))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
               (lambda (_old-id _new-id) t))
              ((symbol-function 'enkan-repl-state-save)
               (lambda (&optional _file) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq final-message (apply #'format fmt args)))))
      (enkan-repl-workspace-renumber "03" "02")
      (should (stringp final-message))
      (should (string-match-p "failed to persist" final-message)))))

(ert-deftest test-enkan-repl-workspace-renumber-reports-success ()
  "When persisting succeeds, the interactive command reports plain success."
  (let* ((enkan-repl--workspaces '(("01" . (:current-project "a"))
                                    ("03" . (:current-project "b"))))
         (enkan-repl--current-workspace "01")
         (final-message nil))
    (cl-letf (((symbol-function 'enkan-repl--terminal-tmux-rename-workspace)
               (lambda (_old-id _new-id) t))
              ((symbol-function 'enkan-repl-state-save)
               (lambda (&optional _file) t))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq final-message (apply #'format fmt args)))))
      (enkan-repl-workspace-renumber "03" "02")
      (should (equal "Renumbered workspace 03 to 02" final-message)))))

(provide 'enkan-repl-workspace-renumber-test)
;;; enkan-repl-workspace-renumber-test.el ends here

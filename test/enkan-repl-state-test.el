;;; enkan-repl-state-test.el --- Tests for persistent state -*- lexical-binding: t -*-

;;; Commentary:
;; ERTs for `enkan-repl-state' (atomic save/load, payload validation,
;; tmux reconcile classification).  Tests requiring a real tmux server
;; are kept out -- those are exercised manually.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "enkan-repl-utils.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-terminal.el" default-directory) nil t)
(load (expand-file-name "enkan-repl-state.el" default-directory) nil t)

;;;; Payload build / validate

(ert-deftest test-enkan-repl-state--build-payload ()
  (let ((p (enkan-repl-state--build-payload
            '(("01" :current-project "lat" :session-list ((1 . "lat"))))
            "01")))
    (should (= 1 (plist-get p :schema-version)))
    (should (string= "01" (plist-get p :current)))
    (should (equal '(("01" :current-project "lat" :session-list ((1 . "lat"))))
                   (plist-get p :workspaces)))
    (should (stringp (plist-get p :saved-at)))))

(ert-deftest test-enkan-repl-state--validate-payload ()
  (should (enkan-repl-state--validate-payload
           '(:schema-version 1 :workspaces ())))
  (should (enkan-repl-state--validate-payload
           '(:schema-version 2 :workspaces (("01" :session-list nil)))))
  (should-not (enkan-repl-state--validate-payload nil))
  (should-not (enkan-repl-state--validate-payload "garbage"))
  (should-not (enkan-repl-state--validate-payload
               '(:workspaces ()))))  ; missing schema-version

;;;; Save / Load round-trip

(ert-deftest test-enkan-repl-state-save-and-load ()
  "Round-trip: save current `enkan-repl--workspaces' to a temp file and
load it back, ensuring data survives serialization."
  (let* ((tmp (make-temp-file "enkan-repl-state-" nil ".eld"))
         (enkan-repl--workspaces
          '(("01" :current-project "lat"
                  :session-list ((1 . "lat") (2 "lat" . 2))
                  :session-counter 2
                  :project-aliases (("lat" . "lat")))
            ("02" :current-project "er"
                  :session-list ((1 . "er"))
                  :session-counter 1
                  :project-aliases (("er" . "er")))))
         (enkan-repl--current-workspace "02"))
    (unwind-protect
        (progn
          (should (string= tmp (enkan-repl-state-save tmp)))
          (let ((loaded (enkan-repl-state-load tmp)))
            (should loaded)
            (should (string= "02" (plist-get loaded :current)))
            (let ((wss (plist-get loaded :workspaces)))
              (should (= 2 (length wss)))
              (should (equal '((1 . "lat") (2 "lat" . 2))
                             (plist-get (cdr (assoc "01" wss))
                                        :session-list)))
              (should (equal '((1 . "er"))
                             (plist-get (cdr (assoc "02" wss))
                                        :session-list))))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest test-enkan-repl-state-load-missing-file ()
  "Loading a non-existent file returns nil (no error)."
  (let ((tmp (concat (make-temp-name "/tmp/enkan-state-nx-") ".eld")))
    (should (null (enkan-repl-state-load tmp)))))

(ert-deftest test-enkan-repl-state-load-corrupt ()
  "Loading a malformed file returns nil and does not raise."
  (let ((tmp (make-temp-file "enkan-repl-state-corrupt-" nil ".eld")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "not-a-valid-elisp-payload"))
          (should (null (enkan-repl-state-load tmp))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest test-enkan-repl-state-save-atomic ()
  "Save replaces target via rename; the .tmp file should not remain."
  (let* ((tmp (make-temp-file "enkan-repl-state-atomic-" nil ".eld"))
         (enkan-repl--workspaces '(("01" :session-list nil :session-counter 0)))
         (enkan-repl--current-workspace "01"))
    (unwind-protect
        (progn
          (should (enkan-repl-state-save tmp))
          (should (file-exists-p tmp))
          (should-not (file-exists-p (concat tmp ".tmp"))))
      (when (file-exists-p tmp) (delete-file tmp))
      (when (file-exists-p (concat tmp ".tmp"))
        (delete-file (concat tmp ".tmp"))))))

;;;; Reconcile classification (mocked tmux)

(ert-deftest test-enkan-repl-state--classify ()
  "With mocked tmux session listing, classify state vs live correctly."
  (let ((enkan-repl-tmux-session-prefix "enkan-"))
    ;; All overlap
    (cl-letf (((symbol-function 'enkan-repl-state--list-live-tmux-sessions)
               (lambda (_p) '("enkan-01" "enkan-02"))))
      (let ((cls (enkan-repl-state--classify
                  '(("01" :session-list nil) ("02" :session-list nil)))))
        (should (equal '("01" "02") (plist-get cls :both)))
        (should (null (plist-get cls :state-only)))
        (should (null (plist-get cls :tmux-only)))))
    ;; State has extra
    (cl-letf (((symbol-function 'enkan-repl-state--list-live-tmux-sessions)
               (lambda (_p) '("enkan-01"))))
      (let ((cls (enkan-repl-state--classify
                  '(("01" :session-list nil) ("02" :session-list nil)))))
        (should (equal '("01") (plist-get cls :both)))
        (should (equal '("02") (plist-get cls :state-only)))
        (should (null (plist-get cls :tmux-only)))))
    ;; tmux has extra
    (cl-letf (((symbol-function 'enkan-repl-state--list-live-tmux-sessions)
               (lambda (_p) '("enkan-01" "enkan-99"))))
      (let ((cls (enkan-repl-state--classify
                  '(("01" :session-list nil)))))
        (should (equal '("01") (plist-get cls :both)))
        (should (null (plist-get cls :state-only)))
        (should (equal '("enkan-99") (plist-get cls :tmux-only)))))))

;;;; Reconcile high-level: ignore policy returns nil

(ert-deftest test-enkan-repl-state-tmux-reconcile-ignore ()
  (let ((enkan-repl-state-recovery-policy 'ignore))
    (should (null (enkan-repl-state-tmux-reconcile "/no/such/file")))))

(ert-deftest test-enkan-repl-state-tmux-reconcile-keeps-current ()
  "Reconcile result includes the persisted current workspace."
  (let ((tmp (make-temp-file "enkan-repl-state-current-" nil ".eld"))
        (enkan-repl-state-recovery-policy 'reattach)
        (enkan-repl-tmux-session-prefix "enkan-"))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (prin1 '(:schema-version 1
                     :saved-at "2026-05-09T00:00:00+0900"
                     :current "02"
                     :workspaces (("01" :session-list nil)
                                  ("02" :session-list nil)))
                   (current-buffer)))
          (cl-letf (((symbol-function 'enkan-repl-state--list-live-tmux-sessions)
                     (lambda (_prefix) '("enkan-02"))))
            (let ((result (enkan-repl-state-tmux-reconcile tmp)))
              (should (equal '("02") (plist-get result :restored)))
              (should (string= "02" (plist-get result :current))))))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(provide 'enkan-repl-state-test)
;;; enkan-repl-state-test.el ends here

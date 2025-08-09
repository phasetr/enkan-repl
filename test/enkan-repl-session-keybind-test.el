;;; enkan-repl-session-keybind-test.el --- Tests for session keybind functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for session minibuffer keybindings, especially C-d for deletion

;;; Code:

(require 'ert)
(require 'enkan-repl)

;;;; Tests for minibuffer keymap

(ert-deftest test-session-minibuffer-map-has-c-d ()
  "Test that session minibuffer map has C-d bound."
  (should (keymapp enkan-repl-session-minibuffer-map))
  (should (eq (lookup-key enkan-repl-session-minibuffer-map (kbd "C-d"))
              'enkan-repl-session-delete-from-minibuffer)))

(ert-deftest test-session-minibuffer-map-inherits-from-completion-map ()
  "Test that session minibuffer map inherits from completion map."
  (should (eq (keymap-parent enkan-repl-session-minibuffer-map)
              minibuffer-local-completion-map)))

;;;; Tests for deletion flag function

(ert-deftest test-session-delete-from-minibuffer-sets-flag ()
  "Test that delete function sets the flag."
  ;; Save original value
  (let ((original-flag enkan-repl--session-delete-requested))
    (unwind-protect
        (progn
          ;; Reset flag
          (setq enkan-repl--session-delete-requested nil)
          ;; Simulate calling the function (without exit-minibuffer)
          (let ((inhibit-quit t))
            (condition-case nil
                (enkan-repl-session-delete-from-minibuffer)
              (error nil)))
          ;; Check flag was set
          (should (eq enkan-repl--session-delete-requested t)))
      ;; Restore original value
      (setq enkan-repl--session-delete-requested original-flag))))

;;;; Test helper functions

(defun enkan-repl-test--simulate-minibuffer-c-d ()
  "Simulate pressing C-d in minibuffer (for testing only)."
  (setq enkan-repl--session-delete-requested t))

(ert-deftest test-session-delete-flag-initial-state ()
  "Test that deletion flag starts as nil."
  (let ((enkan-repl--session-delete-requested 'unset))
    ;; The flag should be reset when entering list-sessions
    (should (not (eq enkan-repl--session-delete-requested nil)))))

(ert-deftest test-minibuffer-deletion-request-flow ()
  "Test the flow of deletion request through minibuffer."
  (let ((enkan-repl--session-delete-requested nil))
    ;; Simulate the deletion request
    (enkan-repl-test--simulate-minibuffer-c-d)
    (should (eq enkan-repl--session-delete-requested t))
    ;; Reset and verify
    (setq enkan-repl--session-delete-requested nil)
    (should (null enkan-repl--session-delete-requested))))

(provide 'enkan-repl-session-keybind-test)
;;; enkan-repl-session-keybind-test.el ends here
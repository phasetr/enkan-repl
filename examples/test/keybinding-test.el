;;; keybinding-test.el --- Tests for keybinding configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for keybinding definitions and 3pane mode integration

;;; Code:

(require 'ert)

;; Load the files to test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir))
  (add-to-list 'load-path (expand-file-name "../.." dir)))

(require 'keybinding-constants)
(require 'keybinding)

;;;; Tests for base keybinding definitions

(ert-deftest test-base-keybindings-include-send-4-and-5 ()
  "Test that base keybindings include send-4 and send-5."
  (let ((keys (mapcar #'car enkan-keybinding-definitions)))
    (should (member "C-M-4" keys))
    (should (member "C-M-5" keys))))

(ert-deftest test-send-4-and-5-commands ()
  "Test that send-4 and send-5 commands are properly defined."
  (let ((binding-4 (assoc "C-M-4" enkan-keybinding-definitions #'string=))
        (binding-5 (assoc "C-M-5" enkan-keybinding-definitions #'string=)))
    (should binding-4)
    (should binding-5)
    (should (eq (nth 1 binding-4) 'enkan-repl-send-4))
    (should (eq (nth 1 binding-5) 'enkan-repl-send-5))
    (should (string= (nth 2 binding-4) "Send 4 to REPL"))
    (should (string= (nth 2 binding-5) "Send 5 to REPL"))
    (should (eq (nth 3 binding-4) 'quick-actions))
    (should (eq (nth 3 binding-5) 'quick-actions))))

;;;; Tests for 3pane mode overrides

(ert-deftest test-3pane-mode-no-send-overrides ()
  "Test that 3pane mode does not override send-1 through send-5."
  (let ((overridden-keys (mapcar #'car enkan-simple-3pane-keybinding-overrides)))
    ;; These should NOT be in the override list
    (should-not (member "<escape>" overridden-keys))
    (should-not (member "C-M-1" overridden-keys))
    (should-not (member "C-M-2" overridden-keys))
    (should-not (member "C-M-3" overridden-keys))
    (should-not (member "C-M-4" overridden-keys))
    (should-not (member "C-M-5" overridden-keys))
    ;; Only C-M-t should be overridden
    (should (member "C-M-t" overridden-keys))))

(ert-deftest test-3pane-command-definitions-no-send-functions ()
  "Test that 3pane command definitions don't include send functions."
  (let ((commands (mapcar #'car enkan-simple-3pane-command-definitions)))
    (should-not (memq 'enkan-simple-3pane-send-escape commands))
    (should-not (memq 'enkan-simple-3pane-send-1 commands))
    (should-not (memq 'enkan-simple-3pane-send-2 commands))
    (should-not (memq 'enkan-simple-3pane-send-3 commands))
    (should-not (memq 'enkan-simple-3pane-send-4 commands))
    (should-not (memq 'enkan-simple-3pane-send-5 commands))))

;;;; Tests for dual-task mode

(ert-deftest test-dual-task-mode-minimal-overrides ()
  "Test that dual-task mode has minimal overrides."
  (let ((overridden-keys (mapcar #'car enkan-dual-task-keybinding-overrides)))
    ;; Only C-M-t should be overridden
    (should (= 1 (length overridden-keys)))
    (should (member "C-M-t" overridden-keys))))

;;;; Tests for keymap creation

(ert-deftest test-keymap-creation ()
  "Test that keymaps can be created from definitions."
  (let ((keymap (enkan-keybinding-make-keymap enkan-keybinding-definitions)))
    (should (keymapp keymap))
    ;; Check that send-4 and send-5 are in the keymap
    (should (lookup-key keymap (kbd "C-M-4")))
    (should (lookup-key keymap (kbd "C-M-5")))))

(ert-deftest test-3pane-keymap-no-send-overrides ()
  "Test that 3pane keymap doesn't override send keys."
  (let ((keymap (enkan-keybinding-make-keymap enkan-simple-3pane-keybinding-overrides)))
    (should (keymapp keymap))
    ;; These should NOT be in the 3pane override keymap
    (should-not (lookup-key keymap (kbd "<escape>")))
    (should-not (lookup-key keymap (kbd "C-M-1")))
    (should-not (lookup-key keymap (kbd "C-M-2")))
    (should-not (lookup-key keymap (kbd "C-M-3")))
    (should-not (lookup-key keymap (kbd "C-M-4")))
    (should-not (lookup-key keymap (kbd "C-M-5")))
    ;; Only C-M-t should be in the override keymap
    (should (lookup-key keymap (kbd "C-M-t")))))

(provide 'keybinding-test)
;;; keybinding-test.el ends here
;;; enkan-repl-mac-notify-test.el --- Tests for enkan-repl-mac-notify -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; License: MIT

;;; Commentary:

;; Tests for macOS notification functionality in enkan-repl.

;;; Code:

(require 'ert)
(require 'enkan-repl-mac-notify)

;;;; Pure Function Tests

(ert-deftest test-enkan-repl--build-notification-command ()
  "Test notification command building."
  ;; Basic message and title
  (should (equal (enkan-repl--build-notification-command "Hello" "Title")
                 "display notification \"Hello\" with title \"Title\""))
  
  ;; Message with quotes that need escaping
  (should (equal (enkan-repl--build-notification-command "He said \"Hello\"" "Title")
                 "display notification \"He said \\\\\"Hello\\\\\"\" with title \"Title\""))
  
  ;; Title with quotes that need escaping
  (should (equal (enkan-repl--build-notification-command "Message" "\"Important\" Title")
                 "display notification \"Message\" with title \"\\\\\"Important\\\\\" Title\""))
  
  ;; Both with quotes
  (should (equal (enkan-repl--build-notification-command "\"Quoted\" message" "\"Quoted\" title")
                 "display notification \"\\\\\"Quoted\\\\\" message\" with title \"\\\\\"Quoted\\\\\" title\""))
  
  ;; Empty strings
  (should (equal (enkan-repl--build-notification-command "" "")
                 "display notification \"\" with title \"\"")))

(ert-deftest test-enkan-repl--should-play-sound-p ()
  "Test sound playing conditions."
  ;; macOS with sound enabled and valid file path
  (should (enkan-repl--should-play-sound-p 'darwin t "/System/Library/Sounds/Glass.aiff"))
  
  ;; macOS with sound disabled
  (should-not (enkan-repl--should-play-sound-p 'darwin nil "/System/Library/Sounds/Glass.aiff"))
  
  ;; Non-macOS system
  (should-not (enkan-repl--should-play-sound-p 'gnu/linux t "/System/Library/Sounds/Glass.aiff"))
  (should-not (enkan-repl--should-play-sound-p 'windows-nt t "/System/Library/Sounds/Glass.aiff"))
  
  ;; macOS with empty file path
  (should-not (enkan-repl--should-play-sound-p 'darwin t ""))
  
  ;; macOS with nil file path
  (should-not (enkan-repl--should-play-sound-p 'darwin t nil))
  
  ;; All conditions false
  (should-not (enkan-repl--should-play-sound-p 'gnu/linux nil "")))

(ert-deftest test-enkan-repl--should-show-notification-p ()
  "Test notification display conditions."
  ;; macOS with notifications enabled
  (should (enkan-repl--should-show-notification-p 'darwin t))
  
  ;; macOS with notifications disabled
  (should-not (enkan-repl--should-show-notification-p 'darwin nil))
  
  ;; Non-macOS systems
  (should-not (enkan-repl--should-show-notification-p 'gnu/linux t))
  (should-not (enkan-repl--should-show-notification-p 'windows-nt t))
  
  ;; Non-macOS with notifications disabled
  (should-not (enkan-repl--should-show-notification-p 'gnu/linux nil)))

;;;; Integration Tests (require manual verification)

(ert-deftest test-enkan-repl-notify-task-complete-manual ()
  "Manual test for notification functionality.
This test requires manual verification that notification appears and sound plays."
  :tags '(:manual)
  (when (eq system-type 'darwin)
    ;; Save original values
    (let ((orig-sound enkan-repl-notify-sound-on-completion)
          (orig-notification enkan-repl-notify-macos-notification-on-completion))
      (unwind-protect
          (progn
            ;; Enable both sound and notification
            (setq enkan-repl-notify-sound-on-completion t)
            (setq enkan-repl-notify-macos-notification-on-completion t)
            
            ;; Call the function - should show notification and play sound
            (enkan-repl-notify-task-complete "Test notification from ERT")
            
            ;; Manual verification required
            (message "Check: Did you see a notification and hear a sound?"))
        ;; Restore original values
        (setq enkan-repl-notify-sound-on-completion orig-sound)
        (setq enkan-repl-notify-macos-notification-on-completion orig-notification)))))

(provide 'enkan-repl-mac-notify-test)

;;; enkan-repl-mac-notify-test.el ends here
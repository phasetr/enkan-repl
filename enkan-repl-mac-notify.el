;;; enkan-repl-mac-notify.el --- macOS specific notifications for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Version: 0.9.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: enkan ai tools convenience macos notification sound
;; URL: https://github.com/phasetr/enkan-repl
;; License: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides macOS specific sound and visual notifications for enkan-repl.
;; This module allows enkan-repl to play sounds and show native macOS notifications
;; when tasks complete or when awaiting user input.

;;; Code:

(require 'enkan-repl-utils)

(defgroup enkan-repl-notifications nil
  "Customization options for enkan-repl notifications."
  :group 'enkan-repl)

(defcustom enkan-repl-notify-sound-on-completion t
  "Enable or disable playing a sound on task completion."
  :type 'boolean
  :group 'enkan-repl-notifications)

(defcustom enkan-repl-notify-sound-file
  "/System/Library/Sounds/Glass.aiff"
  "Path to the sound file to play on task completion.
Defaults to a system sound. Change this to your preferred sound."
  :type 'file
  :group 'enkan-repl-notifications)

(defcustom enkan-repl-notify-macos-notification-on-completion t
  "Enable or disable macOS native notification on task completion."
  :type 'boolean
  :group 'enkan-repl-notifications)

(defcustom enkan-repl-notify-macos-notification-title "Enkan REPL"
  "Title for the macOS native notification."
  :type 'string
  :group 'enkan-repl-notifications)

(defcustom enkan-repl-notify-macos-notification-message "Task completed!"
  "Default message for the macOS native notification."
  :type 'string
  :group 'enkan-repl-notifications)

(defun enkan-repl--play-completion-sound ()
  "Play a notification sound on macOS using afplay.
This function is intended for internal use by enkan-repl."
  (when (enkan-repl--should-play-sound-p
         system-type
         enkan-repl-notify-sound-on-completion
         enkan-repl-notify-sound-file)
    (when (file-exists-p enkan-repl-notify-sound-file)
      (start-process "enkan-repl-sound" nil "afplay" enkan-repl-notify-sound-file))))

(defun enkan-repl--show-completion-notification (&optional message)
  "Show a macOS native notification on task completion using osascript.
MESSAGE is an optional string to override the default notification message.
This function is intended for internal use by enkan-repl."
  (when (enkan-repl--should-show-notification-p
         system-type
         enkan-repl-notify-macos-notification-on-completion)
    (let* ((notification-message (or message enkan-repl-notify-macos-notification-message))
           (osascript-command (enkan-repl--build-notification-command
                              notification-message
                              enkan-repl-notify-macos-notification-title)))
      (start-process "enkan-repl-notification" nil "osascript" "-e" osascript-command))))

(defun enkan-repl-notify-task-complete (&optional message)
  "Notify user that a task has completed.
Optional MESSAGE can override the default notification message.
This is a public function that can be called by other enkan-repl modules."
  (interactive)
  (enkan-repl--play-completion-sound)
  (enkan-repl--show-completion-notification message))

(defun enkan-repl--bell-handler ()
  "Handle bell events from Claude Code.
This function is called when Claude Code sends a bell character."
  (when enkan-repl-notify-macos-notification-on-completion
    (enkan-repl-notify-task-complete "Claude Code task completed!")))

(defun enkan-repl-setup-bell-handler ()
  "Set up or re-setup the completion notification handler.
Use this if system notifications aren't working after starting a session."
  (interactive)
  ;; Find eat buffer matching pattern *enkan:*
  (let ((eat-buffer (seq-find (lambda (buf)
                                (string-match-p "^\\*enkan:.*\\*$" (buffer-name buf)))
                              (buffer-list))))
    (when eat-buffer
      (with-current-buffer eat-buffer
        ;; Set buffer-local ring-bell-function to handle bells
        (setq-local ring-bell-function #'enkan-repl--bell-handler)
        (setq-local visible-bell nil)
        (message "Bell handler configured for enkan-repl session in buffer: %s" (buffer-name))))))

(provide 'enkan-repl-mac-notify)

;;; enkan-repl-mac-notify.el ends here
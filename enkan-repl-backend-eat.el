;;; enkan-repl-backend-eat.el --- EAT backend wrapper for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: enkan-repl team
;; Keywords: terminals, processes
;; Package-Requires: ((emacs "28.2") (eat "0.9.4"))

;; This file is part of enkan-repl.

;;; Commentary:

;; This module provides a thin wrapper around the EAT terminal emulator
;; for enkan-repl.  It isolates EAT-specific implementation details
;; and provides a consistent interface for terminal operations.

;;; Code:

;; Require eat - essential dependency
(require 'eat nil t)  ; noerror flag for testing environments

;; Declare external variables to silence byte-compiler
(defvar eat--process)
(defvar eat-mode)

;; Declare external functions to silence byte-compiler
(declare-function eat "eat" (&optional program))
(declare-function eat--send-string "eat" (process string))

(defun enkan-repl--backend-eat-available-p ()
  "Check if EAT backend is available."
  (featurep 'eat))

(defun enkan-repl--backend-eat-start ()
  "Start a new EAT terminal session in current directory.
Returns the created buffer or nil on failure."
  (if (enkan-repl--backend-eat-available-p)
      (eat)
    (error "EAT backend is not available. Please install eat package")))

(defun enkan-repl--backend-eat-buffer-p (buffer)
  "Check if BUFFER is an EAT terminal buffer."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (boundp 'eat-mode) eat-mode))))

(defun enkan-repl--backend-eat-process-live-p (buffer)
  "Check if BUFFER has a live EAT process."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (and (boundp 'eat--process)
           eat--process
           (process-live-p eat--process)))))

(defun enkan-repl--backend-eat-send-string (buffer string)
  "Send STRING to EAT process in BUFFER.
Returns t on success, nil on failure."
  (when (enkan-repl--backend-eat-process-live-p buffer)
    (with-current-buffer buffer
      (eat--send-string eat--process string)
      t)))

(defun enkan-repl--backend-eat-send-text (buffer text)
  "Send TEXT with carriage return to EAT process in BUFFER.
Returns t on success, nil on failure."
  (when (enkan-repl--backend-eat-send-string buffer text)
    (enkan-repl--backend-eat-send-string buffer "\r")))

(defun enkan-repl--backend-eat-send-key (buffer key)
  "Send special KEY to EAT process in BUFFER.
KEY can be :enter, :escape, or a number 1-9.
Returns t on success, nil on failure."
  (let ((key-string
         (cond
          ((eq key :enter) "\r")
          ((eq key :escape) "\e")
          ((and (numberp key) (>= key 1) (<= key 9))
           (number-to-string key))
          (t nil))))
    (when key-string
      (enkan-repl--backend-eat-send-string buffer key-string))))

(provide 'enkan-repl-backend-eat)

;;; enkan-repl-backend-eat.el ends here
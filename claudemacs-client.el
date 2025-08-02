;;; claudemacs-client.el --- Enhanced client utilities for claudemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1") (claudemacs "0.1.0"))
;; Keywords: claudemacs ai tools convenience
;; URL: https://github.com/phasetr/claudemacs-client
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

;; claudemacs-client provides enhanced utilities for working with claudemacs.
;;
;; Features:
;; - Send text from files, buffers, and regions to claudemacs
;; - Dedicated input buffer with org-mode support
;; - History management for sent content
;; - Multi-project support (planned)
;; - Enhanced key bindings for efficient workflow
;;
;; Installation:
;; Add to your init.el:
;;   (require 'claudemacs-client)
;;
;; Usage:
;; M-x claudemacs-client-create-input-buffer
;; C-M-d (global binding)
;;
;; Acknowledgments:
;; This project is built on top of claudemacs. We thank the original
;; authors for their excellent work.

;;; Code:

(require 'cl-lib)

;; Compatibility check
(when (locate-library "claudemacs")
  (require 'claudemacs))

;;;; Custom Variables

(defgroup claudemacs-client nil
  "Enhanced client utilities for claudemacs."
  :group 'tools
  :prefix "claudemacs-client-")

(defcustom claudemacs-client-input-buffer-name "*Claude Input*"
  "Name of the dedicated input buffer."
  :type 'string
  :group 'claudemacs-client)

(defcustom claudemacs-client-max-history-size 50
  "Maximum number of items to keep in input history."
  :type 'integer
  :group 'claudemacs-client)

(defvar claudemacs-client-input-history '()
  "History of texts sent to claudemacs.")

;;;; Core Functions

(defun claudemacs-client--get-buffer ()
  "Get the claudemacs buffer if it exists and is live."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (buffer-live-p buf)
                       (let ((name (buffer-name buf)))
                         (or (string= name "*claude*")
                             (string= name "*claudemacs*")
                             (string-match-p "^\\*claudemacs:" name)
                             (and (boundp 'claudemacs-mode) claudemacs-mode)
                             (and (boundp 'eat-mode) eat-mode
                                  (string-match-p "claude" name)))))))
              (buffer-list)))

(defun claudemacs-client--send-text (text)
  "Send TEXT to claudemacs buffer."
  (when-let ((claude-buffer (claudemacs-client--get-buffer)))
    (with-current-buffer claude-buffer
      (when (and (boundp 'eat--process) eat--process)
        (eat--send-string eat--process text)
        (eat--send-string eat--process "\r")
        (claudemacs-client--save-to-history text)
        t))))

(defun claudemacs-client--save-to-history (text)
  "Save TEXT to input history, maintaining maximum size."
  (push text claudemacs-client-input-history)
  (when (> (length claudemacs-client-input-history)
           claudemacs-client-max-history-size)
    (setq claudemacs-client-input-history
          (butlast claudemacs-client-input-history))))

;;;; Public API

;;;###autoload
(defun claudemacs-client-send-region (start end)
  "Send the text in region from START to END to claudemacs."
  (interactive "r")
  (when (use-region-p)
    (let ((content (buffer-substring-no-properties start end)))
      (if (claudemacs-client--send-text content)
          (message "Region sent to Claude (%d characters)" (length content))
        (message "Failed to send region - claudemacs buffer not available")))))

;;;###autoload
(defun claudemacs-client-send-buffer ()
  "Send the entire current buffer to claudemacs."
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (if (claudemacs-client--send-text content)
        (message "Buffer sent to Claude: %s (%d characters)"
                 (buffer-name) (length content))
      (message "Failed to send buffer - claudemacs buffer not available"))))

;;;###autoload
(defun claudemacs-client-send-from-cursor ()
  "Send text from cursor position to end of buffer to claudemacs."
  (interactive)
  (let ((content (buffer-substring-no-properties (point) (point-max))))
    (if (or (not content) (string-match-p "\\`[[:space:]]*\\'" content))
        (message "No content from cursor to end of buffer")
      (if (claudemacs-client--send-text content)
          (message "Text from cursor sent to Claude (%d characters)"
                   (length content))
        (message "Failed to send text - claudemacs buffer not available")))))

;;;###autoload
(defun claudemacs-client-create-input-buffer ()
  "Create or switch to a dedicated input buffer for composing text to send to Claude."
  (interactive)
  (let ((buffer-name claudemacs-client-input-buffer-name))
    (let ((input-buffer (get-buffer-create buffer-name)))
      (with-current-buffer input-buffer
        ;; Set up org mode for better editing experience
        (when (fboundp 'org-mode)
          (unless (eq major-mode 'org-mode)
            (let ((org-mode-hook nil))
              (org-mode))))

        ;; Set up key bindings
        (claudemacs-client--setup-input-buffer-keys)

        ;; Add helpful header if buffer is empty
        (when (= (point-min) (point-max))
          (insert "* Claude Input Buffer\n\n")
          (insert "Write your text here and use the following keys:\n\n")
          (insert "- C-c C-c: Send from cursor to end\n")
          (insert "- C-c C-r: Send selected region\n")
          (insert "- C-c C-b: Send entire buffer\n")
          (insert "- C-c C-h: Insert from history\n\n")
          (insert "** Connection Status: ")
          (if (claudemacs-client--get-buffer)
              (insert "✓ Connected\n")
            (insert "✗ Not connected (start claudemacs first)\n"))
          (insert "\n-----\n\n")))

      (switch-to-buffer input-buffer)
      (goto-char (point-max))
      (message "Claude input buffer ready. C-c C-c to send from cursor."))))

(defun claudemacs-client--setup-input-buffer-keys ()
  "Set up key bindings for the Claude input buffer."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claudemacs-client-send-from-cursor)
    (define-key map (kbd "C-c C-r")
      (lambda ()
        (interactive)
        (if (use-region-p)
            (claudemacs-client-send-region (region-beginning) (region-end))
          (message "No region selected"))))
    (define-key map (kbd "C-c C-b") #'claudemacs-client-send-buffer)
    (define-key map (kbd "C-c C-h") #'claudemacs-client-insert-from-history)

    ;; Set up as minor mode
    (setq-local minor-mode-map-alist
                (cons (cons 'claudemacs-client-input-mode map)
                      (assq-delete-all 'claudemacs-client-input-mode
                                       minor-mode-map-alist)))
    (setq-local claudemacs-client-input-mode t)))

;;;###autoload
(defun claudemacs-client-insert-from-history ()
  "Insert previously sent text from history."
  (interactive)
  (if claudemacs-client-input-history
      (let ((selected (completing-read "Insert from history: "
                                       claudemacs-client-input-history)))
        (insert selected))
    (message "No history available")))

;;;###autoload
(defun claudemacs-client-show-status ()
  "Show status of claudemacs-client."
  (interactive)
  (let ((claude-buffer (claudemacs-client--get-buffer)))
    (message "claudemacs-client | History: %d items | Connection: %s"
             (length claudemacs-client-input-history)
             (if claude-buffer
                 (format "Connected (%s)" (buffer-name claude-buffer))
               "Not connected"))))

;;;; Setup

;; Global key binding
;;;###autoload
(define-key global-map (kbd "C-M-d") #'claudemacs-client-create-input-buffer)

;;;###autoload
(defun claudemacs-client-version ()
  "Show the version of claudemacs-client."
  (interactive)
  (message "claudemacs-client version 0.0.0"))

(provide 'claudemacs-client)

;;; claudemacs-client.el ends here

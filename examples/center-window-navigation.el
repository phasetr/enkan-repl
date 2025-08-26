;;; center-window-navigation.el --- Center file window layout and navigation for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2024 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;; Center file window layout and navigation utilities extracted from enkan-repl.el
;; This file provides complete window layout management and navigation functions
;; for center file multi-buffer access pattern.

;;; Code:

;;;; Pure Functions for Project Path Resolution

(defun enkan-repl--get-session-project-path-pure (session-number session-list project-registry)
  "Get project path for SESSION-NUMBER from SESSION-LIST and PROJECT-REGISTRY.
Pure function that returns project path string or nil.
SESSION-LIST format: ((session-number . project-name) ...)
PROJECT-REGISTRY format: ((alias . (project-name . project-path)) ...)"
  (let* ((session-project (cdr (assoc session-number session-list)))
         (project-info (when session-project
                         (cl-find-if (lambda (entry)
                                       (string= (car (cdr entry)) session-project))
                                     project-registry)))
         (project-path (when project-info (cdr (cdr project-info)))))
    project-path))

(defun enkan-repl--setup-window-dired-pure (window session-number session-list project-registry)
  "Pure function to determine dired setup for WINDOW and SESSION-NUMBER.
Returns cons (window . project-path) or nil if invalid."
  (let ((project-path (enkan-repl--get-session-project-path-pure
                       session-number session-list project-registry)))
    (when (and project-path (file-directory-p (expand-file-name project-path)))
      (cons window project-path))))

(defun enkan-repl--setup-window-eat-buffer-pure (window session-number session-list project-registry)
  "Pure function to determine eat buffer setup for WINDOW and SESSION-NUMBER.
Returns cons (window . buffer-name) or nil if session not registered."
  (let ((project-name (cdr (assoc session-number session-list))))
    (when project-name
      (let ((project-path (enkan-repl--get-project-path-from-registry project-name project-registry)))
        (when project-path
          (let* ((expanded-path (expand-file-name project-path))
                 (buffer-name (format "*enkan:%s*" expanded-path)))
            (cons window buffer-name)))))))

;;;; Window Layout Variables

(defvar enkan-repl--window-1 nil
  "Window 1 (center file) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-2 nil
  "Window 2 (work area) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-3 nil
  "Window 3 (reserve area) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-4 nil
  "Window 4 (session 1) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-5 nil
  "Window 5 (session 2) for enkan-repl multi-buffer layout.")


;;;; Window Layout Functions

;; Helper function for setting up eat buffer in window
(defun enkan-repl--setup-session-eat-buffer (window session-number)
  "Setup eat buffer for SESSION-NUMBER in WINDOW."
  (let ((eat-setup (enkan-repl--setup-window-eat-buffer-pure
                     window session-number enkan-repl-session-list enkan-repl-center-project-registry)))
    (if eat-setup
      (let ((buffer (get-buffer (cdr eat-setup))))
        (if buffer
          (progn
            (select-window (car eat-setup))
            (switch-to-buffer buffer)
            (message "✅ Window %d: Opened eat buffer %s"
                    session-number (cdr eat-setup)))
          (message "❌ Window %d: Eat buffer %s not found. Run C-M-s to start sessions."
            session-number (cdr eat-setup))))
      (message "❌ Window %d: No session registered for slot %d (internal number %d)."
        session-number session-number session-number))))

;;;###autoload
(defun enkan-repl-setup-2session-layout ()
  "Setup window layout for 2-session management.
  +--------+---+---+
  |    1   | 2 | 3 |
  | center |   |   |
  |  file  |   |   |
  +-----+--+---+---+

  1: Center file (command source)
  4, 5: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.35)))
  ;; Move to right section and split into 3 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.55)))
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup eat buffers in session windows (1, 2 for multi-project order)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-2 1)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-3 2))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-3session-layout ()
  "Setup window layout for 3-session management.
  +--------+---+---+---+
  |    1   | 2 | 3 | 4 |
  | center |   |   |   |
  |  file  |   |   |   |
  +--------+---+---+---+

  4, 5, 6: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 3 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.5)))
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup eat buffers in session windows (4, 5, 6 in multi-project order)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-2 1)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-3 2)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-4 3))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))

;;;###autoload
(defun enkan-repl-setup-4session-layout ()
  "Setup window layout for 4-session management.
  +--------+---+---+---+---+
  |    1   | 2 | 3 | 4 | 5 |
  | center |   |   |   |   |
  |  file  |   |   |   |   |
  +--------+---+---+---+---+

  4, 5, 6, 7: enkan-repl sessions

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; Create left column for center file (30%) and right section (70%)
  (split-window-right (floor (* (window-width) 0.3)))
  ;; Move to right section and split into 4 columns
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.3)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.4)))
  (other-window 1)
  (split-window-right)
  ;; Set window variables - direct assignment by position
  ;; Currently at rightmost window, go back to leftmost
  (other-window 2)
  (setq enkan-repl--window-1 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-2 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-3 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-4 (selected-window))
  (other-window 1)
  (setq enkan-repl--window-5 (selected-window))
  ;; Open center file in window 1
  (when (and enkan-repl--window-1 enkan-repl-center-file)
    (select-window enkan-repl--window-1)
    (find-file enkan-repl-center-file)
    (message "✅ Window 1: Opened center file %s" enkan-repl-center-file))
  ;; Setup eat buffers in session windows (4, 5, 6, 7 in multi-project order)
  (when (and (boundp 'enkan-repl-session-list) enkan-repl-session-list)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-2 1)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-3 2)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-4 3)
    (enkan-repl--setup-session-eat-buffer enkan-repl--window-5 4))
  ;; Always select the center file window (Window 1) at the end
  (when enkan-repl--window-1
    (select-window enkan-repl--window-1)))


;;;; Window Navigation Functions

(defun enkan-repl-goto-window (window-number)
  "Move to the specified window number.
window-number: Integer 1-7

Category: Utilities"
  (interactive "nWindow number (1-7): ")
  (unless (<= 1 window-number 7)
    (user-error "Window number must be 1-7"))

  (let ((target-window (nth (1- window-number) (window-list nil 'no-minibuf))))
    (if target-window
        (select-window target-window)
      (user-error "Window %d does not exist" window-number))))

;; Individual window navigation functions for keybindings
;;;###autoload
(defun enkan-repl-goto-window-1 () "Go to window 1" (interactive) (enkan-repl-goto-window 1))
;;;###autoload
(defun enkan-repl-goto-window-2 () "Go to window 2" (interactive) (enkan-repl-goto-window 2))
;;;###autoload
(defun enkan-repl-goto-window-3 () "Go to window 3" (interactive) (enkan-repl-goto-window 3))
;;;###autoload
(defun enkan-repl-goto-window-4 () "Go to window 4" (interactive) (enkan-repl-goto-window 4))
;;;###autoload
(defun enkan-repl-goto-window-5 () "Go to window 5" (interactive) (enkan-repl-goto-window 5))
;;;###autoload
(defun enkan-repl-goto-window-6 () "Go to window 6" (interactive) (enkan-repl-goto-window 6))
;;;###autoload
(defun enkan-repl-goto-window-7 () "Go to window 7" (interactive) (enkan-repl-goto-window 7))

;;;; Center file multi-buffer window navigation
(defvar enkan-repl-center--current-window 1
  "Current window number for center file layout cycling.")

(provide 'center-window-navigation)
;;; center-window-navigation.el ends here

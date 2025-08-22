;;; center-window-navigation.el --- Center file window layout and navigation for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2024 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;; Center file window layout and navigation utilities extracted from enkan-repl.el
;; This file provides complete window layout management and navigation functions
;; for center file multi-buffer access pattern.

;;; Code:

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

(defvar enkan-repl--window-6 nil
  "Window 6 (session 3) for enkan-repl multi-buffer layout.")

(defvar enkan-repl--window-7 nil
  "Window 7 (session 4) for enkan-repl multi-buffer layout.")

;;;; Window Layout Functions

;;;###autoload
(defun enkan-repl-setup-2session-layout ()
  "2セッション管理用のウインドウレイアウトを設定.
  +----------+---+---+
  |    1     | 4 | 5 |
  |  中心    |   |   |
  |ファイル  |   |   |
  +-----+----|   |   |
  | 2   | 3  |   |   |
  |作業 |予備|   |   |
  +-----+----+---+---+

  1: 中心ファイル（送信元）
  2: 作業・調査用（雑多なファイル・バッファ）
  3: 予備エリア（magitコミット等の一時バッファ用）
  4, 5: enkan-replセッション

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; 右側に2列作成
  (split-window-right (floor (* (window-width) 0.6)))
  (other-window 1)
  (split-window-right)
  ;; 左下に分割
  (other-window 2)
  (split-window-below (floor (* (window-height) 0.65)))
  (other-window 1)
  (split-window-right (floor (* (window-width) 0.5)))
  (other-window 4)
  (balance-windows)
  ;; Set window variables for center file layout
  (let ((windows (window-list)))
    (setq enkan-repl--window-1 (nth 0 windows))  ; Center file
    (setq enkan-repl--window-2 (nth 1 windows))  ; Work area
    (setq enkan-repl--window-3 (nth 2 windows))  ; Reserve area
    (setq enkan-repl--window-4 (nth 3 windows))  ; Session 1
    (setq enkan-repl--window-5 (nth 4 windows))) ; Session 2
  (select-window (car (window-list))))

;;;###autoload
(defun enkan-repl-setup-3session-layout ()
  "3セッション管理用のウインドウレイアウトを設定.
  +----------+---+---+---+
  |    1     | 4 | 5 | 6 |
  |  中心    |   |   |   |
  |ファイル  |   |   |   |
  +-----+----+   |   |   |
  | 2   | 3  |   |   |   |
  |作業 |予備|   |   |   |
  +-----+----+---+---+---+

  4, 5, 6: enkan-replセッション

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; 右側に3列作成
  (split-window-right (floor (* (window-width) 0.6)))
  (other-window 1)
  (split-window-right)
  (split-window-right)
  ;; 左下に分割
  (other-window 2)
  (split-window-below (floor (* (window-height) 0.6)))
  (split-window-right (floor (* (window-width) 0.5)))
  (balance-windows)
  ;; Set window variables for center file layout
  (let ((windows (window-list)))
    (setq enkan-repl--window-1 (nth 0 windows))  ; Center file
    (setq enkan-repl--window-2 (nth 1 windows))  ; Work area
    (setq enkan-repl--window-3 (nth 2 windows))  ; Reserve area
    (setq enkan-repl--window-4 (nth 3 windows))  ; Session 1
    (setq enkan-repl--window-5 (nth 4 windows))  ; Session 2
    (setq enkan-repl--window-6 (nth 5 windows))) ; Session 3
  (select-window (car (window-list))))

;;;###autoload
(defun enkan-repl-setup-4session-layout ()
  "4セッション管理用のウインドウレイアウトを設定.
  +----------+---+---+---+---+
  |    1     | 4 | 5 | 6 | 7 |
  |  中心    |   |   |   |   |
  |ファイル  |   |   |   |   |
  +-----+----|   |   |   |   |
  | 2   | 3  |   |   |   |   |
  |作業 |予備|   |   |   |   |
  +-----+----+---+---+---+---+

  4, 5, 6, 7: enkan-replセッション

Category: Utilities"
  (interactive)
  (delete-other-windows)
  ;; 右側に4列作成
  (split-window-right (floor (* (window-width) 0.5)))
  (split-window-right)
  (split-window-right)
  (split-window-right)
  ;; 左下に分割
  (split-window-below (floor (* (window-height) 0.6)))
  (split-window-right (floor (* (window-width) 0.5)))
  (balance-windows)
  ;; Set window variables for center file layout
  (let ((windows (window-list)))
    (setq enkan-repl--window-1 (nth 0 windows))  ; Center file
    (setq enkan-repl--window-2 (nth 1 windows))  ; Work area
    (setq enkan-repl--window-3 (nth 2 windows))  ; Reserve area
    (setq enkan-repl--window-4 (nth 3 windows))  ; Session 1
    (setq enkan-repl--window-5 (nth 4 windows))  ; Session 2
    (setq enkan-repl--window-6 (nth 5 windows))  ; Session 3
    (setq enkan-repl--window-7 (nth 6 windows))) ; Session 4
  (select-window (car (window-list))))

;;;; Window Navigation Functions

(defun enkan-repl-goto-window (window-number)
  "指定されたウインドウ番号に移動.
window-number: 1-7の整数

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

;;;###autoload
(defun enkan-repl-center-other-window ()
  "Switch between windows 1-3 in center file layout.
Window 1: Center file, Window 2: Work area, Window 3: Reserve area.

Category: Center File Multi-buffer Access"
  (interactive)
  (let* ((window-list (window-list nil 'no-minibuf))
         (max-window (min 3 (length window-list))))
    (when (> max-window 0)
      (setq enkan-repl-center--current-window
            (1+ (mod enkan-repl-center--current-window max-window)))
      (when (> enkan-repl-center--current-window max-window)
        (setq enkan-repl-center--current-window 1))
      (let ((target-window (nth (1- enkan-repl-center--current-window) window-list)))
        (when target-window
          (select-window target-window)
          (message "Switched to window %d" enkan-repl-center--current-window))))))

(provide 'center-window-navigation)
;;; center-window-navigation.el ends here

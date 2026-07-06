;;; enkan-repl-transcript.el --- Read AI CLI chat transcripts -*- lexical-binding: t -*-

;; Copyright (C) 2025 phasetr

;; Author: phasetr <phasetr@gmail.com>
;; Keywords: convenience, tools

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Full-screen AI CLIs (Claude Code, codex) redraw their viewport in place and
;; do not push the conversation into tmux scrollback, so `tmux capture-pane'
;; can only return the current screen.  Each CLI, however, writes its own
;; conversation transcript to disk.  This module reads that transcript for a
;; pane's working directory so the full chat history can be shown in Emacs,
;; independent of tmux.
;;
;; Claude Code stores one JSONL file per session under
;; `~/.claude/projects/NAME/', where NAME is the project's working directory
;; with every non-alphanumeric character replaced by a hyphen.  Each line is a
;; JSON event; `user' and `assistant' events carry the conversation.
;;
;; The pure parsing/formatting helpers are kept separate from the I/O and
;; display helpers so they can be tested deterministically.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function enkan-repl--terminal-tmux--call "enkan-repl-terminal" (args &optional capture))
(declare-function enkan-repl--terminal-tmux--target "enkan-repl-terminal" (id))

(defgroup enkan-repl-transcript nil
  "Reading AI CLI chat transcripts for enkan-repl."
  :group 'enkan-repl)

(defcustom enkan-repl-transcript-max-turns 400
  "Maximum number of recent user/assistant turns to show in a transcript.
When nil or non-positive, show the whole transcript."
  :type '(choice (const :tag "All" nil) integer)
  :group 'enkan-repl-transcript)

(defcustom enkan-repl-claude-projects-directory "~/.claude/projects/"
  "Directory where Claude Code stores per-project session transcripts."
  :type 'directory
  :group 'enkan-repl-transcript)

;;;; Pure helpers

(defun enkan-repl--transcript-encode-project-dir (cwd)
  "Return the Claude Code project-directory name for absolute path CWD.
Claude Code replaces every non-alphanumeric character of the working
directory with a hyphen (for example \"/Users/me/dev/app\" becomes
\"-Users-me-dev-app\").  This is a pure function."
  (replace-regexp-in-string "[^A-Za-z0-9]" "-" cwd))

(defun enkan-repl--transcript-role-marker (role)
  "Return a short display marker for conversation ROLE.  Pure function."
  (cond ((equal role "user") "▶")
        ((equal role "assistant") "◀")
        (t "•")))

(defun enkan-repl--transcript-claude-extract-text (content)
  "Return readable text from a Claude Code message CONTENT.
CONTENT is either a string or a list of block alists; only `text' blocks
contribute (thinking, tool calls, and tool results are dropped).  Returns a
string, possibly empty.  This is a pure function."
  (cond
   ((stringp content) content)
   ((listp content)
    (mapconcat
     (lambda (block)
       (if (and (consp block)
                (equal (alist-get 'type block) "text"))
           (or (alist-get 'text block) "")
         ""))
     content ""))
   (t "")))

(defun enkan-repl--transcript-claude-parse-line (line)
  "Parse a Claude Code transcript JSONL LINE into a (:role :text) plist.
Return nil for blank lines, unparseable lines, non-conversation events,
sidechain/meta entries, or entries whose extracted text is empty.  This is a
pure function."
  (when (and (stringp line) (not (string-empty-p (string-trim line))))
    (let ((obj (ignore-errors
                 (json-parse-string line
                                    :object-type 'alist
                                    :array-type 'list))))
      (when (and obj (listp obj))
        (let ((type (alist-get 'type obj))
              (sidechain (alist-get 'isSidechain obj))
              (meta (alist-get 'isMeta obj)))
          (when (and (member type '("user" "assistant"))
                     (not (eq sidechain t))
                     (not (eq meta t)))
            (let* ((message (alist-get 'message obj))
                   (role (or (and (listp message) (alist-get 'role message))
                             type))
                   (text (string-trim
                          (enkan-repl--transcript-claude-extract-text
                           (and (listp message) (alist-get 'content message))))))
              (unless (string-empty-p text)
                (list :role role :text text)))))))))

(defun enkan-repl--transcript-claude-format (lines &optional max-turns)
  "Format Claude Code transcript LINES (a list of JSONL strings) into text.
Keep only the last MAX-TURNS user/assistant turns when MAX-TURNS is a positive
integer.  Each turn is rendered as a role-marked block.  This is a pure
function."
  (let* ((turns (delq nil (mapcar #'enkan-repl--transcript-claude-parse-line
                                  lines)))
         (turns (if (and (integerp max-turns) (> max-turns 0)
                         (> (length turns) max-turns))
                    (last turns max-turns)
                  turns)))
    (mapconcat
     (lambda (turn)
       (let ((role (plist-get turn :role)))
         (format "%s %s\n%s"
                 (enkan-repl--transcript-role-marker role)
                 role
                 (plist-get turn :text))))
     turns "\n\n")))

;;;; I/O helpers

(defun enkan-repl--transcript-claude-dir (cwd)
  "Return the Claude Code transcript directory for CWD, or nil when absent."
  (let ((dir (expand-file-name
              (enkan-repl--transcript-encode-project-dir
               (directory-file-name (expand-file-name cwd)))
              (expand-file-name enkan-repl-claude-projects-directory))))
    (and (file-directory-p dir) dir)))

(defun enkan-repl--transcript-claude-latest-file (cwd)
  "Return the most recently modified Claude Code transcript file for CWD.
Return nil when the project directory or any transcript is absent."
  (let ((dir (enkan-repl--transcript-claude-dir cwd)))
    (when dir
      (car (sort (directory-files dir t "\\.jsonl\\'")
                 (lambda (a b)
                   (time-less-p (file-attribute-modification-time
                                 (file-attributes b))
                                (file-attribute-modification-time
                                 (file-attributes a)))))))))

(defun enkan-repl--transcript-file-lines (file)
  "Return the non-empty lines of FILE as a list of strings, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t))))

(defun enkan-repl--transcript-claude-load (cwd &optional max-turns)
  "Return a plist (:file FILE :text TEXT) for CWD's Claude Code transcript.
Return nil when no transcript exists.  MAX-TURNS bounds the number of turns."
  (let ((file (enkan-repl--transcript-claude-latest-file cwd)))
    (when file
      (list :file file
            :text (enkan-repl--transcript-claude-format
                   (enkan-repl--transcript-file-lines file) max-turns)))))

(defun enkan-repl--transcript-tmux-cwd (id)
  "Return the working directory of tmux terminal ID, or nil."
  (when (and id
             (fboundp 'enkan-repl--terminal-tmux--call)
             (fboundp 'enkan-repl--terminal-tmux--target))
    (let ((out (enkan-repl--terminal-tmux--call
                (list "display-message" "-p" "-t"
                      (enkan-repl--terminal-tmux--target id)
                      "#{pane_current_path}")
                t)))
      (when (and out (not (string-empty-p (string-trim out))))
        (string-trim out)))))

(defun enkan-repl--transcript-buffer-cwd (buffer)
  "Return the working directory associated with terminal BUFFER, or nil."
  (when (buffer-live-p buffer)
    (or (enkan-repl--transcript-tmux-cwd
         (buffer-local-value 'enkan-repl--tmux-mirror-id buffer))
        (buffer-local-value 'default-directory buffer))))

(defun enkan-repl--transcript-display (file text cwd)
  "Show transcript TEXT read from FILE for project CWD in a dedicated buffer.
Return the buffer."
  (let ((buf (get-buffer-create
              (format "*enkan-transcript: %s*"
                      (file-name-nondirectory (directory-file-name cwd))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# transcript: %s\n# source: %s\n\n" cwd file))
        (insert (or text ""))
        (goto-char (point-max)))
      (setq buffer-read-only t)
      (setq-local default-directory (file-name-as-directory cwd)))
    (display-buffer buf)
    buf))

(provide 'enkan-repl-transcript)

;;; enkan-repl-transcript.el ends here

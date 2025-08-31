;;; enkan-repl-utils.el --- Utility functions for enkan-repl -*- lexical-binding: t -*-

;; Copyright (C) 2025 [phasetr]

;; Author: [phasetr] <phasetr@gmail.com>
;; Keywords: tools
;; Package-Requires: ((emacs "28.2") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions for enkan-repl package.
;; This file provides shared functionality for documentation generation
;; and dynamic function listing that can be used by both the main package
;; and build/documentation scripts.

;;; Code:

(require 'cl-lib)

;;;; Session List Pure Functions

(defun enkan-repl--extract-project-name (buffer-name-or-path)
  "Extract final directory name from buffer name or path for use as project name.
Example: \\='*enkan:/path/to/pt-tools/*\\=' -> \\='pt-tools\\='"
  (let ((path (if (string-match "\\*enkan:\\(.+\\)\\*" buffer-name-or-path)
                  (match-string 1 buffer-name-or-path)
                buffer-name-or-path)))
    (file-name-nondirectory (directory-file-name path))))

(defun enkan-repl--encode-full-path (path prefix separator)
  "Pure function: Encode PATH with PREFIX and SEPARATOR.
PATH: Directory path to encode.
PREFIX: Prefix string to add (e.g., \\='enkan\\=').
SEPARATOR: String to replace '/' with (e.g., '--').
Example: \='/Users/project\=' + \='enkan\=' + \='--\=' -> \='enkan--Users--project\='"
  (let*
      ((expanded-path (expand-file-name path))
       (cleaned-path
        (if
            (string-suffix-p "/" expanded-path)
            (substring expanded-path 0 -1)
          expanded-path)))
    (concat prefix (replace-regexp-in-string "/" separator cleaned-path))))

(defun enkan-repl--decode-full-path (encoded-name prefix separator)
  "Pure function: Decode ENCODED-NAME with PREFIX and SEPARATOR.
ENCODED-NAME: Encoded filename to decode.
PREFIX: Expected prefix string (e.g., \\='enkan\\=').
SEPARATOR: String to replace with '/' (e.g., '--').
Example: \='enkan--Users--project\=' + \='enkan\=' + \='--\=' -> \='/Users/project/\='"
  (when (string-prefix-p prefix encoded-name)
    (let
        ((path-part (substring encoded-name (length prefix))))  ; Remove prefix
      (concat (replace-regexp-in-string (regexp-quote separator) "/" path-part) "/"))))

(defun enkan-repl--extract-directory-from-buffer-name (buffer-name)
  "Pure function to extract expanded directory path from enkan buffer name.
Returns expanded directory path or nil if buffer name is not valid enkan format."
  (when (and (stringp buffer-name) (string-match "^\\*enkan:\\(.*\\)\\*$" buffer-name))
    (let ((raw-path (match-string 1 buffer-name)))
      (file-name-as-directory (expand-file-name raw-path)))))

(defun enkan-repl--make-buffer-name (path)
  "Create buffer name for given PATH.
Returns buffer name in format *enkan:<expanded-path>*"
  (format "*enkan:%s*" (expand-file-name path)))

(defun enkan-repl--buffer-matches-directory (buffer-name target-directory)
  "Pure function to check if buffer name matches target directory.
Returns t if buffer is enkan buffer for target directory, nil otherwise."
  (and (stringp buffer-name)
       (stringp target-directory)
       (string-match-p "^\\*enkan:" buffer-name)
       (string= buffer-name (enkan-repl--make-buffer-name target-directory))))

(defun enkan-repl--extract-session-info (buffer-name buffer-live-p has-eat-process process-live-p)
  "Pure function to extract session info from buffer properties.
BUFFER-NAME is the name of the buffer.
BUFFER-LIVE-P indicates if buffer is live.
HAS-EAT-PROCESS indicates if buffer has eat--process variable.
PROCESS-LIVE-P indicates if the process is alive.
Returns a plist with :name, :directory, and :status, or nil if not a session."
  (when (and buffer-name
             buffer-live-p
             (string-match "^\\*enkan:\\(.*?\\)\\*$" buffer-name))
    (let ((dir (match-string 1 buffer-name))
          (status (if (and has-eat-process process-live-p)
                      'alive
                    'dead)))
      (list :name buffer-name
            :directory dir
            :status status))))

(defun enkan-repl--collect-sessions (buffer-info-list)
  "Pure function to collect session info from buffer info list.
BUFFER-INFO-LIST is a list of plists with buffer properties.
Each plist should have :name, :live-p, :has-eat-process, and :process-live-p.
Returns a list of session info plists."
  (let ((sessions '()))
    (dolist (buffer-info buffer-info-list)
      (let ((session-info
             (enkan-repl--extract-session-info
              (plist-get buffer-info :name)
              (plist-get buffer-info :live-p)
              (plist-get buffer-info :has-eat-process)
              (plist-get buffer-info :process-live-p))))
        (when session-info
          (push session-info sessions))))
    (nreverse sessions)))

(defun enkan-repl--format-sessions (sessions)
  "Pure function to format sessions for display.
SESSIONS is a list of session info plists.
Returns a formatted string for display."
  (if (null sessions)
      "No active sessions found\n"
    (concat "Active enkan-repl sessions:\n"
            "Press 'd' to delete a session, 'q' to quit\n"
            "─────────────────────────────────────────\n\n"
            (mapconcat
             (lambda (session)
               (format "  %s\n    Directory: %s\n    Status: %s\n\n"
                       (plist-get session :name)
                       (plist-get session :directory)
                       (plist-get session :status)))
             sessions
             ""))))

(defun enkan-repl--find-deletion-bounds (lines current-line)
  "Pure function to find bounds for deletion.
LINES is a list of strings (buffer lines).
CURRENT-LINE is the current line number (0-indexed).
Returns a plist with :start-line and :end-line for deletion bounds,
or nil if not on a session entry."
  (when (and (>= current-line 0)
             (< current-line (length lines)))
    (let ((line (nth current-line lines)))
      ;; Check if we're on or near a session entry
      (cond
       ;; On session name line
       ((string-match "^  \\*enkan:" line)
        (list :start-line current-line
              :end-line (min (+ current-line 4) (length lines))))
       ;; On directory line
       ((and (> current-line 0)
             (string-match "^    Directory:" line)
             (string-match "^  \\*enkan:" (nth (1- current-line) lines)))
        (list :start-line (1- current-line)
              :end-line (min (+ current-line 3) (length lines))))
       ;; On status line
       ((and (> current-line 1)
             (string-match "^    Status:" line)
             (string-match "^  \\*enkan:" (nth (- current-line 2) lines)))
        (list :start-line (- current-line 2)
              :end-line (min (+ current-line 2) (length lines))))
       (t nil)))))

(defun enkan-repl--format-numbered-sessions (sessions)
  "Pure function to format numbered session list.
SESSIONS is a list of session info plists.
Returns a formatted string with numbered sessions."
  (let ((result "")
        (index 1))
    (dolist (session sessions)
      (setq result
            (concat result
                    (format "%d. %s — Directory: %s, Status: %s\n"
                            index
                            (plist-get session :name)
                            (plist-get session :directory)
                            (plist-get session :status))))
      (setq index (1+ index)))
    result))

(defun enkan-repl--format-minibuffer-sessions (sessions)
  "Pure function to format sessions for minibuffer display.
SESSIONS is a list of session info plists.
Returns a list of formatted strings, one per session."
  (let ((index 1)
        (result '()))
    (dolist (session sessions)
      (push (format "%d. %s — Directory: %s, Status: %s"
                    index
                    (plist-get session :name)
                    (plist-get session :directory)
                    (plist-get session :status))
            result)
      (setq index (1+ index)))
    (nreverse result)))

(defun enkan-repl--prepare-session-candidates (sessions)
  "Pure function to prepare candidates for completing-read.
SESSIONS is a list of session info plists.
Returns an alist of (NAME . DESCRIPTION)."
  (mapcar
   (lambda (session)
     (cons (plist-get session :name)
           (format "Directory: %s, Status: %s"
                   (plist-get session :directory)
                   (plist-get session :status))))
   sessions))

(defun enkan-repl--find-session-buffer (selected-name buffer-info-list)
  "Pure function to find buffer for selected session.
SELECTED-NAME is the selected session name.
BUFFER-INFO-LIST is the list of buffer info plists.
Returns the buffer object or nil."
  (let ((buf-info (cl-find-if
                   (lambda (info)
                     (equal (plist-get info :name) selected-name))
                   buffer-info-list)))
    (and buf-info (plist-get buf-info :buffer))))

(defun enkan-repl--session-action (action selected-name)
  "Pure function to determine session action result.
ACTION is the action character (?s, ?d, ?c).
SELECTED-NAME is the selected session name.
Returns a plist with :type and :message."
  (cl-case action
    (?s (list :type 'switch
              :message (format "Switched to session: %s" selected-name)))
    (?d (list :type 'delete
              :message (format "Session deleted: %s" selected-name)))
    (?c (list :type 'cancel
              :message "Cancelled"))
    (t (list :type 'unknown
             :message "Unknown action"))))

(defun enkan-repl-utils--extract-function-info (file-path)
  "Extract public function information from an Emacs Lisp file.
Returns a list of plists, each containing :name, :args, :docstring,
:interactive, and :autoload status."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((functions-info '())
          lines
          (current-line 0))
      ;; Split into lines for easier processing
      (setq lines (split-string (buffer-string) "\n"))
      (while (< current-line (length lines))
        (let ((line (nth current-line lines))
              (autoload-p nil))
          ;; Check for autoload marker
          (when (string-match "^;;;###autoload" line)
            (setq autoload-p t)
            (setq current-line (1+ current-line))
            (setq line (nth current-line lines)))
          ;; Check for defun
          (when (and line (string-match "^(defun\\s-+\\([^ \t\n(]+\\)\\s-+\\((.*)\\)" line))
            (let* ((func-name (match-string 1 line))
                   (args-str (match-string 2 line))
                   (args nil)
                   (docstring "")
                   (interactive-p nil))
              ;; Parse arguments safely
              (condition-case nil
                  (setq args (read args-str))
                (error (setq args nil)))
              ;; Look for docstring in next lines (support multi-line)
              (let ((next-line-idx (1+ current-line))
                    (in-docstring nil)
                    (docstring-lines '()))
                (while (and (< next-line-idx (length lines))
                            (< next-line-idx (+ current-line 10))) ; reasonable limit
                  (let ((next-line (nth next-line-idx lines)))
                    (cond
                     ;; Start of docstring
                     ((and (not in-docstring)
                           (string-match "^\\s-*\"\\(.*\\)\"\\s-*$" next-line))
                      ;; Single line docstring
                      (setq docstring (match-string 1 next-line))
                      (setq next-line-idx (length lines))) ; break
                     ((and (not in-docstring)
                           (string-match "^\\s-*\"\\(.*\\)" next-line))
                      ;; Start of multi-line docstring
                      (setq in-docstring t)
                      (push (match-string 1 next-line) docstring-lines))
                     ;; End of multi-line docstring
                     ((and in-docstring
                           (string-match "\\(.*\\)\"\\s-*$" next-line))
                      (push (match-string 1 next-line) docstring-lines)
                      (setq docstring (mapconcat 'identity (nreverse docstring-lines) " "))
                      (setq next-line-idx (length lines))) ; break
                     ;; Middle of multi-line docstring
                     (in-docstring
                      (push (string-trim next-line) docstring-lines))
                     ;; Not a docstring line, break
                     (t (setq next-line-idx (length lines)))))
                  (setq next-line-idx (1+ next-line-idx))))
              ;; Look for (interactive) in the function body
              ;; Only search until we find another defun or end of reasonable function scope
              (let ((body-idx (1+ current-line))
                    (paren-count 0)
                    (in-function t))
                (while (and (< body-idx (length lines))
                            (not interactive-p)
                            in-function
                            (< body-idx (+ current-line 20))) ; reasonable limit
                  (let ((body-line (nth body-idx lines)))
                    (when body-line
                      ;; Stop if we encounter another defun
                      (when (string-match "^(defun\\|^;;;###autoload" body-line)
                        (setq in-function nil))
                      ;; Look for interactive only if still in this function
                      (when (and in-function (string-match "(interactive" body-line))
                        (setq interactive-p t))))
                  (setq body-idx (1+ body-idx))))
              ;; Add to results if public
              (when (or interactive-p autoload-p)
                (push (list :name func-name
                            :args args
                            :docstring docstring
                            :interactive interactive-p
                            :autoload autoload-p)
                      functions-info))))
          (setq current-line (1+ current-line))))
      (nreverse functions-info))))

(defun enkan-repl-utils--generate-function-list-flat (functions)
  "Generate flat function list in org-mode format.
FUNCTIONS should be a list of function info plists from
`enkan-repl-utils--extract-function-info'."
  (let ((result ""))
    (dolist (func functions)
      (let* ((name (plist-get func :name))
             (docstring (plist-get func :docstring))
             (description (if (and docstring (not (zerop (length docstring))))
                              docstring
                            "Send command")))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name description)))))
    result))

(defun enkan-repl-utils--get-all-public-functions (file-path)
  "Get all public functions from FILE-PATH as flat org-mode list.
Returns a string containing org-mode formatted function list."
  (let* ((functions (enkan-repl-utils--extract-function-info file-path))
         (interactive-functions (cl-remove-if-not
                                (lambda (f) (plist-get f :interactive))
                                functions)))
    (enkan-repl-utils--generate-function-list-flat interactive-functions)))

(defun enkan-repl-utils--extract-category-from-docstring (docstring)
  "Extract category information from DOCSTRING.
Returns category string or nil if not found."
  (when (and docstring (stringp docstring))
    (if (string-match "Category: \\(.*\\)$" docstring)
        (match-string 1 docstring)
      nil)))

(defun enkan-repl-utils--get-categorized-functions (file-path)
  "Get functions organized by category from FILE-PATH.
Returns an alist where each element is (CATEGORY . FUNCTIONS-LIST).
FUNCTIONS-LIST contains plists with :name, :docstring, :category."
  (let ((functions-info (enkan-repl-utils--extract-function-info file-path))
        (categorized-functions '())
        (categories '("Command Palette" "Text Sender" "Session Controller" "Utilities")))
    ;; Extract categories from docstrings and group functions
    (dolist (func functions-info)
      (when (plist-get func :interactive)
        (let* ((docstring (plist-get func :docstring))
               (category (enkan-repl-utils--extract-category-from-docstring docstring))
               (clean-docstring (if category
                                   (replace-regexp-in-string "  Category: .*$" "" docstring)
                                 docstring))
               (func-info `(:name ,(plist-get func :name)
                           :docstring ,clean-docstring
                           :category ,(or category "Uncategorized"))))
          ;; Add to appropriate category list
          (let ((category-entry (assoc (or category "Uncategorized") categorized-functions)))
            (if category-entry
                (setcdr category-entry (append (cdr category-entry) (list func-info)))
              (push (cons (or category "Uncategorized") (list func-info)) categorized-functions))))))
    ;; Sort by predefined category order
    (let ((sorted-result '()))
      (dolist (category categories)
        (let ((category-functions (cdr (assoc category categorized-functions))))
          (when category-functions
            (push (cons category category-functions) sorted-result))))
      ;; Add any uncategorized functions at the end
      (let ((uncategorized (cdr (assoc "Uncategorized" categorized-functions))))
        (when uncategorized
          (push (cons "Uncategorized" uncategorized) sorted-result)))
      (nreverse sorted-result))))

(defun enkan-repl-utils--generate-org-section (category functions header-level)
  "Generate org-mode section for CATEGORY with FUNCTIONS at HEADER-LEVEL.
HEADER-LEVEL should be a number (1 for *, 2 for **, etc.)."
  (let ((header-prefix (make-string header-level ?*))
        (result ""))
    (setq result (concat result (format "%s %s\n\n" header-prefix category)))
    (dolist (func functions)
      (let ((name (plist-get func :name))
            (docstring (plist-get func :docstring)))
        (setq result (concat result (format "- ~M-x %s~ - %s\n" name (or docstring "No description"))))))
    (concat result "\n")))

(defun enkan-repl-utils--generate-categorized-documentation (file-path header-level)
  "Generate categorized documentation from FILE-PATH with HEADER-LEVEL.
Returns org-mode formatted string with functions organized by category."
  (let ((categorized-functions (enkan-repl-utils--get-categorized-functions file-path))
        (result ""))
    (dolist (category-entry categorized-functions)
      (let ((category (car category-entry))
            (functions (cdr category-entry)))
        (setq result (concat result (enkan-repl-utils--generate-org-section category functions header-level)))))
    result))

;; Additional pure functions for session management tests

(defun enkan-repl--validate-session-selection (selection candidates)
  "Pure function to validate session selection against candidates.
SELECTION is the selected string.
CANDIDATES is a list of valid candidate strings.
Returns non-nil if selection is valid."
  (member selection candidates))

(defun enkan-repl--format-session-list (sessions)
  "Pure function to format session list for display.
SESSIONS is a list of plists with :name, :directory, :status.
Returns formatted string."
  (mapconcat
   (lambda (session)
     (let ((name (plist-get session :name))
           (status (plist-get session :status)))
       (format "%s [%s]"
               (if (string-match "\\*enkan:\\(.*?\\)\\*" name)
                   (match-string 1 name)
                 name)
               (upcase (symbol-name status)))))
   sessions
   "\n"))

;;;; Path encode/decode helpers (pure)

(defun enkan-repl-utils--encode-full-path (path prefix separator)
  "Encode PATH with PREFIX and SEPARATOR (pure).
Returns string like PREFIX + PATH with '/' replaced by SEPARATOR.
Example: PATH '/Users/proj', PREFIX 'enkan', SEP '--' -> 'enkan--Users--proj'"
  (let* ((expanded-path (expand-file-name path))
         (cleaned-path (if (string-suffix-p "/" expanded-path)
                           (substring expanded-path 0 -1)
                         expanded-path)))
    (concat prefix (replace-regexp-in-string "/" separator cleaned-path))))

(defun enkan-repl-utils--decode-full-path (encoded-name prefix separator)
  "Decode ENCODED-NAME using PREFIX and SEPARATOR (pure).
Returns normalized directory path ending with '/'.
Example: 'enkan--Users--proj' -> '/Users/proj/'"
  (when (string-prefix-p prefix encoded-name)
    (let ((path-part (substring encoded-name (length prefix))))
      (concat (replace-regexp-in-string (regexp-quote separator) "/" path-part) "/"))))

;;;; Project Configuration Pure Functions

(defun enkan-repl--get-project-info-from-directories (alias target-directories)
  "Get project info from directories for ALIAS in TARGET-DIRECTORIES.
Return (project-name . project-path) or nil if not found."
  (cdr (assoc alias target-directories)))

(defun enkan-repl--get-project-path-from-directories (project-name target-directories)
  "Pure function to get project path from directories by project name."
  (let ((project-info (cl-find-if (lambda (entry)
                                    (string= (car (cdr entry)) project-name))
                                  target-directories)))
    (when project-info
      (cdr (cdr project-info)))))

;;;; Session State Pure Functions

(defun enkan-repl--get-current-session-state-info (current-project session-list session-counter project-aliases)
  "Retrieve session state information as an alist.
CURRENT-PROJECT is the current project list.
SESSION-LIST is the list of sessions.
SESSION-COUNTER is the session counter value.
PROJECT-ALIASES is the list of project aliases.
This is a pure function."
  (list
   (cons 'current-project current-project)
   (cons 'session-list session-list)
   (cons 'session-counter session-counter)
   (cons 'project-aliases project-aliases)))

(defun enkan-repl--format-session-state-display (state-info &optional prefix)
  "Format session state information for display.
STATE-INFO is an alist from `enkan-repl--get-current-session-state-info`.
PREFIX is an optional string to prepend to each line.
This is a pure function."
  (let ((current-project (cdr (assoc 'current-project state-info)))
        (session-list (cdr (assoc 'session-list state-info)))
        (session-counter (cdr (assoc 'session-counter state-info)))
        (project-aliases (cdr (assoc 'project-aliases state-info)))
        (prefix-str (or prefix "")))
    (concat
     (format "%s  Layout (enkan-repl--current-project): %s\n" prefix-str (or current-project "nil"))
     (format "%s  Sessions (enkan-repl-session-list): %s\n" prefix-str session-list)
     (format "%s  Counter (enkan-repl--session-counter): %d\n" prefix-str session-counter)
     (when project-aliases
       (format "%s  Permanent aliases (enkan-repl-project-aliases): %s\n" prefix-str project-aliases)))))

;;;; Send Primitive Pure Functions

(defun enkan-repl--send-primitive (text special-key-type)
  "Pure function to prepare send content from TEXT and SPECIAL-KEY-TYPE.
TEXT: original text content
SPECIAL-KEY-TYPE: :enter, :escape, number 1-9, or nil
Returns plist with :content (string to send) and :action (action type)."
  (cond
   ((eq special-key-type :enter)
    (list :content "\r" :action 'key))
   ((eq special-key-type :escape)
    (list :content "\e" :action 'key))
   ((and (numberp special-key-type)
         (>= special-key-type 1)
         (<= special-key-type 9))
    (list :content (number-to-string special-key-type) :action 'number))
   ((null special-key-type)
    (list :content text :action 'text))
   (t
    (error "Invalid special-key-type: %s" special-key-type))))

;;;; macOS Notification Pure Functions

(defun enkan-repl--build-notification-command (message title)
  "Build osascript command for notification with MESSAGE and TITLE.
This is a pure function for testing."
  (format "display notification \"%s\" with title \"%s\""
          (replace-regexp-in-string "\"" "\\\"" message t t)
          (replace-regexp-in-string "\"" "\\\"" title t t)))

(defun enkan-repl--should-play-sound-p (sys-type enabled-p file-path)
  "Check if sound should be played based on SYS-TYPE, ENABLED-P, and FILE-PATH.
This is a pure function for testing."
  (and (eq sys-type 'darwin)
       enabled-p
       (stringp file-path)
       (not (string= "" file-path))))

(defun enkan-repl--should-show-notification-p (sys-type enabled-p)
  "Check if notification should be shown based on SYS-TYPE and ENABLED-P.
This is a pure function for testing."
  (and (eq sys-type 'darwin)
       enabled-p))

(provide 'enkan-repl-utils)

;;; enkan-repl-utils.el ends here

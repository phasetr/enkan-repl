;;; enkan-repl-transcript-test.el --- Tests for transcript reader -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure parsing/formatting helpers of `enkan-repl-transcript'.
;; I/O and display helpers require real files/tmux and are exercised manually.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((project-root (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

(require 'enkan-repl-transcript)

;;;; project-dir encoding

(ert-deftest test-enkan-repl--transcript-encode-project-dir ()
  "Every non-alphanumeric character becomes a hyphen; hyphens are kept."
  (should (string= "-Users-sekine-dev-self-lattice-system"
                   (enkan-repl--transcript-encode-project-dir
                    "/Users/sekine/dev/self/lattice-system")))
  ;; Dotted components collapse each dot to a hyphen (so /.emacs.d -> --emacs-d).
  (should (string= "-Users-sekine--emacs-d"
                   (enkan-repl--transcript-encode-project-dir
                    "/Users/sekine/.emacs.d"))))

;;;; role markers

(ert-deftest test-enkan-repl--transcript-role-marker ()
  (should (string= "▶" (enkan-repl--transcript-role-marker "user")))
  (should (string= "◀" (enkan-repl--transcript-role-marker "assistant")))
  (should (string= "•" (enkan-repl--transcript-role-marker "system"))))

;;;; content text extraction

(ert-deftest test-enkan-repl--transcript-claude-extract-text-string ()
  "A plain string content is returned as-is."
  (should (string= "hello"
                   (enkan-repl--transcript-claude-extract-text "hello"))))

(ert-deftest test-enkan-repl--transcript-claude-extract-text-blocks ()
  "Only text blocks contribute; thinking/tool blocks are dropped."
  (let ((content (list '((type . "thinking") (thinking . "hmm"))
                       '((type . "text") (text . "first"))
                       '((type . "tool_use") (name . "Bash"))
                       '((type . "text") (text . "second")))))
    (should (string= "firstsecond"
                     (enkan-repl--transcript-claude-extract-text content)))))

;;;; JSONL line parsing

(ert-deftest test-enkan-repl--transcript-claude-parse-line-user ()
  "A user line with string content yields a user turn."
  (let ((line "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"hi there\"}}"))
    (should (equal '(:role "user" :text "hi there")
                   (enkan-repl--transcript-claude-parse-line line)))))

(ert-deftest test-enkan-repl--transcript-claude-parse-line-assistant ()
  "An assistant line with text blocks yields an assistant turn."
  (let ((line "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"x\"},{\"type\":\"text\",\"text\":\"answer\"}]}}"))
    (should (equal '(:role "assistant" :text "answer")
                   (enkan-repl--transcript-claude-parse-line line)))))

(ert-deftest test-enkan-repl--transcript-claude-parse-line-skips ()
  "Blank, unparseable, non-conversation, sidechain, meta, and empty-text
lines are skipped."
  (should-not (enkan-repl--transcript-claude-parse-line ""))
  (should-not (enkan-repl--transcript-claude-parse-line "   "))
  (should-not (enkan-repl--transcript-claude-parse-line "{not json"))
  (should-not (enkan-repl--transcript-claude-parse-line
               "{\"type\":\"system\",\"message\":{\"role\":\"system\",\"content\":\"x\"}}"))
  (should-not (enkan-repl--transcript-claude-parse-line
               "{\"type\":\"user\",\"isSidechain\":true,\"message\":{\"role\":\"user\",\"content\":\"sub\"}}"))
  (should-not (enkan-repl--transcript-claude-parse-line
               "{\"type\":\"assistant\",\"isMeta\":true,\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"m\"}]}}"))
  (should-not (enkan-repl--transcript-claude-parse-line
               "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"only\"}]}}")))

;;;; formatting

(ert-deftest test-enkan-repl--transcript-claude-format-orders-and-marks ()
  "Formatting renders each turn with its role marker in order."
  (let ((lines (list
                "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"q1\"}}"
                "garbage"
                "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"a1\"}]}}")))
    (should (string= "▶ user\nq1\n\n◀ assistant\na1"
                     (enkan-repl--transcript-claude-format lines)))))

(ert-deftest test-enkan-repl--transcript-claude-format-max-turns ()
  "MAX-TURNS keeps only the most recent turns."
  (let ((lines (list
                "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"q1\"}}"
                "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"a1\"}]}}"
                "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"q2\"}}")))
    (should (string= "▶ user\nq2"
                     (enkan-repl--transcript-claude-format lines 1)))))

(provide 'enkan-repl-transcript-test)

;;; enkan-repl-transcript-test.el ends here

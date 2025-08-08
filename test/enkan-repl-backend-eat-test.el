;;; enkan-repl-backend-eat-test.el --- Tests for eat backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the eat backend implementation.

;;; Code:

(require 'ert)
(require 'enkan-repl-backend)
(require 'enkan-repl-backend-eat)

(ert-deftest enkan-repl-backend-eat-test-session-creation ()
  "Test creating a new eat session."
  (let ((enkan-repl-backend-type 'eat)
        (enkan-repl-sessions-alist nil)
        (test-dir "/tmp/test-enkan"))
    ;; Create directory if it doesn't exist
    (make-directory test-dir t)
    
    ;; Create session
    (let ((buffer (enkan-repl-backend-create-session test-dir)))
      (should (bufferp buffer))
      (should (buffer-live-p buffer))
      (should (string-match "\\*enkan-eat:" (buffer-name buffer)))
      
      ;; Check session is registered
      (should (= 1 (length enkan-repl-sessions-alist)))
      (let ((session (car enkan-repl-sessions-alist)))
        (should (string= (plist-get (cdr session) :backend) 'eat))
        (should (string= (plist-get (cdr session) :directory) test-dir)))
      
      ;; Clean up
      (kill-buffer buffer))))

(ert-deftest enkan-repl-backend-eat-test-message-routing ()
  "Test @ notation message routing."
  (let ((text1 "@app-1 test message")
        (text2 "@enkan-eat:/tmp/test full path message")
        (text3 "plain message"))
    
    ;; Test short name parsing
    (let ((parsed (enkan-repl-backend--parse-target text1)))
      (should (null (car parsed)))  ; No matching session yet
      (should (string= (cdr parsed) text1)))
    
    ;; Test full path parsing
    (let ((parsed (enkan-repl-backend--parse-target text2)))
      (should (null (car parsed)))  ; No matching session yet
      (should (string= (cdr parsed) text2)))
    
    ;; Test plain message
    (let ((parsed (enkan-repl-backend--parse-target text3)))
      (should (null (car parsed)))
      (should (string= (cdr parsed) text3)))))

(ert-deftest enkan-repl-backend-eat-test-short-name-extraction ()
  "Test short name extraction from paths."
  (should (string= "project" 
                   (enkan-repl-backend--extract-short-name "/Users/test/project")))
  (should (string= "app-1" 
                   (enkan-repl-backend--extract-short-name "/tmp/app-1/")))
  (should (string= "test" 
                   (enkan-repl-backend--extract-short-name "/test"))))

(ert-deftest enkan-repl-backend-eat-test-session-list-display ()
  "Test session list display format."
  (let ((enkan-repl-sessions-alist
         '(("eat:/tmp/app-1" . (:buffer nil :backend eat 
                                :directory "/tmp/app-1" 
                                :short-name "app-1" 
                                :created nil))
           ("eat:/tmp/proj-2" . (:buffer nil :backend eat 
                                 :directory "/tmp/proj-2" 
                                 :short-name "proj-2" 
                                 :created nil)))))
    (let ((display-list (enkan-repl-backend-list-sessions)))
      (should (= 2 (length display-list)))
      (should (member "app-1 (eat:/tmp/app-1)" display-list))
      (should (member "proj-2 (eat:/tmp/proj-2)" display-list)))))

(provide 'enkan-repl-backend-eat-test)
;;; enkan-repl-backend-eat-test.el ends here
;;; vterm-ai-resume-test.el --- Tests for vterm-ai-resume  -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -L . -l ert -l test/vterm-ai-resume-test.el -f ert-run-tests-batch-and-exit
;; Needs sqlite3 on PATH.

;;; Code:

(require 'ert)
(require 'vterm-ai)
(require 'vterm-ai-claude)
(require 'vterm-ai-codex)
(require 'vterm-ai-resume)

(defun vterm-ai-resume-test--history-line (sid cwd ms text)
  (json-encode `((display . ,text) (pastedContents . ,(make-hash-table))
                 (timestamp . ,ms) (project . ,cwd) (sessionId . ,sid))))

(defun vterm-ai-resume-test--transcript (dir cwd sid)
  "Create an (empty) transcript for SID under Claude config DIR."
  (let ((file (let ((vterm-ai-claude-config-dirs (list dir)))
                (vterm-ai-claude--transcript-path cwd sid))))
    (make-directory (file-name-directory file) t)
    (write-region "" nil file)))

(ert-deftest vterm-ai-resume-test-account-and-command ()
  (should (equal (vterm-ai-resume--account "~/.claude_redhat_vertex") "redhat_vertex"))
  (should (equal (vterm-ai-resume--account "~/.codex_personal/") "personal"))
  (should (equal (vterm-ai-resume--account "~/.claude") "default"))
  (let ((vterm-ai-resume-commands '(("~/.claude_personal" . "claude-personal")
                                    ("~/.codex_redhat/" . "codex-redhat"))))
    (should (equal (vterm-ai-resume--command-for
                    (expand-file-name "~/.claude_personal/"))
                   "claude-personal"))
    (should (equal (vterm-ai-resume--command-for "~/.codex_redhat") "codex-redhat"))
    (should-not (vterm-ai-resume--command-for "~/.claude_other"))
    (should (equal (vterm-ai-resume--command
                    '(:agent "claude" :dir "~/.claude_personal" :id "abc-1"))
                   "claude-personal --resume abc-1"))
    (should (equal (vterm-ai-resume--command
                    '(:agent "codex" :dir "~/.codex_redhat" :id "019c"))
                   "codex-redhat resume 019c"))))

(ert-deftest vterm-ai-resume-test-claude-sessions ()
  (let* ((dir (make-temp-file "claude-cfg" t))
         (now-ms (* 1000 (floor (float-time))))
         (old-ms (- now-ms (* 1000 86400 40))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "history.jsonl" dir)
            (insert (vterm-ai-resume-test--history-line "s1" "/w/a" (- now-ms 5000) "最初の依頼") "\n"
                    (vterm-ai-resume-test--history-line "s1" "/w/a" (- now-ms 4000) "二つ目の依頼") "\n"
                    (vterm-ai-resume-test--history-line "s1" "/w/a" (- now-ms 3000) "/exit") "\n"
                    (vterm-ai-resume-test--history-line "s2" "/w/b" (- now-ms 1000) "別の作業") "\n"
                    (vterm-ai-resume-test--history-line "s3" "/w/c" old-ms "古い作業") "\n"
                    (vterm-ai-resume-test--history-line "s4" "/w/d" (- now-ms 500) "履歴なし") "\n"
                    "not json\n"))
          (dolist (s '(("s1" . "/w/a") ("s2" . "/w/b") ("s3" . "/w/c")))
            (vterm-ai-resume-test--transcript dir (cdr s) (car s)))
          (let* ((since (- (float-time) (* 86400 30)))
                 (sessions (vterm-ai-resume--claude-sessions dir since))
                 (s1 (cl-find "s1" sessions :key (lambda (s) (plist-get s :id)) :test #'equal)))
            ;; s3 is too old; s4 has no transcript left to resume.
            (should (equal (sort (mapcar (lambda (s) (plist-get s :id)) sessions) #'string<)
                           '("s1" "s2")))
            ;; Label is the last prompt that is not a slash command; time is the latest.
            (should (equal (plist-get s1 :label) "二つ目の依頼"))
            (should (= (plist-get s1 :time) (/ (- now-ms 3000) 1000.0)))
            (should (equal (plist-get s1 :cwd) "/w/a"))))
      (delete-directory dir t))))

(ert-deftest vterm-ai-resume-test-codex-sessions-and-order ()
  (let* ((root (make-temp-file "resume" t))
         (home (expand-file-name ".codex_work" root))
         (claude (expand-file-name ".claude_work" root))
         (now (floor (float-time)))
         (db (expand-file-name "state_5.sqlite" home)))
    (unwind-protect
        (progn
          (make-directory home t)
          (should (zerop (call-process
                          "sqlite3" nil nil nil db
                          (concat "CREATE TABLE threads (id TEXT, cwd TEXT, updated_at INTEGER, name TEXT, title TEXT, source TEXT, archived INTEGER);"
                                  (format "INSERT INTO threads VALUES ('t-cli','/w/x',%d,NULL,'対話の作業','cli',0);" (- now 100))
                                  (format "INSERT INTO threads VALUES ('t-exec','/w/x',%d,NULL,'exec run','exec',0);" (- now 50))
                                  (format "INSERT INTO threads VALUES ('t-old','/w/x',%d,NULL,'old','cli',0);" (- now (* 86400 60)))))))
          (make-directory claude t)
          (with-temp-file (expand-file-name "history.jsonl" claude)
            (insert (vterm-ai-resume-test--history-line "c1" "/w/y" (* 1000 (- now 10)) "最新") "\n"))
          (vterm-ai-resume-test--transcript claude "/w/y" "c1")
          (let* ((vterm-ai-enabled-providers '(claude codex))
                 (vterm-ai-claude-config-dirs (list claude))
                 (vterm-ai-codex-homes (list home))
                 (vterm-ai-resume-commands `((,claude . "claude-work") (,home . "codex-work")))
                 (sessions (vterm-ai-resume--sessions)))
            ;; Newest first; only the cli thread within the period.
            (should (equal (mapcar (lambda (s) (plist-get s :id)) sessions) '("c1" "t-cli")))
            (should (equal (plist-get (cadr sessions) :label) "対話の作業"))
            ;; A directory with no resume command is not listed.
            (let ((vterm-ai-resume-commands `((,home . "codex-work"))))
              (should (equal (mapcar (lambda (s) (plist-get s :id)) (vterm-ai-resume--sessions))
                             '("t-cli"))))))
      (delete-directory root t))))

(ert-deftest vterm-ai-resume-test-running-session ()
  (let* ((buf (generate-new-buffer " *vt*"))
         (vterm-ai-dashboard--sessions
          (list (make-vterm-ai-session :type "claude" :session-id "s1" :vterm-buffer buf))))
    (unwind-protect
        (progn
          (should (eq (vterm-ai-resume--running-buffer '(:agent "claude" :id "s1")) buf))
          (should-not (vterm-ai-resume--running-buffer '(:agent "codex" :id "s1")))
          (should (string-match-p "(running)"
                                  (vterm-ai-resume--candidate
                                   (list :agent "claude" :id "s1" :dir "~/.claude_personal"
                                         :cwd "/w/a" :time (float-time) :label "x")))))
      (kill-buffer buf))))

(ert-deftest vterm-ai-resume-test-shorten-path-keeps-project ()
  (should (equal (vterm-ai-resume--shorten-path "/w/a" 28) "/w/a"))
  (let ((short (vterm-ai-resume--shorten-path "/Users/x/Dropbox/home/src/coding-agent" 20)))
    (should (<= (string-width short) 20))
    (should (string-prefix-p "…" short))
    (should (string-suffix-p "coding-agent" short))))

(provide 'vterm-ai-resume-test)
;;; vterm-ai-resume-test.el ends here

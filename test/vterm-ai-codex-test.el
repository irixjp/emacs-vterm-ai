;;; vterm-ai-codex-test.el --- Tests for the Codex provider  -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -L . -l ert -l test/vterm-ai-codex-test.el -f ert-run-tests-batch-and-exit
;; Needs sqlite3 on PATH.

;;; Code:

(require 'ert)
(require 'vterm-ai-codex)

(defun vterm-ai-codex-test--make-home (dir rows)
  "Create a Codex home in DIR whose state DB holds ROWS.
ROWS are (ID SOURCE CWD UPDATED TITLE).  The DB is left in WAL mode with
no -wal/-shm files, like a home whose last Codex exited cleanly."
  (make-directory dir t)
  (let ((db (expand-file-name "state_5.sqlite" dir))
        (sql (concat
              "PRAGMA journal_mode=WAL;"
              "CREATE TABLE threads (id TEXT, model TEXT, name TEXT, title TEXT,"
              " approval_mode TEXT, first_user_message TEXT, cwd TEXT,"
              " archived INTEGER, source TEXT, updated_at INTEGER, rollout_path TEXT);"
              (mapconcat
               (lambda (r)
                 (format "INSERT INTO threads VALUES ('%s','gpt-x',NULL,'%s','on-request','first','%s',0,'%s',%d,'/r/%s.jsonl');"
                         (nth 0 r) (nth 4 r)
                         (replace-regexp-in-string "'" "''" (nth 2 r))
                         (nth 1 r) (nth 3 r) (nth 0 r)))
               rows "")
              "PRAGMA wal_checkpoint(TRUNCATE);")))
    (should (zerop (call-process "sqlite3" nil nil nil db sql)))
    ;; macOS sqlite3 keeps the WAL files after exit; drop them once checkpointed.
    (dolist (suffix '("-wal" "-shm"))
      (when (file-exists-p (concat db suffix))
        (delete-file (concat db suffix))))
    dir))

(ert-deftest vterm-ai-codex-test-sqlite-uri ()
  (let* ((dir (make-temp-file "codex-uri" t))
         (db (expand-file-name "a b.sqlite" dir)))
    (unwind-protect
        (progn
          (should (equal (vterm-ai-codex--sqlite-uri db)
                         (concat "file:" (replace-regexp-in-string " " "%20" db)
                                 "?immutable=1")))
          (write-region "" nil (concat db "-wal"))
          (should (string-suffix-p "?mode=ro" (vterm-ai-codex--sqlite-uri db))))
      (delete-directory dir t))))

(ert-deftest vterm-ai-codex-test-query-thread-across-homes ()
  (let* ((root (make-temp-file "codex-homes" t))
         (cwd "/work/it's")
         (home-a (vterm-ai-codex-test--make-home
                  (expand-file-name "a" root)
                  `(("a-old" "cli" ,cwd 100 "old in a")
                    ("a-exec" "exec" ,cwd 900 "codex-request run"))))
         (home-b (vterm-ai-codex-test--make-home
                  (expand-file-name "b" root)
                  `(("b-new" "cli" ,cwd 500 "new in b")
                    ("b-other" "cli" "/elsewhere" 999 "other cwd"))))
         (vterm-ai-codex-homes (list home-a home-b)))
    (unwind-protect
        (let ((info (vterm-ai-codex--query-thread cwd)))
          ;; Newest cli thread across homes; the newer exec thread is ignored.
          (should (equal (alist-get 'session-id info) "b-new"))
          (should (equal (alist-get 'title info) "new in b"))
          (should (equal (alist-get 'home info) home-b))
          (should (equal (gethash "b-new" vterm-ai-codex--thread-homes) home-b))
          ;; Reading never left files beside a WAL-mode DB.
          (should-not (file-exists-p (expand-file-name "state_5.sqlite-shm" home-b)))
          (should-not (vterm-ai-codex--query-thread "/nowhere")))
      (delete-directory root t))))


(ert-deftest vterm-ai-codex-test-read-last-reply ()
  (let ((file (make-temp-file "rollout" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"type\":\"session_meta\",\"payload\":{\"cwd\":\"/w\"}}\n"
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_complete\",\"last_agent_message\":\"一回目\"}}\n"
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_complete\",\"last_agent_message\":\"  二回目の応答です。\\n次の行  \"}}\n"
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_started\"}}\n"))
          ;; The newest finished turn wins, trimmed; a later unfinished turn is ignored.
          (should (equal (vterm-ai-codex--read-last-reply file) "二回目の応答です。\n次の行"))
          ;; A tail cut mid-file still finds it.
          (let ((vterm-ai-codex--rollout-tail-bytes 200))
            (should (equal (vterm-ai-codex--read-last-reply file) "二回目の応答です。\n次の行")))
          (should-not (vterm-ai-codex--read-last-reply "/nonexistent.jsonl")))
      (delete-file file))))

(defun vterm-ai-codex-test--run-hook (home json)
  "Feed JSON to hooks/codex-activity.sh with CODEX_HOME=HOME."
  (with-temp-buffer
    (insert json)
    (let ((process-environment (cons (concat "CODEX_HOME=" home) process-environment)))
      (call-process-region (point-min) (point-max) "sh" nil nil nil
                           (expand-file-name "hooks/codex-activity.sh")))))

(ert-deftest vterm-ai-codex-test-hook-script ()
  (skip-unless (executable-find "jq"))
  (let* ((home (make-temp-file "codex-home" t))
         (file (expand-file-name "vterm-ai-activity/s1" home)))
    (unwind-protect
        (cl-flet ((summary ()
                    (nth 3 (vterm-ai-codex--read-activity-line file))))
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"s1\",\"hook_event_name\":\"PreToolUse\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"\\n  pnpm test\\nmore\"}}")
          (should (equal (summary) "Bash: pnpm test"))
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"s1\",\"hook_event_name\":\"PreToolUse\",\"tool_name\":\"apply_patch\",\"tool_input\":{\"command\":\"*** Begin Patch\\n*** Update File: a.ts\\n*** Add File: b.ts\\n*** End Patch\"}}")
          (should (equal (summary) "apply_patch: a.ts, b.ts"))
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"s1\",\"hook_event_name\":\"PermissionRequest\",\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"curl x\\ty\"}}")
          (should (equal (summary) "approve Bash: curl x y"))
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"s1\",\"hook_event_name\":\"Stop\",\"last_assistant_message\":\"ok\"}")
          (should (equal (car (vterm-ai-codex--read-activity-line file)) "Stop"))
          (should (equal (summary) ""))
          ;; A session id that could escape the directory is ignored.
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"../x\",\"hook_event_name\":\"PreToolUse\",\"tool_name\":\"Bash\"}")
          (should-not (file-exists-p (expand-file-name "x" home)))
          (vterm-ai-codex-test--run-hook
           home "{\"session_id\":\"s1\",\"hook_event_name\":\"SessionEnd\"}")
          (should-not (file-exists-p file)))
      (delete-directory home t))))

(ert-deftest vterm-ai-codex-test-activity-for-session ()
  (let* ((home (make-temp-file "codex-home" t))
         (dir (expand-file-name "vterm-ai-activity" home))
         (now (floor (float-time)))
         (vterm-ai-codex-homes (list home)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file (expand-file-name "a" dir)
            (insert (format "PreToolUse\t%d\t4242\tBash: make\n" now)))
          (with-temp-file (expand-file-name "b" dir)
            (insert (format "PreToolUse\t%d\t9999\tother session\n" now)))
          (let ((s (make-vterm-ai-session :type "codex" :pid 4242 :status "busy"
                                          :state-since (- now 5))))
            (should (equal (vterm-ai-codex-activity s) "Bash: make"))
            ;; A record older than the current state is from a previous turn.
            (setf (vterm-ai-session-state-since s) (+ now 10))
            (should-not (vterm-ai-codex-activity s))
            ;; No OSC state yet: nothing to compare against, so nothing shown.
            (setf (vterm-ai-session-state-since s) nil)
            (should-not (vterm-ai-codex-activity s))))
      (delete-directory home t))))

(ert-deftest vterm-ai-codex-test-apply-osc-uses-hook-activity ()
  (let* ((home (make-temp-file "codex-home" t))
         (dir (expand-file-name "vterm-ai-activity" home))
         (buf (generate-new-buffer " *osc*"))
         (vterm-ai-codex-homes (list home)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-current-buffer buf
            (vterm-ai-osc--apply-report '(:id nil :state "working" :app "codex")))
          (with-temp-file (expand-file-name "a" dir)
            (insert (format "PreToolUse\t%d\t77\tapply_patch: x.ts\n" (floor (float-time)))))
          (let ((s (make-vterm-ai-session :type "codex" :pid 77 :vterm-buffer buf)))
            (vterm-ai-data-apply-osc s)
            (should (equal (vterm-ai-session-status s) "busy"))
            (should (equal (vterm-ai-session-activity s) "apply_patch: x.ts"))
            ;; Once the turn is done the hook record is not shown.
            (with-current-buffer buf
              (vterm-ai-osc--apply-report '(:id nil :state "done" :app "codex")))
            (vterm-ai-data-apply-osc s)
            (should-not (vterm-ai-session-activity s))))
      (kill-buffer buf)
      (delete-directory home t))))

(provide 'vterm-ai-codex-test)
;;; vterm-ai-codex-test.el ends here

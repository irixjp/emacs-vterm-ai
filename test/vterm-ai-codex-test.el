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

(provide 'vterm-ai-codex-test)
;;; vterm-ai-codex-test.el ends here

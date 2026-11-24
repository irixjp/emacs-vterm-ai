;;; vterm-ai-dashboard-test.el --- Tests for vterm-ai-dashboard  -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -L . -l ert -l test/vterm-ai-dashboard-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'vterm-ai)

(defun vterm-ai-dashboard-test--session (since)
  (make-vterm-ai-session
   :type "claude" :pid 1 :status "busy" :title "task" :cwd "/tmp"
   :activity "Writing foo.el" :state-since since
   :children (list (list :id "c1" :state "working" :title "sub" :since since))))

(ert-deftest vterm-ai-dashboard-test-update-elapsed-rewrites-labels-only ()
  (with-temp-buffer
    (vterm-ai-dashboard-mode)
    (should (eq buffer-undo-list t))
    (let ((session (vterm-ai-dashboard-test--session (- (float-time) 5))))
      (vterm-ai-dashboard--render (list session))
      (let ((before (buffer-string)))
        ;; Nothing to change: the buffer is not modified.
        (set-buffer-modified-p nil)
        (vterm-ai-dashboard--update-elapsed)
        (should-not (buffer-modified-p))
        ;; Move the start time back; only the two labels change.
        (let ((inhibit-read-only t)
              (pos (point-min)))
          (while (setq pos (text-property-not-all pos (point-max) 'vterm-ai-since nil))
            (let ((end (next-single-property-change pos 'vterm-ai-since nil (point-max))))
              (put-text-property pos end 'vterm-ai-since (- (float-time) 125))
              (setq pos end))))
        (vterm-ai-dashboard--update-elapsed)
        (should (string-match-p "\\[BUSY 2:0[5-6]\\] claude: task" (buffer-string)))
        (should (string-match-p "└ \\[BUSY 2:0[5-6]\\] sub" (buffer-string)))
        (should (equal (replace-regexp-in-string "\\[BUSY [0-9:]+\\]" "[L]" before)
                       (replace-regexp-in-string "\\[BUSY [0-9:]+\\]" "[L]" (buffer-string))))
        ;; Rewritten labels keep their face and the card's session tag.
        (goto-char (point-min))
        (search-forward "[BUSY 2:")
        (should (eq (get-text-property (point) 'vterm-ai-session) session))
        (should (eq (get-text-property (point) 'face) 'vterm-ai-no-buffer))))))

(ert-deftest vterm-ai-dashboard-test-labels-without-time-are-static ()
  (with-temp-buffer
    (vterm-ai-dashboard-mode)
    (vterm-ai-dashboard--render
     (list (make-vterm-ai-session :type "claude" :pid 2 :status "idle" :cwd "/tmp")))
    (should-not (text-property-not-all (point-min) (point-max) 'vterm-ai-since nil))
    (should (string-match-p "^\\[IDLE\\] claude" (buffer-string)))))

(defun vterm-ai-dashboard-test--render (sessions &optional compact)
  "Render SESSIONS in a fresh dashboard buffer and return its text."
  (with-current-buffer (get-buffer-create " *vterm-ai-test*")
    (vterm-ai-dashboard-mode)
    (setq-local vterm-ai-dashboard-compact (if (eq compact 'full) nil t))
    (vterm-ai-dashboard--render sessions)
    (buffer-string)))

(ert-deftest vterm-ai-dashboard-test-compact-shapes ()
  (let* ((now (float-time))
         (asking (make-vterm-ai-session
                  :type "claude" :pid 1 :status "waiting" :title "ask" :cwd "/tmp/a"
                  :model "m" :mode "plan" :activity "approve Bash: rm x"
                  :last-prompt "please clean" :state-since now))
         (done (make-vterm-ai-session
                :type "claude" :pid 2 :status "done" :title "fin" :cwd "/tmp/d"
                :last-reply "## 結果\n```\ncode\n```\n全部終わりました。\n残りはありません。"
                :last-prompt "do it" :state-since now))
         (busy (make-vterm-ai-session
                :type "codex" :pid 3 :status "busy" :title "work" :cwd "/tmp/b"
                :activity "Writing foo.el" :last-prompt "p" :state-since now))
         (idle (make-vterm-ai-session
                :type "claude" :pid 4 :status "idle" :title "rest" :cwd "/tmp/i"
                :last-prompt "p" :state-since now))
         (text (vterm-ai-dashboard-test--render (list asking done busy idle))))
    ;; ASKING: details, mode kept on the status line, one-line prompt, no Model line.
    (should (string-match-p "\\[ASKING 0:0[0-9]\\] claude: ask {Plan}\nPath: /tmp/a\nNow: approve Bash: rm x\nPrompt: please clean\n" text))
    (should-not (string-match-p "^Model:" text))
    ;; DONE: Markdown scaffolding dropped from the reply.
    (should (string-match-p "\\[DONE 0:0[0-9]\\] claude: fin\nPath: /tmp/d\nReply: 結果\n       code\n       全部終わりました。 …\nPrompt: do it\n" text))
    ;; BUSY: what it is doing only.  IDLE: one line.
    (should (string-match-p "\\[BUSY 0:0[0-9]\\] codex: work\nNow: Writing foo.el\n─" text))
    (should (string-match-p "\\[IDLE 0:0[0-9]\\] claude: rest\n\\'" text))
    ;; Full view keeps the old card.
    (should (string-match-p "^Model: m$"
                            (vterm-ai-dashboard-test--render (list asking) 'full)))))

(ert-deftest vterm-ai-dashboard-test-quiet-idle-collapses ()
  (let* ((old (- (float-time) 4000))
         (quiet (make-vterm-ai-session :type "claude" :pid 1 :status "idle"
                                       :title "quiet" :state-since old))
         (unknown (make-vterm-ai-session :type "claude" :pid 2 :status "idle"
                                         :title "unknown-since"))
         (vterm-ai-dashboard-stale-after 1800))
    (with-current-buffer (get-buffer-create " *vterm-ai-test*")
      (vterm-ai-dashboard-mode)
      (setq vterm-ai-dashboard--sessions (list quiet unknown))
      (vterm-ai-dashboard--render vterm-ai-dashboard--sessions)
      (should-not (string-match-p "quiet\n" (buffer-string)))
      ;; Without a known start, a session is never treated as quiet.
      (should (string-match-p "unknown-since" (buffer-string)))
      (should (string-match-p "… 1 quiet idle hidden (30m\\+)  i:show" (buffer-string)))
      (vterm-ai-dashboard-toggle-quiet-idle)
      (should (string-match-p "\\[IDLE 1:06:[0-9][0-9]\\] claude: quiet" (buffer-string)))
      (should (string-match-p "i:hide" (buffer-string))))))

(ert-deftest vterm-ai-dashboard-test-tint-keeps-foreground ()
  (with-current-buffer (get-buffer-create " *vterm-ai-test*")
    (vterm-ai-dashboard-mode)
    (vterm-ai-dashboard--render
     (list (make-vterm-ai-session :type "claude" :pid 1 :status "done" :title "t"
                                  :cwd "/tmp" :vterm-buffer (current-buffer))))
    (goto-char (point-min))
    (search-forward "[DONE")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (memq 'vterm-ai-status-done (ensure-list face)))
      (should (memq 'vterm-ai-card-done (ensure-list face)))
      ;; The status face comes first, so its foreground wins.
      (should (eq (car (ensure-list face)) 'vterm-ai-status-done)))))

(ert-deftest vterm-ai-dashboard-test-claude-last-reply ()
  (require 'vterm-ai-claude)
  (let* ((lines '("{\"type\":\"assistant\",\"message\":{\"model\":\"m1\",\"stop_reason\":\"end_turn\",\"content\":[{\"type\":\"text\",\"text\":\"古い応答\"}]}}"
                  "{\"type\":\"user\",\"message\":{\"content\":\"次の依頼\"}}"
                  "{\"type\":\"assistant\",\"message\":{\"model\":\"m2\",\"stop_reason\":\"tool_use\",\"content\":[{\"type\":\"tool_use\",\"name\":\"Bash\"}]}}"
                  "{\"type\":\"assistant\",\"message\":{\"model\":\"m2\",\"stop_reason\":\"end_turn\",\"content\":[{\"type\":\"text\",\"text\":\"新しい応答\"},{\"type\":\"text\",\"text\":\"二段落目\"}]}}"
                  "{\"type\":\"system\",\"subtype\":\"turn_duration\"}"))
         (entries (mapcar (lambda (l) (json-parse-string l :object-type 'alist)) lines))
         (summary (vterm-ai-claude--extract-summary entries)))
    (should (equal (alist-get 'last-reply summary) "新しい応答\n二段落目"))
    (should (equal (alist-get 'model summary) "m2"))
    (should (equal (alist-get 'last-prompt summary) "次の依頼"))))

(ert-deftest vterm-ai-dashboard-test-claude-reply-behind-large-entry ()
  ;; Claude writes a large attachment entry right after the final reply; the
  ;; title sits after it, inside the first tail window.
  (require 'vterm-ai-claude)
  (let ((file (make-temp-file "transcript" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"type\":\"assistant\",\"message\":{\"model\":\"m\",\"stop_reason\":\"end_turn\",\"content\":[{\"type\":\"text\",\"text\":\"最終の応答\"}]}}\n"
                    "{\"type\":\"attachment\",\"blob\":\"" (make-string 70000 ?x) "\"}\n"
                    "{\"type\":\"ai-title\",\"aiTitle\":\"題\"}\n"))
          (let ((summary (vterm-ai-claude--extract-summary
                          (vterm-ai-claude--read-tail-expanding file))))
            (should (equal (alist-get 'title summary) "題"))
            (should (equal (alist-get 'last-reply summary) "最終の応答"))))
      (delete-file file))))

(provide 'vterm-ai-dashboard-test)
;;; vterm-ai-dashboard-test.el ends here

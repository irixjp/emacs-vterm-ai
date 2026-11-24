;;; vterm-ai-osc-test.el --- Tests for vterm-ai-osc  -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -L . -l ert -l test/vterm-ai-osc-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'vterm-ai-osc)
(require 'vterm-ai-data)

(defun vterm-ai-osc-test--b64 (text)
  (base64-encode-string (encode-coding-string text 'utf-8) t))

(defun vterm-ai-osc-test--seq (body &optional bel)
  (concat "\e]7501;" body (if bel "\a" "\e\\")))

;;; --- Parsing ---

(ert-deftest vterm-ai-osc-test-parse-query ()
  (should (equal (vterm-ai-osc-parse-body "?") '(query)))
  (should (equal (vterm-ai-osc-parse-body " ? ") '(query))))

(ert-deftest vterm-ai-osc-test-parse-root-report ()
  (let ((r (cdr (vterm-ai-osc-parse-body
                 (concat "state=blocked:app=claude-code:kind=question:msg="
                         (vterm-ai-osc-test--b64 "answer: 好きな色? (赤 · 青)"))))))
    (should (equal (plist-get r :state) "blocked"))
    (should (null (plist-get r :id)))
    (should (equal (plist-get r :kind) "question"))
    (should (equal (plist-get r :app) "claude-code"))
    (should (equal (plist-get r :msg) "answer: 好きな色? (赤 · 青)"))))

(ert-deftest vterm-ai-osc-test-parse-child-report ()
  (let ((r (cdr (vterm-ai-osc-parse-body
                 (concat "state=working:app=claude-code:id=abf5901aadf5ca193:title="
                         (vterm-ai-osc-test--b64 "作業ディレクトリの ls"))))))
    (should (equal (plist-get r :id) "abf5901aadf5ca193"))
    (should (equal (plist-get r :title) "作業ディレクトリの ls"))))

(ert-deftest vterm-ai-osc-test-parse-unpadded-base64 ()
  (let ((enc (replace-regexp-in-string "=+\\'" "" (vterm-ai-osc-test--b64 "ab"))))
    (should (equal (plist-get (cdr (vterm-ai-osc-parse-body (concat "state=working:msg=" enc)))
                          :msg)
                   "ab"))))

(ert-deftest vterm-ai-osc-test-parse-rejects ()
  ;; No state, unknown state, malformed id, control characters in msg.
  (should-not (vterm-ai-osc-parse-body "app=codex"))
  (should-not (vterm-ai-osc-parse-body "state=sleeping"))
  (should-not (vterm-ai-osc-parse-body "state=working:id=bad id"))
  (should-not (vterm-ai-osc-parse-body "state=working:id=a//b"))
  (should-not (vterm-ai-osc-parse-body
               (concat "state=working:msg=" (vterm-ai-osc-test--b64 "a\nb"))))
  (should-not (vterm-ai-osc-parse-body "state=working:msg=a"))
  (should-not (vterm-ai-osc-parse-body (concat "state=idle:x=" (make-string 4100 ?a)))))

(ert-deftest vterm-ai-osc-test-parse-skips-malformed-pairs ()
  (let ((r (cdr (vterm-ai-osc-parse-body "state=idle:Bad=1:app=co dex:noequals"))))
    (should (equal (plist-get r :state) "idle"))
    (should (null (plist-get r :app)))))

(ert-deftest vterm-ai-osc-test-parse-kind-and-progress-scope ()
  (let ((working (cdr (vterm-ai-osc-parse-body "state=working:kind=question:progress=042")))
        (idle (cdr (vterm-ai-osc-parse-body "state=idle:progress=10")))
        (over (cdr (vterm-ai-osc-parse-body "state=working:progress=101"))))
    (should (null (plist-get working :kind)))
    (should (equal (plist-get working :progress) 42))
    (should (null (plist-get idle :progress)))
    (should (null (plist-get over :progress)))))

(ert-deftest vterm-ai-osc-test-display-text ()
  (should (equal (vterm-ai-osc-display-text "  a‮b  ") "ab"))
  (should (null (vterm-ai-osc-display-text "​")))
  (let ((vterm-ai-osc-text-max-length 5))
    (should (equal (vterm-ai-osc-display-text "abcdefg") "abcd…"))))

;;; --- Stream scanning ---

(defun vterm-ai-osc-test--feed (chunks)
  "Feed CHUNKS through the advice into a fresh buffer.
Return (BUFFER REPLIES CHANGES)."
  (let ((buf (generate-new-buffer " *osc-test*"))
        (replies 0)
        (changes 0)
        (vterm-ai-osc-mode t))
    (let ((vterm-ai-osc-change-functions (list (lambda (_b) (cl-incf changes)))))
      (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf))
                ((symbol-function 'process-send-string)
                 (lambda (_p s)
                   (should (equal s vterm-ai-osc--query-reply))
                   (cl-incf replies))))
        (dolist (c chunks)
          (vterm-ai-osc--scan 'fake-process c))))
    (list buf replies changes)))

(defconst vterm-ai-osc-test--stream
  (concat "\e[?25l" (vterm-ai-osc-test--seq "?") "\e[c"
          "hello \e[31mred\e[0m"
          (vterm-ai-osc-test--seq "state=idle:app=claude-code")
          "more text"
          (vterm-ai-osc-test--seq
           (concat "state=working:app=claude-code:msg="
                   (vterm-ai-osc-test--b64 "Writing foo.el"))
           t)
          (vterm-ai-osc-test--seq
           (concat "state=working:app=claude-code:id=c1:title="
                   (vterm-ai-osc-test--b64 "sub task")))
          "\e[1;1H tail")
  "Output holding a probe, two root reports, and a child report.")

(defun vterm-ai-osc-test--check-final (buf)
  (let ((root (vterm-ai-osc-state buf))
        (children (vterm-ai-osc-children buf)))
    (should (equal (plist-get root :state) "working"))
    (should (equal (plist-get root :msg) "Writing foo.el"))
    (should (equal (plist-get root :app) "claude-code"))
    (should (= (length children) 1))
    (should (equal (plist-get (car children) :id) "c1"))
    (should (equal (plist-get (car children) :title) "sub task"))
    (should (null (buffer-local-value 'vterm-ai-osc--pending buf)))))

(ert-deftest vterm-ai-osc-test-scan-whole ()
  (pcase-let ((`(,buf ,replies ,changes)
               (vterm-ai-osc-test--feed (list vterm-ai-osc-test--stream))))
    (unwind-protect
        (progn
          (should (= replies 1))
          (should (= changes 1))
          (vterm-ai-osc-test--check-final buf))
      (kill-buffer buf))))

(ert-deftest vterm-ai-osc-test-scan-every-split ()
  ;; Splitting the stream anywhere must give the same result.
  (let ((s vterm-ai-osc-test--stream))
    (dotimes (i (1+ (length s)))
      (pcase-let ((`(,buf ,replies ,_)
                   (vterm-ai-osc-test--feed (list (substring s 0 i) (substring s i)))))
        (unwind-protect
            (progn
              (should (= replies 1))
              (vterm-ai-osc-test--check-final buf))
          (kill-buffer buf))))))

(ert-deftest vterm-ai-osc-test-scan-byte-by-byte ()
  (pcase-let ((`(,buf ,replies ,_)
               (vterm-ai-osc-test--feed
                (mapcar #'char-to-string vterm-ai-osc-test--stream))))
    (unwind-protect
        (progn
          (should (= replies 1))
          (vterm-ai-osc-test--check-final buf))
      (kill-buffer buf))))

(ert-deftest vterm-ai-osc-test-scan-no-reply-when-mode-off ()
  (let ((buf (generate-new-buffer " *osc-test*"))
        (vterm-ai-osc-mode nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'process-send-string)
                   (lambda (&rest _) (error "Must not reply"))))
          (vterm-ai-osc--scan 'fake-process (vterm-ai-osc-test--seq "?")))
      (kill-buffer buf))))

(ert-deftest vterm-ai-osc-test-overlong-open-body-is-dropped ()
  (pcase-let ((`(,buf ,_ ,_)
               (vterm-ai-osc-test--feed
                (list (concat "\e]7501;state=idle:x=" (make-string 5000 ?a))))))
    (unwind-protect
        (should (null (buffer-local-value 'vterm-ai-osc--pending buf)))
      (kill-buffer buf))))

;;; --- Records ---

(defun vterm-ai-osc-test--report (body)
  (vterm-ai-osc--apply-report (cdr (vterm-ai-osc-parse-body body))))

(ert-deftest vterm-ai-osc-test-records ()
  (with-temp-buffer
    (should (vterm-ai-osc-test--report "state=working:app=codex"))
    (let ((since (plist-get vterm-ai-osc--root :since)))
      ;; Same display: no change, :since kept.
      (should-not (vterm-ai-osc-test--report "state=working:app=codex"))
      (should (equal (plist-get vterm-ai-osc--root :since) since))
      ;; :app is kept when a later report omits it.
      (vterm-ai-osc-test--report "state=done")
      (should (equal (plist-get vterm-ai-osc--root :app) "codex")))
    (vterm-ai-osc-test--report "state=working:id=c1")
    (vterm-ai-osc-test--report "state=working:id=c2")
    (should (vterm-ai-osc-test--report "state=clear:id=c1"))
    (should (equal (mapcar (lambda (c) (plist-get c :id))
                           (vterm-ai-osc-children (current-buffer)))
                   '("c2")))
    ;; idle (start or interrupt) drops children it left behind.
    (should (vterm-ai-osc-test--report "state=idle:app=codex"))
    (should (null vterm-ai-osc--children))
    (vterm-ai-osc-test--report "state=working:id=c3")
    ;; Root clear drops everything.
    (should (vterm-ai-osc-test--report "state=clear"))
    (should (null vterm-ai-osc--root))
    (should (null vterm-ai-osc--children))))

;;; --- Overlay onto sessions ---

(ert-deftest vterm-ai-osc-test-apply-osc ()
  (let ((buf (generate-new-buffer " *osc-test*")))
    (unwind-protect
        (let ((claude (make-vterm-ai-session :type "claude" :status "running"
                                             :vterm-buffer buf))
              (codex (make-vterm-ai-session :type "codex" :status "idle"
                                            :status-source "title" :vterm-buffer buf)))
          (with-current-buffer buf
            (vterm-ai-osc-test--report
             (concat "state=blocked:app=claude-code:kind=permission:msg="
                     (vterm-ai-osc-test--b64 "approve Bash: ls"))))
          ;; App matches: overlay.
          (vterm-ai-data-apply-osc claude)
          (should (equal (vterm-ai-session-status claude) "waiting"))
          (should (equal (vterm-ai-session-blocked-kind claude) "permission"))
          (should (equal (vterm-ai-session-activity claude) "approve Bash: ls"))
          (should (equal (vterm-ai-session-status-source claude) "osc"))
          ;; App mismatch: untouched.
          (vterm-ai-data-apply-osc codex)
          (should (equal (vterm-ai-session-status codex) "idle"))
          (should (equal (vterm-ai-session-status-source codex) "title"))
          ;; Record cleared: an OSC-sourced session falls back to running.
          (with-current-buffer buf (vterm-ai-osc-test--report "state=clear"))
          (vterm-ai-data-apply-osc claude)
          (should (equal (vterm-ai-session-status claude) "running"))
          (should (null (vterm-ai-session-activity claude)))
          (should (null (vterm-ai-session-status-source claude))))
      (kill-buffer buf))))

(ert-deftest vterm-ai-osc-test-status-map ()
  (should (equal (mapcar #'vterm-ai-data-osc-status
                         '("working" "blocked" "done" "idle" "error" "weird"))
                 '("busy" "waiting" "done" "idle" "error" "running"))))

(provide 'vterm-ai-osc-test)
;;; vterm-ai-osc-test.el ends here

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

(provide 'vterm-ai-dashboard-test)
;;; vterm-ai-dashboard-test.el ends here

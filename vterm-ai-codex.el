;;; vterm-ai-codex.el --- Codex provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Codex provider: async process detection via ps/lsof
;; and SQLite-based session info retrieval.

;;; Code:

(require 'vterm-ai-data)

(defvar vterm-ai-codex--db-path
  (expand-file-name "state_5.sqlite"
                    (expand-file-name ".codex" (getenv "HOME"))))

;;; --- Async session discovery ---

(defun vterm-ai-codex--get-sessions-async (callback)
  "Detect running Codex processes asynchronously.
Call CALLBACK with a list of alists containing pid and cwd.
Return the process object for cancellation management."
  (let ((buf (generate-new-buffer " *vterm-ai-codex*")))
    (make-process
     :name "vterm-ai-codex-detect"
     :buffer buf
     :command '("sh" "-c"
                "ps -eo pid,comm | grep '/codex$' | awk '{print $1}' | while read pid; do cwd=$(lsof -a -d cwd -p \"$pid\" -F n 2>/dev/null | grep '^n/' | sed 's/^n//'); [ -n \"$cwd\" ] && echo \"$pid $cwd\"; done")
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (funcall callback
                      (when (and (zerop (process-exit-status proc))
                                 (buffer-live-p (process-buffer proc)))
                        (with-current-buffer (process-buffer proc)
                          (goto-char (point-min))
                          (let (results)
                            (while (not (eobp))
                              (when (looking-at "\\([0-9]+\\) \\(.+\\)")
                                (push `((pid . ,(string-to-number (match-string 1)))
                                        (cwd . ,(match-string 2))
                                        (status . "running"))
                                      results))
                              (forward-line 1))
                            (nreverse results)))))
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

;;; --- Enrich via SQLite ---

(defvar vterm-ai-codex--enrich-cache (make-hash-table :test 'equal)
  "Cache keyed by cwd.  Values: (mtime title model prompt).")

(defun vterm-ai-codex--query-thread (cwd)
  "Query the Codex SQLite database for the most recent thread matching CWD.
Return an alist with title, model, and first-user-message, or nil."
  (when (file-readable-p vterm-ai-codex--db-path)
    (with-temp-buffer
      (let ((ret (call-process
                  "sqlite3" nil t nil
                  "-separator" "\t"
                  vterm-ai-codex--db-path
                  (format "SELECT id, title, model, substr(first_user_message, 1, 500) FROM threads WHERE cwd = '%s' AND archived = 0 ORDER BY updated_at DESC LIMIT 1;"
                          (replace-regexp-in-string "'" "''" cwd)))))
        (when (and (zerop ret) (> (buffer-size) 0))
          (goto-char (point-min))
          (let ((parts (split-string (buffer-substring-no-properties
                                      (point) (line-end-position))
                                     "\t")))
            (when (>= (length parts) 4)
              `((session-id . ,(nth 0 parts))
                (title . ,(nth 1 parts))
                (model . ,(nth 2 parts))
                (prompt . ,(nth 3 parts))))))))))

(defun vterm-ai-codex-enrich (session)
  "Enrich SESSION with title, model, and last-prompt from Codex SQLite."
  (when (file-readable-p vterm-ai-codex--db-path)
    (let* ((cwd (vterm-ai-session-cwd session))
           (mtime (file-attribute-modification-time
                   (file-attributes vterm-ai-codex--db-path)))
           (cached (gethash cwd vterm-ai-codex--enrich-cache)))
      (if (and cached (equal (car cached) mtime))
          (let ((data (cdr cached)))
            (setf (vterm-ai-session-session-id session) (nth 0 data))
            (setf (vterm-ai-session-title session) (nth 1 data))
            (setf (vterm-ai-session-model session) (nth 2 data))
            (setf (vterm-ai-session-last-prompt session) (nth 3 data)))
        (let ((info (vterm-ai-codex--query-thread cwd)))
          (when info
            (let ((sid (alist-get 'session-id info))
                  (title (or (alist-get 'title info) ""))
                  (model (or (alist-get 'model info) ""))
                  (prompt (or (alist-get 'prompt info) "")))
              (setf (vterm-ai-session-session-id session) sid)
              (setf (vterm-ai-session-title session) title)
              (setf (vterm-ai-session-model session) model)
              (setf (vterm-ai-session-last-prompt session) prompt)
              (puthash cwd (cons mtime (list sid title model prompt))
                       vterm-ai-codex--enrich-cache))))))))

;;; --- Detail ---

(defun vterm-ai-codex-detail (session)
  "Return a detailed string for SESSION."
  (format "Session: %s\nStatus:  %s\nCWD:     %s\nTitle:   %s\nModel:   %s\nPID:     %d\n\n--- Last Prompt ---\n\n%s\n"
          (or (vterm-ai-session-name session) "codex")
          (or (vterm-ai-session-status session) "unknown")
          (or (vterm-ai-session-cwd session) "N/A")
          (or (vterm-ai-session-title session) "N/A")
          (or (vterm-ai-session-model session) "N/A")
          (vterm-ai-session-pid session)
          (or (vterm-ai-session-last-prompt session) "(none)")))

;;; --- Auto-register ---

(vterm-ai-register-provider
 '(:name "codex"
   :get-sessions-async vterm-ai-codex--get-sessions-async
   :enrich vterm-ai-codex-enrich
   :detail vterm-ai-codex-detail))

(provide 'vterm-ai-codex)
;;; vterm-ai-codex.el ends here

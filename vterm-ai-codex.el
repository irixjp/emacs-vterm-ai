;;; vterm-ai-codex.el --- Codex provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Codex provider: async process detection via ps/lsof
;; and SQLite-based session info retrieval.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vterm-ai-data)

(defvar vterm-ai-codex--config-dir
  (expand-file-name ".codex" (getenv "HOME")))

(defvar vterm-ai-codex--db-path
  (expand-file-name "state_5.sqlite" vterm-ai-codex--config-dir))

(defvar vterm-ai-codex--history-db-path
  (expand-file-name "thread_history_1.sqlite" vterm-ai-codex--config-dir))

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

;;; --- Status from vterm buffer name ---

(defun vterm-ai-codex--parse-buffer-name (name)
  "Parse vterm buffer NAME for Codex status and project name.
Return (status . project) or nil."
  (let ((stripped (if (string-match "\\`vterm: \\(.+\\)" name)
                      (match-string 1 name)
                    name)))
    (cond
     ((string-match "Action Required | .+ | \\(.+\\)" stripped)
      (cons "waiting" (string-trim (match-string 1 stripped))))
     ((string-match "Working | \\(.+\\)" stripped)
      (cons "busy" (string-trim (match-string 1 stripped))))
     ((string-match "Ready | \\(.+\\)" stripped)
      (cons "idle" (string-trim (match-string 1 stripped))))
     (t nil))))

;;; --- Approval mode mapping ---

(defun vterm-ai-codex--map-approval-mode (mode)
  "Map Codex approval MODE to dashboard mode string."
  (cond
   ((equal mode "never") "plan")
   ((equal mode "untrusted") "acceptEdits")
   ((equal mode "on-request") "default")
   (t (or mode ""))))

;;; --- Thread query ---

(defvar vterm-ai-codex--enrich-cache (make-hash-table :test 'equal)
  "Cache keyed by cwd.  Values: (mtime session-id title model mode last-prompt).")

(defun vterm-ai-codex--query-thread (cwd)
  "Query the Codex SQLite database for the most recent thread matching CWD.
Return an alist with session-id, title, model, mode, and first-user-message."
  (when (file-readable-p vterm-ai-codex--db-path)
    (with-temp-buffer
      (let ((ret (call-process
                  "sqlite3" nil t nil
                  "-separator" "\t"
                  vterm-ai-codex--db-path
                  (format "SELECT id, model, COALESCE(name, title), approval_mode, first_user_message FROM threads WHERE cwd = '%s' AND archived = 0 ORDER BY updated_at DESC LIMIT 1;"
                          (replace-regexp-in-string "'" "''" cwd)))))
        (when (and (zerop ret) (> (buffer-size) 0))
          (goto-char (point-min))
          (let ((parts (split-string (buffer-substring-no-properties
                                      (point) (line-end-position))
                                     "\t")))
            (when (>= (length parts) 5)
              `((session-id . ,(nth 0 parts))
                (model . ,(nth 1 parts))
                (title . ,(nth 2 parts))
                (mode . ,(vterm-ai-codex--map-approval-mode (nth 3 parts)))
                (first-user-message . ,(nth 4 parts))))))))))

;;; --- Last prompt from thread history ---

(defun vterm-ai-codex--query-last-prompt (thread-id)
  "Query the last user message for THREAD-ID from thread history.
Return the message text, or nil."
  (when (and thread-id (file-readable-p vterm-ai-codex--history-db-path))
    (with-temp-buffer
      (let ((ret (call-process
                  "sqlite3" nil t nil
                  vterm-ai-codex--history-db-path
                  (format "SELECT json_extract(item_json, '$.content[0].text') FROM thread_items WHERE thread_id = '%s' AND item_type = 'userMessage' ORDER BY rollout_ordinal DESC LIMIT 1;"
                          (replace-regexp-in-string "'" "''" thread-id)))))
        (when (and (zerop ret) (> (buffer-size) 0))
          (string-trim (buffer-string)))))))

(defun vterm-ai-codex--newest-mtime ()
  "Return the newest mtime among Codex database files."
  (let (newest)
    (dolist (path (list vterm-ai-codex--db-path
                        vterm-ai-codex--history-db-path))
      (dolist (suffix '("" "-wal"))
        (let ((f (concat path suffix)))
          (when (file-exists-p f)
            (let ((mt (file-attribute-modification-time (file-attributes f))))
              (when (or (not newest) (time-less-p newest mt))
                (setq newest mt)))))))
    newest))

(defun vterm-ai-codex-enrich (session)
  "Enrich SESSION with status/title from buffer name and data from SQLite."
  (let ((buf (vterm-ai-session-vterm-buffer session)))
    (when (and buf (buffer-live-p buf))
      (let ((parsed (vterm-ai-codex--parse-buffer-name (buffer-name buf))))
        (when parsed
          (setf (vterm-ai-session-status session) (car parsed))
          (setf (vterm-ai-session-title session) (cdr parsed))))))
  (let* ((cwd (vterm-ai-session-cwd session))
         (mtime (vterm-ai-codex--newest-mtime))
         (cached (gethash cwd vterm-ai-codex--enrich-cache)))
    (if (and cached mtime (equal (car cached) mtime))
        (let ((data (cdr cached)))
          (setf (vterm-ai-session-session-id session) (nth 0 data))
          (setf (vterm-ai-session-title session) (nth 1 data))
          (setf (vterm-ai-session-model session) (nth 2 data))
          (setf (vterm-ai-session-mode session) (nth 3 data))
          (setf (vterm-ai-session-last-prompt session) (nth 4 data)))
      (let ((info (vterm-ai-codex--query-thread cwd)))
        (if info
            (let* ((sid (alist-get 'session-id info))
                   (title (or (alist-get 'title info) ""))
                   (model (or (alist-get 'model info) ""))
                   (mode (or (alist-get 'mode info) ""))
                   (last-prompt (or (vterm-ai-codex--query-last-prompt sid)
                                    (alist-get 'first-user-message info)
                                    "")))
              (setf (vterm-ai-session-session-id session) sid)
              (setf (vterm-ai-session-title session) title)
              (setf (vterm-ai-session-model session) model)
              (setf (vterm-ai-session-mode session) mode)
              (setf (vterm-ai-session-last-prompt session) last-prompt)
              (puthash cwd (cons mtime (list sid title model mode last-prompt))
                       vterm-ai-codex--enrich-cache))
          (setf (vterm-ai-session-last-prompt session) "(not available)"))))))

;;; --- Recent prompts for detail view ---

(defun vterm-ai-codex--query-recent-prompts (thread-id &optional limit)
  "Query recent user messages for THREAD-ID from thread history.
Return a list of message strings, most recent first.  LIMIT defaults to 10."
  (when (and thread-id (file-readable-p vterm-ai-codex--history-db-path))
    (with-temp-buffer
      (let ((ret (call-process
                  "sqlite3" nil t nil
                  vterm-ai-codex--history-db-path
                  (format "SELECT json_extract(item_json, '$.content[0].text') FROM thread_items WHERE thread_id = '%s' AND item_type = 'userMessage' ORDER BY rollout_ordinal DESC LIMIT %d;"
                          (replace-regexp-in-string "'" "''" thread-id)
                          (or limit 10)))))
        (when (and (zerop ret) (> (buffer-size) 0))
          (split-string (string-trim (buffer-string)) "\n" t))))))

;;; --- Detail ---

(defun vterm-ai-codex-detail (session)
  "Return a detailed string for SESSION with recent prompts."
  (let* ((sid (vterm-ai-session-session-id session))
         (prompts (when sid (vterm-ai-codex--query-recent-prompts sid))))
    (with-temp-buffer
      (insert (format "Session: %s\n" (or (vterm-ai-session-name session) "codex")))
      (insert (format "Status:  %s\n" (or (vterm-ai-session-status session) "unknown")))
      (insert (format "CWD:     %s\n" (or (vterm-ai-session-cwd session) "N/A")))
      (insert (format "Title:   %s\n" (or (vterm-ai-session-title session) "N/A")))
      (insert (format "Model:   %s\n" (or (vterm-ai-session-model session) "N/A")))
      (insert (format "Mode:    %s\n" (or (vterm-ai-session-mode session) "N/A")))
      (insert (format "PID:     %d\n" (vterm-ai-session-pid session)))
      (insert "\n--- Recent Prompts ---\n\n")
      (if prompts
          (dolist (p (nreverse prompts))
            (insert (format ">> %s\n\n"
                            (truncate-string-to-width p 200 nil nil "..."))))
        (insert "(no prompts found)\n"))
      (buffer-string))))

;;; --- Auto-register ---

(vterm-ai-register-provider
 '(:name "codex"
   :get-sessions-async vterm-ai-codex--get-sessions-async
   :enrich vterm-ai-codex-enrich
   :detail vterm-ai-codex-detail))

(provide 'vterm-ai-codex)
;;; vterm-ai-codex.el ends here

;;; vterm-ai-cursor.el --- Cursor provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Cursor provider: async process detection via ps/lsof,
;; status extraction from vterm buffer names, and session enrichment
;; from Cursor's data files (~/.cursor/chats/).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vterm-ai-data)

(defvar vterm-ai-cursor--config-dir
  (expand-file-name ".cursor" (getenv "HOME")))

;;; --- Async session discovery ---

(defun vterm-ai-cursor--get-sessions-async (callback)
  "Detect running Cursor agent processes asynchronously.
Call CALLBACK with a list of alists containing pid and cwd.
Return the process object for cancellation management."
  (let ((buf (generate-new-buffer " *vterm-ai-cursor*")))
    (make-process
     :name "vterm-ai-cursor-detect"
     :buffer buf
     :command '("sh" "-c"
                "ps -eo pid,command | grep 'cursor-agent' | grep -v 'worker-server' | grep -v grep | awk '{print $1}' | while read pid; do cwd=$(lsof -a -d cwd -p \"$pid\" -Fn 2>/dev/null | grep '^n/' | sed 's/^n//'); [ -n \"$cwd\" ] && echo \"$pid $cwd\"; done")
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

;;; --- Status and title from vterm buffer name ---

(defun vterm-ai-cursor--parse-buffer-name (name)
  "Parse vterm buffer NAME for Cursor status and title.
Return (status . title) or nil if not a Cursor status-indicator name."
  (cond
   ((string-match "\\(.+\\) - .+Waiting" name)
    (cons "waiting" (string-trim (match-string 1 name))))
   ((string-match "\\(.+\\) - .+Working" name)
    (cons "busy" (string-trim (match-string 1 name))))
   ((string-match "\\(.+\\) - .+Ready" name)
    (cons "idle" (string-trim (match-string 1 name))))
   (t nil)))

;;; --- Chat index: CWD -> latest chat mapping ---

(defvar vterm-ai-cursor--chat-index nil
  "Hash table CWD -> (chat-dir title updated-at-ms).")

(defvar vterm-ai-cursor--chat-index-time nil
  "Time when chat-index was last built.")

(defun vterm-ai-cursor--activity-mtime (chat-dir)
  "Return the mtime of the most recent user-activity file in CHAT-DIR.
Prefers prompt_history.json (actual user input), falls back to meta.json."
  (let ((ph (expand-file-name "prompt_history.json" chat-dir)))
    (if (file-exists-p ph)
        (file-attribute-modification-time (file-attributes ph))
      (file-attribute-modification-time
       (file-attributes (expand-file-name "meta.json" chat-dir))))))

(defun vterm-ai-cursor--build-chat-index ()
  "Scan meta.json files to build CWD -> latest-chat mapping.
For each CWD, keeps the chat with the most recently modified
prompt_history.json (or meta.json as fallback)."
  (let ((chats-dir (expand-file-name "chats" vterm-ai-cursor--config-dir))
        (index (make-hash-table :test 'equal)))
    (when (file-directory-p chats-dir)
      (dolist (ws-dir (directory-files chats-dir t "\\`[^.]"))
        (when (file-directory-p ws-dir)
          (dolist (chat-dir (directory-files ws-dir t "\\`[^.]"))
            (when (file-directory-p chat-dir)
              (let ((meta-file (expand-file-name "meta.json" chat-dir)))
                (when (file-readable-p meta-file)
                  (condition-case nil
                      (let* ((meta (json-parse-string
                                    (with-temp-buffer
                                      (insert-file-contents meta-file)
                                      (buffer-string))
                                    :object-type 'alist))
                             (cwd (alist-get 'cwd meta))
                             (title (alist-get 'title meta))
                             (is-sub (eq (alist-get 'isSubagent meta) t))
                             (act-mt (vterm-ai-cursor--activity-mtime chat-dir)))
                        (when (and cwd (not is-sub))
                          (let ((existing (gethash cwd index)))
                            (when (or (not existing)
                                      (time-less-p (nth 2 existing) act-mt))
                              (puthash cwd (list chat-dir title act-mt)
                                       index)))))
                    (error nil)))))))))
    (setq vterm-ai-cursor--chat-index index
          vterm-ai-cursor--chat-index-time (current-time))))

(defun vterm-ai-cursor--get-chat-for-cwd (cwd)
  "Return (chat-dir title updated-at-ms) for CWD, or nil.
Rebuilds the index if older than 60 seconds."
  (unless (and vterm-ai-cursor--chat-index
               vterm-ai-cursor--chat-index-time
               (< (float-time (time-subtract (current-time)
                                             vterm-ai-cursor--chat-index-time))
                  60))
    (vterm-ai-cursor--build-chat-index))
  (gethash cwd vterm-ai-cursor--chat-index))

;;; --- Hex decode for store.db ---

(defun vterm-ai-cursor--hex-decode (hex)
  "Decode HEX string to UTF-8 text."
  (when (and hex (> (length hex) 0) (zerop (% (length hex) 2)))
    (decode-coding-string
     (apply #'unibyte-string
            (cl-loop for i from 0 below (length hex) by 2
                     collect (string-to-number
                              (substring hex i (+ i 2)) 16)))
     'utf-8)))

;;; --- Store.db meta reader ---

(defun vterm-ai-cursor--read-store-meta (chat-dir)
  "Read model and mode from store.db in CHAT-DIR.
The meta value is stored as hex-encoded JSON.
Return alist with model and mode, or nil."
  (let ((db (expand-file-name "store.db" chat-dir)))
    (when (file-readable-p db)
      (with-temp-buffer
        (let ((ret (call-process
                    "sqlite3" nil t nil db
                    "SELECT value FROM meta WHERE key='0';")))
          (when (and (zerop ret) (> (buffer-size) 0))
            (condition-case nil
                (let* ((hex (string-trim (buffer-string)))
                       (json-str (vterm-ai-cursor--hex-decode hex))
                       (data (and json-str
                                  (json-parse-string json-str
                                                     :object-type 'alist)))
                       (run-everything (eq (alist-get 'isRunEverything data) t))
                       (model (or (alist-get 'lastUsedModel data) "")))
                  `((model . ,model)
                    (mode . ,(if run-everything "bypassPermissions" ""))))
              (error nil))))))))

;;; --- Prompt history reader ---

(defun vterm-ai-cursor--read-prompts (chat-dir)
  "Read all prompts from prompt_history.json in CHAT-DIR.
Return a list of strings, or nil."
  (let ((file (expand-file-name "prompt_history.json" chat-dir)))
    (when (file-readable-p file)
      (condition-case nil
          (json-parse-string
           (with-temp-buffer
             (insert-file-contents file)
             (buffer-string))
           :array-type 'list)
        (error nil)))))

;;; --- Enrich cache ---

(defvar vterm-ai-cursor--enrich-cache (make-hash-table :test 'equal)
  "Cache keyed by chat-dir.  Values: (mtime title model mode last-prompt).")

(defun vterm-ai-cursor--newest-mtime (chat-dir)
  "Return the newest mtime among data files in CHAT-DIR."
  (let (newest)
    (dolist (name '("meta.json" "prompt_history.json" "store.db" "store.db-wal"))
      (let ((f (expand-file-name name chat-dir)))
        (when (file-exists-p f)
          (let ((mt (file-attribute-modification-time (file-attributes f))))
            (when (or (not newest) (time-less-p newest mt))
              (setq newest mt))))))
    newest))

;;; --- Enrich ---

(defun vterm-ai-cursor-enrich (session)
  "Enrich SESSION with status from buffer name and data from Cursor files."
  (let (buf-title)
    ;; Status from buffer name (most accurate real-time source)
    (let ((buf (vterm-ai-session-vterm-buffer session)))
      (when (and buf (buffer-live-p buf))
        (let ((parsed (vterm-ai-cursor--parse-buffer-name (buffer-name buf))))
          (when parsed
            (setf (vterm-ai-session-status session) (car parsed))
            (setq buf-title (cdr parsed))
            (when (string-match "\\`vterm: \\(.+\\)" buf-title)
              (setq buf-title (match-string 1 buf-title)))))))
    ;; Rich data from Cursor chat files
    (let* ((cwd (vterm-ai-session-cwd session))
           (chat-info (when cwd (vterm-ai-cursor--get-chat-for-cwd cwd))))
      (if chat-info
          (let* ((chat-dir (nth 0 chat-info))
                 (chat-uuid (file-name-nondirectory chat-dir))
                 (mtime (vterm-ai-cursor--newest-mtime chat-dir))
                 (cached (gethash chat-dir vterm-ai-cursor--enrich-cache)))
            (setf (vterm-ai-session-session-id session) chat-uuid)
            (if (and cached (equal (car cached) mtime))
                (let ((data (cdr cached)))
                  (setf (vterm-ai-session-title session) (nth 0 data))
                  (setf (vterm-ai-session-model session) (nth 1 data))
                  (setf (vterm-ai-session-mode session) (nth 2 data))
                  (setf (vterm-ai-session-last-prompt session) (nth 3 data)))
              (let* ((title (or (nth 1 chat-info) ""))
                     (store-meta (vterm-ai-cursor--read-store-meta chat-dir))
                     (model (or (alist-get 'model store-meta) ""))
                     (mode (or (alist-get 'mode store-meta) ""))
                     (prompts (vterm-ai-cursor--read-prompts chat-dir))
                     (last-prompt (or (car (last prompts)) "")))
                (setf (vterm-ai-session-title session) title)
                (setf (vterm-ai-session-model session) model)
                (setf (vterm-ai-session-mode session) mode)
                (setf (vterm-ai-session-last-prompt session) last-prompt)
                (puthash chat-dir (cons mtime (list title model mode last-prompt))
                         vterm-ai-cursor--enrich-cache))))
        ;; No chat found — fall back to buffer name title
        (when buf-title
          (setf (vterm-ai-session-title session) buf-title))
        (setf (vterm-ai-session-last-prompt session) "(not available)")))))

;;; --- Detail ---

(defun vterm-ai-cursor-detail (session)
  "Return a detailed string for SESSION with recent prompts."
  (let* ((cwd (vterm-ai-session-cwd session))
         (chat-info (when cwd (vterm-ai-cursor--get-chat-for-cwd cwd)))
         (chat-dir (and chat-info (nth 0 chat-info)))
         (prompts (when chat-dir (vterm-ai-cursor--read-prompts chat-dir))))
    (with-temp-buffer
      (insert (format "Session: %s\n" (or (vterm-ai-session-name session) "cursor")))
      (insert (format "Status:  %s\n" (or (vterm-ai-session-status session) "unknown")))
      (insert (format "CWD:     %s\n" (or cwd "N/A")))
      (insert (format "Title:   %s\n" (or (vterm-ai-session-title session) "N/A")))
      (insert (format "Model:   %s\n" (or (vterm-ai-session-model session) "N/A")))
      (insert (format "PID:     %d\n" (vterm-ai-session-pid session)))
      (insert "\n--- Recent Prompts ---\n\n")
      (if prompts
          (dolist (p prompts)
            (insert (format ">> %s\n\n"
                            (truncate-string-to-width p 200 nil nil "..."))))
        (insert "(no prompts found)\n"))
      (buffer-string))))

;;; --- Auto-register ---

(vterm-ai-register-provider
 '(:name "cursor"
   :get-sessions-async vterm-ai-cursor--get-sessions-async
   :enrich vterm-ai-cursor-enrich
   :detail vterm-ai-cursor-detail))

(provide 'vterm-ai-cursor)
;;; vterm-ai-cursor.el ends here

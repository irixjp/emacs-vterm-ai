;;; vterm-ai-opencode.el --- OpenCode provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the OpenCode provider: async process detection via ps/lsof
;; and SQLite-based session info retrieval from OpenCode's own database
;; (~/.local/share/opencode/opencode.db by default).  Unlike the Codex
;; and Cursor providers, busy/idle status is derived directly from the
;; database (whether the most recent message has finished), so no
;; terminal-title configuration is required to see live status.
;;
;; The database has no record of a pending tool-permission request --
;; that only exists in the running process's memory -- and OpenCode's
;; terminal title does not change with status either, so busy/idle/done
;; is all that can be inferred from files on disk.  To also detect the
;; "asking" (permission pending) state, install the companion OpenCode
;; plugin (opencode-plugin/vterm-ai-status.js, or M-x
;; vterm-ai-opencode-install-plugin), which hooks OpenCode's internal
;; event bus and writes live status to `vterm-ai-opencode-status-dir'.
;; This provider works without it, just without "asking" support.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vterm-ai-data)

(defcustom vterm-ai-opencode-db-path
  (expand-file-name "opencode/opencode.db"
                     (or (getenv "XDG_DATA_HOME")
                         (expand-file-name ".local/share" (getenv "HOME"))))
  "Path to OpenCode's SQLite database."
  :type 'file
  :group 'vterm-ai)

(defcustom vterm-ai-opencode-status-dir
  (expand-file-name "opencode/vterm-ai-status"
                     (or (getenv "XDG_DATA_HOME")
                         (expand-file-name ".local/share" (getenv "HOME"))))
  "Directory where the vterm-ai-status OpenCode plugin writes live
busy/idle/asking status, one JSON file per working directory.  See
`vterm-ai-opencode-install-plugin'.  Without the plugin installed,
this provider falls back to database-only busy/idle detection and
cannot show the \"asking\" (permission pending) state at all."
  :type 'directory
  :group 'vterm-ai)

(defcustom vterm-ai-opencode-plugin-dir
  (expand-file-name "opencode/plugins"
                     (or (getenv "XDG_CONFIG_HOME")
                         (expand-file-name ".config" (getenv "HOME"))))
  "OpenCode's global plugin directory.
Files placed here are loaded automatically by every OpenCode session.
See https://opencode.ai/docs/plugins/."
  :type 'directory
  :group 'vterm-ai)

(defconst vterm-ai-opencode--plugin-source
  (expand-file-name
   "opencode-plugin/vterm-ai-status.js"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Path to the bundled vterm-ai-status OpenCode plugin source.")

;;;###autoload
(defun vterm-ai-opencode-install-plugin ()
  "Install the vterm-ai-status OpenCode plugin.
Copies it into `vterm-ai-opencode-plugin-dir', which enables \"asking\"
\(permission pending) status detection for the OpenCode provider.
OpenCode only loads global plugins at startup, so restart any running
OpenCode sessions afterwards for this to take effect."
  (interactive)
  (unless (file-readable-p vterm-ai-opencode--plugin-source)
    (user-error "Bundled plugin not found at %s" vterm-ai-opencode--plugin-source))
  (make-directory vterm-ai-opencode-plugin-dir t)
  (let ((dest (expand-file-name "vterm-ai-status.js" vterm-ai-opencode-plugin-dir)))
    (copy-file vterm-ai-opencode--plugin-source dest t)
    (message "Installed %s -- restart OpenCode sessions to pick it up" dest)))

;;; --- Async process discovery ---

(defun vterm-ai-opencode--get-sessions-async (callback)
  "Detect running OpenCode processes asynchronously.
Call CALLBACK with a list of alists containing pid and cwd.
Return the process object for cancellation management."
  (let ((buf (generate-new-buffer " *vterm-ai-opencode*")))
    (make-process
     :name "vterm-ai-opencode-detect"
     :buffer buf
     :command '("sh" "-c"
                "ps -eo pid,comm | awk '{c=$2; sub(/^.*\\//, \"\", c); if (c == \"opencode\") print $1}' | while read pid; do cwd=$(lsof -a -d cwd -p \"$pid\" -F n 2>/dev/null | grep '^n/' | sed 's/^n//'); [ -n \"$cwd\" ] && echo \"$pid $cwd\"; done")
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

;;; --- SQLite helpers ---

(defun vterm-ai-opencode--db-query (sql)
  "Run SQL against the OpenCode database with JSON output.
Return the parsed JSON array (as a list of alists), or nil."
  (when (file-readable-p vterm-ai-opencode-db-path)
    (with-temp-buffer
      (let ((ret (call-process "sqlite3" nil t nil
                                "-readonly" "-json"
                                vterm-ai-opencode-db-path sql)))
        (when (and (zerop ret) (> (buffer-size) 0))
          (condition-case nil
              (json-parse-string (buffer-string)
                                  :object-type 'alist :array-type 'list)
            (error nil)))))))

(defun vterm-ai-opencode--sql-quote (str)
  "Escape STR for safe embedding in a single-quoted SQL string literal."
  (replace-regexp-in-string "'" "''" (or str "")))

;;; --- Model formatting ---

(defun vterm-ai-opencode--format-model (model-json)
  "Parse MODEL-JSON (as stored in the session.model column) into
\"providerID/id\", or nil if MODEL-JSON is absent or unparsable."
  (when (and model-json (stringp model-json) (not (string-empty-p model-json)))
    (condition-case nil
        (let* ((obj (json-parse-string model-json :object-type 'alist))
               (id (alist-get 'id obj))
               (provider (alist-get 'providerID obj)))
          (cond
           ((and provider id) (format "%s/%s" provider id))
           (id id)
           (t nil)))
      (error nil))))

;;; --- SQL NULL helper ---

(defun vterm-ai-opencode--nn (value)
  "Convert JSON null (parsed by `json-parse-string' as the symbol
`:null') to nil, so callers can use plain `or' for fallbacks.  SQL NULL
columns (e.g. session.agent, which is NULL for sessions created
without an explicit agent) would otherwise pass a non-nil `:null'
symbol through unchanged, which downstream code that expects a string
or nil can choke on."
  (unless (eq value :null) value))

;;; --- Title helpers ---

(defun vterm-ai-opencode--default-title-p (title)
  "Return non-nil if TITLE is OpenCode's placeholder for an untitled
session (e.g. \"New session - 2026-09-15T09:37:58.552Z\"), generated
before the AI has produced a real title.  OpenCode's own TUI treats
this the same way (falls back to a generic display) rather than
showing it as the session title."
  (and title (string-match-p "\\`New session\\( - .+\\)?\\'" title)))

;;; --- Session + status query ---

(defun vterm-ai-opencode--query-session (cwd)
  "Query the most recent non-archived OpenCode session for CWD.
Return an alist with session-id, name (slug), title, model, mode
\(agent), and status (idle/busy), or nil if no session is found."
  (let* ((sql (format "SELECT s.id, s.slug, s.title, s.model, s.agent,
  (SELECT json_extract(m.data,'$.role') FROM message m
     WHERE m.session_id = s.id ORDER BY m.time_created DESC LIMIT 1) AS last_role,
  (SELECT json_extract(m.data,'$.time.completed') FROM message m
     WHERE m.session_id = s.id ORDER BY m.time_created DESC LIMIT 1) AS last_completed
FROM session s
WHERE s.directory = '%s' AND s.time_archived IS NULL
ORDER BY s.time_updated DESC LIMIT 1;"
                       (vterm-ai-opencode--sql-quote cwd)))
         (rows (vterm-ai-opencode--db-query sql))
         (row (car rows)))
    (when row
      (let ((role (vterm-ai-opencode--nn (alist-get 'last_role row)))
            (completed (vterm-ai-opencode--nn (alist-get 'last_completed row)))
            (title (vterm-ai-opencode--nn (alist-get 'title row))))
        `((session-id . ,(vterm-ai-opencode--nn (alist-get 'id row)))
          (name . ,(vterm-ai-opencode--nn (alist-get 'slug row)))
          (title . ,(if (vterm-ai-opencode--default-title-p title) "" title))
          (model . ,(vterm-ai-opencode--format-model (alist-get 'model row)))
          (mode . ,(vterm-ai-opencode--nn (alist-get 'agent row)))
          (status . ,(cond
                       ((null role) "idle")
                       ((equal role "user") "busy")
                       ((and (equal role "assistant") (null completed)) "busy")
                       (t "idle"))))))))

;;; --- Last prompt / recent prompts ---

(defun vterm-ai-opencode--query-recent-prompts (session-id &optional limit)
  "Query the last LIMIT (default 1) human prompts for SESSION-ID.
Return a list of strings, most recent first."
  (when session-id
    (let* ((sql (format "SELECT json_extract(p.data,'$.text') AS text
FROM message m JOIN part p ON p.message_id = m.id
WHERE m.session_id = '%s'
  AND json_extract(m.data,'$.role') = 'user'
  AND json_extract(p.data,'$.type') = 'text'
ORDER BY m.time_created DESC, p.time_created ASC LIMIT %d;"
                         (vterm-ai-opencode--sql-quote session-id)
                         (or limit 1)))
           (rows (vterm-ai-opencode--db-query sql)))
      (delq nil (mapcar (lambda (row) (alist-get 'text row)) rows)))))

;;; --- Live status (from the vterm-ai-status plugin) ---

(defun vterm-ai-opencode--status-file (cwd pid)
  "Return the vterm-ai-status plugin's status file path for CWD and PID.
The pid is part of the filename (not just a field inside it) so that
two OpenCode processes running in the same directory at once -- e.g.
two terminals both cd'd into the same project -- each get their own
file instead of racing to overwrite one shared file.  The sanitization
here must match the plugin's `sanitize' function
\(opencode-plugin/vterm-ai-status.js) exactly, or the two sides will
look for different filenames."
  (expand-file-name
   (format "%s-%d.json" (replace-regexp-in-string "[^a-zA-Z0-9]" "-" cwd) pid)
   vterm-ai-opencode-status-dir))

(defun vterm-ai-opencode--live-status (cwd pid)
  "Return the live status ATOM (\"idle\"/\"busy\"/\"waiting\") for the
OpenCode process PID running in CWD, as last written by the
vterm-ai-status OpenCode plugin, or nil if unavailable (e.g. the
plugin isn't installed, or hasn't written anything for this process
yet).  The recorded pid inside the file is double-checked against PID
as well, purely defensively."
  (when pid
    (let ((file (vterm-ai-opencode--status-file cwd pid)))
      (when (file-readable-p file)
        (condition-case nil
            (let* ((data (json-parse-string
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))
                          :object-type 'alist))
                   (file-pid (alist-get 'pid data))
                   (status (alist-get 'status data)))
              (when (and status file-pid (= file-pid pid))
                status))
          (error nil))))))

;;; --- Enrich cache ---

(defvar vterm-ai-opencode--enrich-cache (make-hash-table :test 'equal)
  "Cache keyed by cwd.
Values: (mtime session-id name title model mode status last-prompt).")

(defun vterm-ai-opencode--newest-mtime ()
  "Return the newest mtime among the OpenCode database files."
  (let (newest)
    (dolist (suffix '("" "-wal" "-shm"))
      (let ((f (concat vterm-ai-opencode-db-path suffix)))
        (when (file-exists-p f)
          (let ((mt (file-attribute-modification-time (file-attributes f))))
            (when (or (not newest) (time-less-p newest mt))
              (setq newest mt))))))
    newest))

;;; --- Provider interface ---

(defun vterm-ai-opencode-enrich (session)
  "Enrich SESSION with title, model, mode, status, and last-prompt
from OpenCode's SQLite database, then overlay live busy/idle/asking
status from the vterm-ai-status OpenCode plugin, if installed and
running for this session (see `vterm-ai-opencode-install-plugin')."
  (let* ((cwd (vterm-ai-session-cwd session))
         (mtime (vterm-ai-opencode--newest-mtime))
         (cached (gethash cwd vterm-ai-opencode--enrich-cache)))
    (if (and cached mtime (equal (car cached) mtime))
        (let ((data (cdr cached)))
          (setf (vterm-ai-session-session-id session) (nth 0 data))
          (setf (vterm-ai-session-name session) (nth 1 data))
          (setf (vterm-ai-session-title session) (nth 2 data))
          (setf (vterm-ai-session-model session) (nth 3 data))
          (setf (vterm-ai-session-mode session) (nth 4 data))
          (setf (vterm-ai-session-status session) (nth 5 data))
          (setf (vterm-ai-session-last-prompt session) (nth 6 data)))
      (let ((info (and cwd (vterm-ai-opencode--query-session cwd))))
        (if info
            (let* ((sid (alist-get 'session-id info))
                   (name (or (alist-get 'name info) ""))
                   (title (or (alist-get 'title info) ""))
                   (model (or (alist-get 'model info) ""))
                   (mode (or (alist-get 'mode info) ""))
                   (status (or (alist-get 'status info) "unknown"))
                   (last-prompt (or (car (vterm-ai-opencode--query-recent-prompts sid 1))
                                    "")))
              (setf (vterm-ai-session-session-id session) sid)
              (setf (vterm-ai-session-name session) name)
              (setf (vterm-ai-session-title session) title)
              (setf (vterm-ai-session-model session) model)
              (setf (vterm-ai-session-mode session) mode)
              (setf (vterm-ai-session-status session) status)
              (setf (vterm-ai-session-last-prompt session) last-prompt)
              (puthash cwd
                       (cons mtime (list sid name title model mode status last-prompt))
                       vterm-ai-opencode--enrich-cache))
          (setf (vterm-ai-session-last-prompt session) "(not available)"))))
    ;; The plugin's status file is cheap to read and can change (e.g. a
    ;; permission request opening or closing) without the database
    ;; mtime changing in lockstep, so this check always runs fresh,
    ;; independent of the cache branch above.
    (let ((live-status (vterm-ai-opencode--live-status
                        cwd (vterm-ai-session-pid session))))
      (when live-status
        (setf (vterm-ai-session-status session) live-status)))))

(defun vterm-ai-opencode-detail (session)
  "Return a detailed string for SESSION with recent prompts."
  (let* ((sid (vterm-ai-session-session-id session))
         (prompts (when sid (vterm-ai-opencode--query-recent-prompts sid 10))))
    (with-temp-buffer
      (insert (format "Session: %s\n" (or (vterm-ai-session-name session) "opencode")))
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
 '(:name "opencode"
   :get-sessions-async vterm-ai-opencode--get-sessions-async
   :enrich vterm-ai-opencode-enrich
   :detail vterm-ai-opencode-detail))

(provide 'vterm-ai-opencode)
;;; vterm-ai-opencode.el ends here

;;; vterm-ai-claude.el --- Claude Code provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Claude Code provider: async session discovery via
;; `claude agents --json' and lightweight JSONL transcript parsing
;; for title, model, and last prompt.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vterm-ai-data)

(defvar vterm-ai-claude--dir
  (or (getenv "CLAUDE_CONFIG_DIR")
      (expand-file-name ".claude" (getenv "HOME")))
  "Legacy single Claude config directory.
Superseded by `vterm-ai-claude-config-dirs'; kept as its default value.")

(defcustom vterm-ai-claude-config-dirs
  (list vterm-ai-claude--dir)
  "List of Claude Code config directories to query.

Each entry corresponds to a value that could be passed via the
`CLAUDE_CONFIG_DIR' environment variable when launching `claude'
\(for example via separate shell aliases for different accounts
or providers).  vterm-ai runs `claude agents --json' once per
directory and merges the results, and looks up transcripts in
whichever directory actually contains them.

If you only ever use the default `~/.claude' directory (or a
single `CLAUDE_CONFIG_DIR'), leave this at its default.  If you
use several, list them all, e.g.:

  (setq vterm-ai-claude-config-dirs
        (list \"~/.claude_work\" \"~/.claude_personal\"))"
  :type '(repeat directory)
  :group 'vterm-ai)

;;; --- Async session discovery ---

(defun vterm-ai-claude--get-sessions-one (dir callback)
  "Run `claude agents --json' with `CLAUDE_CONFIG_DIR' set to DIR.
Call CALLBACK with a list of alists on completion.
Return the process object for cancellation management."
  (let* ((buf (generate-new-buffer " *vterm-ai-claude*"))
         (process-environment
          (cons (format "CLAUDE_CONFIG_DIR=%s" (expand-file-name dir))
                process-environment)))
    (make-process
     :name "vterm-ai-claude-agents"
     :buffer buf
     :command '("claude" "agents" "--json")
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (funcall callback
                      (when (and (zerop (process-exit-status proc))
                                 (buffer-live-p (process-buffer proc)))
                        (with-current-buffer (process-buffer proc)
                          (goto-char (point-min))
                          (condition-case nil
                              (let* ((r (json-parse-buffer :object-type 'alist))
                                     (lst (if (vectorp r) (append r nil) r)))
                                (cl-remove-if
                                 (lambda (a)
                                   (or
                                    ;; Exclude subagents/forks. Claude harness marks
                                    ;; these with the "⑂" glyph in session name.
                                    (string-match-p "⑂" (or (alist-get 'name a) ""))
                                    ;; Hide finished background sessions. They can stay
                                    ;; visible in `claude agents --json` while their
                                    ;; worker process still exists, but they are no
                                    ;; longer actionable in vterm-ai.
                                    (and (equal (alist-get 'kind a) "background")
                                         (equal (alist-get 'state a) "done"))))
                                 lst))
                            (error nil)))))
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

(defun vterm-ai-claude--get-sessions-async (callback)
  "Run `claude agents --json' once per `vterm-ai-claude-config-dirs'.
Merge the results and call CALLBACK with the combined list of alists.
Return a list of process objects for cancellation management."
  (let* ((dirs (or vterm-ai-claude-config-dirs (list vterm-ai-claude--dir)))
         (pending (length dirs))
         (all nil))
    (if (zerop pending)
        (progn (funcall callback nil) nil)
      (mapcar
       (lambda (dir)
         (vterm-ai-claude--get-sessions-one
          dir
          (lambda (agents)
            (setq all (append all agents))
            (cl-decf pending)
            (when (zerop pending)
              (funcall callback all)))))
       dirs))))

;;; --- Transcript helpers ---

(defun vterm-ai-claude--transcript-path (cwd session-id)
  "Derive transcript JSONL path from CWD and SESSION-ID.
Searches each directory in `vterm-ai-claude-config-dirs' and returns
the first path where the file actually exists, falling back to the
path under the first configured directory if none match."
  (let* ((escaped (replace-regexp-in-string "[^[:alnum:]]" "-" cwd))
         (dirs (or vterm-ai-claude-config-dirs (list vterm-ai-claude--dir)))
         (candidates
          (mapcar (lambda (dir)
                    (expand-file-name (concat session-id ".jsonl")
                                      (expand-file-name escaped
                                                        (expand-file-name "projects"
                                                                          (expand-file-name dir)))))
                  dirs)))
    (or (cl-find-if #'file-readable-p candidates)
        (car candidates))))

(defun vterm-ai-claude--read-tail (file &optional bytes)
  "Read the last BYTES (default 16384) of FILE as parsed JSON lines.
Only parses lines relevant to lightweight extraction: ai-title,
last-prompt, small user messages, and small assistant messages (for model)."
  (let ((bytes (or bytes 16384)))
    (when (file-readable-p file)
      (let* ((attrs (file-attributes file))
             (size (file-attribute-size attrs))
             (start (max 0 (- size bytes))))
        (with-temp-buffer
          (insert-file-contents file nil start size)
          (goto-char (point-min))
          (when (> start 0)
            (forward-line 1))
          (let (result)
            (while (not (eobp))
              (let* ((beg (line-beginning-position))
                     (end (line-end-position))
                     (len (- end beg))
                     relevant)
                (when (> len 0)
                  (save-excursion
                    (goto-char beg)
                    (setq relevant
                          (or (search-forward "\"ai-title\"" end t)
                              (progn (goto-char beg)
                                     (search-forward "\"last-prompt\"" end t))
                              (and (progn (goto-char beg)
                                         (search-forward "\"type\":\"user\"" end t))
                                   (< len 32768))
                              (and (progn (goto-char beg)
                                         (search-forward "\"type\":\"assistant\"" end t))
                                   (< len 32768)))))
                  (when relevant
                    (let ((line (buffer-substring-no-properties beg end)))
                      (condition-case nil
                          (push (json-parse-string line :object-type 'alist) result)
                        (error nil)))))
)
              (forward-line 1))
            (nreverse result)))))))

(defconst vterm-ai-claude--tail-growth-factor 4
  "Multiplier applied to the tail window each retry in
`vterm-ai-claude--read-tail-expanding' when the title is not found.")

(defconst vterm-ai-claude--tail-max-attempts 4
  "Maximum number of window expansions in
`vterm-ai-claude--read-tail-expanding' (16K -> 64K -> 256K -> 1M).")

(defun vterm-ai-claude--read-tail-expanding (file)
  "Read tail entries from FILE, growing the window backward when the
title is not found in the initial (lightweight) window.

A single busy conversation turn can push the most recent ai-title
past a small fixed-size tail window (see `vterm-ai-claude--read-tail'),
which would otherwise make the title look empty until the next
unrelated transcript change shifts the window.  Retrying with a
larger window is bounded by `vterm-ai-claude--tail-max-attempts' so a
transcript that genuinely has no title yet does not get re-read in
full on every enrich cycle."
  (let ((bytes 16384)
        (attempts 0)
        (size (and (file-readable-p file)
                   (file-attribute-size (file-attributes file))))
        entries)
    (while (progn
             (setq entries (vterm-ai-claude--read-tail file bytes))
             (and (not (alist-get 'title (vterm-ai-claude--extract-summary entries)))
                  (< attempts vterm-ai-claude--tail-max-attempts)
                  (< bytes (or size 0))))
      (setq bytes (* bytes vterm-ai-claude--tail-growth-factor))
      (cl-incf attempts))
    entries))

(defun vterm-ai-claude--extract-text (content)
  "Extract text from a message CONTENT field (string or content-block array)."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (let (texts)
      (seq-doseq (block content)
        (when (equal (alist-get 'type block) "text")
          (push (alist-get 'text block) texts)))
      (mapconcat #'identity (nreverse texts) "\n")))
   (t "")))

(defun vterm-ai-claude--extract-summary (entries)
  "Extract title, last-prompt, and model from ENTRIES."
  (let (title last-prompt last-human-prompt model)
    (dolist (entry (reverse entries))
      (let ((type (alist-get 'type entry)))
        (cond
         ((and (not title) (equal type "ai-title"))
          (setq title (alist-get 'aiTitle entry)))
         ((and (not last-prompt) (equal type "last-prompt"))
          (setq last-prompt (alist-get 'lastPrompt entry)))
         ((and (not model) (equal type "assistant"))
          (let ((msg (alist-get 'message entry)))
            (setq model (alist-get 'model msg))))
         ((and (not last-human-prompt) (equal type "user")
               (not (alist-get 'toolUseResult entry)))
          (let* ((msg (alist-get 'message entry))
                 (content (alist-get 'content msg))
                 (text (vterm-ai-claude--extract-text content)))
            (unless (string-empty-p text)
              (setq last-human-prompt text)))))))
    `((title . ,title)
      (last-prompt . ,(or last-human-prompt last-prompt))
      (model . ,model))))

(defun vterm-ai-claude--read-permission-mode (file)
  "Read the last permission-mode value from FILE.
Scans backwards from end in 64KB chunks to find the last
type:permission-mode entry without reading the whole file."
  (when (file-readable-p file)
    (let* ((size (file-attribute-size (file-attributes file)))
           (chunk 65536)
           (pos size)
           result)
      (while (and (not result) (> pos 0))
        (let* ((start (max 0 (- pos chunk)))
               (end pos))
          (with-temp-buffer
            (insert-file-contents file nil start end)
            (goto-char (point-max))
            (when (search-backward "\"type\":\"permission-mode\"" nil t)
              (beginning-of-line)
              (let ((line (buffer-substring-no-properties
                           (point) (line-end-position))))
                (condition-case nil
                    (let ((obj (json-parse-string line :object-type 'alist)))
                      (setq result (alist-get 'permissionMode obj)))
                  (error nil))))))
        (setq pos (max 0 (- pos chunk))))
      result)))

;;; --- vterm buffer fallback linking ---

(defun vterm-ai-claude--normalize-dir (dir)
  "Return DIR as an absolute, truenamed directory path with trailing slash.
If DIR is nil/empty, return nil.  Falls back to `expand-file-name' if
`file-truename' cannot be resolved (for example on missing paths)."
  (when (and (stringp dir) (not (string-empty-p dir)))
    (let* ((expanded (expand-file-name dir))
           (truename (condition-case nil
                         (file-truename expanded)
                       (error expanded))))
      (file-name-as-directory truename))))

(defun vterm-ai-claude--vterm-shell-pid (buf)
  "Return BUF's live vterm shell pid, or nil if unavailable."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (eq major-mode 'vterm-mode)
                 (boundp 'vterm--process)
                 vterm--process
                 (process-live-p vterm--process))
        (process-id vterm--process)))))

(defun vterm-ai-claude--find-vterm-buffer-fallback (session)
  "Best-effort vterm buffer lookup for SESSION when PID linking fails.
Daemon-managed Claude sessions can report a worker pid that is detached from
the vterm shell process tree, so `vterm-ai-data--find-vterm-buffer' cannot
resolve them by PPID chain.  This fallback tries:
1) session-name + cwd match
2) session-name match
3) cwd match
Only a unique candidate is accepted at each step."
  (let* ((session-name (vterm-ai-session-name session))
         (session-cwd (vterm-ai-claude--normalize-dir
                       (vterm-ai-session-cwd session)))
         name-matches
         cwd-matches)
    (dolist (buf (buffer-list))
      (let ((shell-pid (vterm-ai-claude--vterm-shell-pid buf)))
        (when shell-pid
          (with-current-buffer buf
            (let ((buf-name (buffer-name buf))
                  (buf-cwd (vterm-ai-claude--normalize-dir default-directory)))
              (when (and (stringp session-name)
                         (not (string-empty-p session-name))
                         (string-match-p (regexp-quote session-name) buf-name))
                (push buf name-matches))
              (when (and session-cwd buf-cwd (equal session-cwd buf-cwd))
                (push buf cwd-matches)))))))
    (let ((name+cwd (cl-intersection name-matches cwd-matches :test #'eq)))
      (cond
       ((= (length name+cwd) 1) (car name+cwd))
       ((= (length name-matches) 1) (car name-matches))
       ((= (length cwd-matches) 1) (car cwd-matches))
       (t nil)))))

;;; --- Enrichment cache ---

(defvar vterm-ai-claude--enrich-cache (make-hash-table :test 'equal)
  "Cache keyed by session-id.  Values: (mtime title last-prompt model mode).")

;;; --- Provider interface ---

(defun vterm-ai-claude-enrich (session)
  "Enrich SESSION with title, model, and last-prompt from the transcript.
Uses file modification time to skip re-reading unchanged files."
  (let* ((cwd (vterm-ai-session-cwd session))
         (sid (vterm-ai-session-session-id session))
         (file (vterm-ai-claude--transcript-path cwd sid)))
    (when (file-readable-p file)
      (let* ((mtime (file-attribute-modification-time (file-attributes file)))
             (cached (gethash sid vterm-ai-claude--enrich-cache)))
        (if (and cached (equal (car cached) mtime))
            (let ((data (cdr cached)))
              (setf (vterm-ai-session-title session) (nth 0 data))
              (setf (vterm-ai-session-last-prompt session) (nth 1 data))
              (setf (vterm-ai-session-model session) (nth 2 data))
              (setf (vterm-ai-session-mode session) (nth 3 data)))
          (let* ((entries (vterm-ai-claude--read-tail-expanding file))
                 (summary (vterm-ai-claude--extract-summary entries))
                 (name (or (vterm-ai-session-name session) ""))
                 (title-from-transcript (or (alist-get 'title summary) ""))
                 (title (if (and (not (string-empty-p name))
                                 ;; During permission prompts, transcript ai-title
                                 ;; can lag behind a renamed session title. Prefer
                                 ;; the authoritative session name when waiting.
                                 (equal (vterm-ai-session-status session) "waiting"))
                            name
                          title-from-transcript))
                 (prompt (or (alist-get 'last-prompt summary) ""))
                 (model (or (alist-get 'model summary) ""))
                 (mode (or (vterm-ai-claude--read-permission-mode file) "")))
            (setf (vterm-ai-session-title session) title)
            (setf (vterm-ai-session-last-prompt session) prompt)
            (setf (vterm-ai-session-model session) model)
            (setf (vterm-ai-session-mode session) mode)
            (puthash sid (cons mtime (list title prompt model mode))
                     vterm-ai-claude--enrich-cache))))
      (unless (vterm-ai-session-vterm-buffer session)
        (let ((fallback (vterm-ai-claude--find-vterm-buffer-fallback session)))
          (when fallback
            (setf (vterm-ai-session-vterm-buffer session) fallback
                  (vterm-ai-session-shell-pid session)
                  (vterm-ai-claude--vterm-shell-pid fallback))))))))

(defun vterm-ai-claude--extract-recent-prompts (entries &optional limit)
  "Extract the last LIMIT (default 5) human prompts from ENTRIES."
  (let ((limit (or limit 5))
        prompts)
    (dolist (entry (reverse entries))
      (when (and (equal (alist-get 'type entry) "user")
                 (not (alist-get 'toolUseResult entry))
                 (< (length prompts) limit))
        (let* ((msg (alist-get 'message entry))
               (content (alist-get 'content msg))
               (text (vterm-ai-claude--extract-text content)))
          (unless (string-empty-p text)
            (push text prompts)))))
    (nreverse prompts)))

(defun vterm-ai-claude-detail (session)
  "Return a detailed string for SESSION with recent prompts."
  (let* ((cwd (vterm-ai-session-cwd session))
         (sid (vterm-ai-session-session-id session))
         (file (vterm-ai-claude--transcript-path cwd sid)))
    (if (not (file-readable-p file))
        "Transcript file not found."
      (let* ((entries (vterm-ai-claude--read-tail file 32768))
             (summary (vterm-ai-claude--extract-summary entries))
             (prompts (vterm-ai-claude--extract-recent-prompts entries)))
        (with-temp-buffer
          (insert (format "Session: %s\n" (vterm-ai-session-name session)))
          (insert (format "Status:  %s\n" (vterm-ai-session-status session)))
          (insert (format "CWD:     %s\n" (vterm-ai-session-cwd session)))
          (insert (format "Title:   %s\n" (or (alist-get 'title summary) "N/A")))
          (insert (format "Model:   %s\n" (or (alist-get 'model summary) "N/A")))
          (insert (format "PID:     %d\n" (vterm-ai-session-pid session)))
          (insert "\n--- Recent Prompts ---\n\n")
          (if prompts
              (dolist (p prompts)
                (insert (format ">> %s\n\n"
                                (truncate-string-to-width p 200 nil nil "..."))))
            (insert "(no prompts found)\n"))
          (buffer-string))))))

;;; --- Auto-register ---

(vterm-ai-register-provider
 '(:name "claude"
   :get-sessions-async vterm-ai-claude--get-sessions-async
   :enrich vterm-ai-claude-enrich
   :detail vterm-ai-claude-detail))

(provide 'vterm-ai-claude)
;;; vterm-ai-claude.el ends here

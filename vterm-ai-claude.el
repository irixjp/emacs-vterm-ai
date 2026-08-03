;;; vterm-ai-claude.el --- Claude Code provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Claude Code provider: async session discovery via
;; `claude agents --json' and lightweight JSONL transcript parsing
;; for title, model, and last prompt.

;;; Code:

(require 'json)
(require 'vterm-ai-data)

(defvar vterm-ai-claude--dir
  (expand-file-name ".claude" (getenv "HOME")))

;;; --- Async session discovery ---

(defun vterm-ai-claude--get-sessions-async (callback)
  "Run `claude agents --json' asynchronously.
Call CALLBACK with a list of alists on completion.
Return the process object for cancellation management."
  (let ((buf (generate-new-buffer " *vterm-ai-claude*")))
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
                                 (lambda (a) (equal (alist-get 'kind a) "background"))
                                 lst))
                            (error nil)))))
           (when (buffer-live-p (process-buffer proc))
             (kill-buffer (process-buffer proc)))))))))

;;; --- Transcript helpers ---

(defun vterm-ai-claude--transcript-path (cwd session-id)
  "Derive transcript JSONL path from CWD and SESSION-ID."
  (let ((escaped (concat "-" (substring (replace-regexp-in-string "/" "-" cwd) 1))))
    (expand-file-name (concat session-id ".jsonl")
                      (expand-file-name escaped
                                        (expand-file-name "projects"
                                                          vterm-ai-claude--dir)))))

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
          (let* ((entries (vterm-ai-claude--read-tail file))
                 (summary (vterm-ai-claude--extract-summary entries))
                 (title (or (alist-get 'title summary) ""))
                 (prompt (or (alist-get 'last-prompt summary) ""))
                 (model (or (alist-get 'model summary) ""))
                 (mode (or (vterm-ai-claude--read-permission-mode file) "")))
            (setf (vterm-ai-session-title session) title)
            (setf (vterm-ai-session-last-prompt session) prompt)
            (setf (vterm-ai-session-model session) model)
            (setf (vterm-ai-session-mode session) mode)
            (puthash sid (cons mtime (list title prompt model mode))
                     vterm-ai-claude--enrich-cache)))))))

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

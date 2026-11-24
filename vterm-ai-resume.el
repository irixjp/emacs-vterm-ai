;;; vterm-ai-resume.el --- Resume past agent sessions in vterm  -*- lexical-binding: t; -*-

;;; Commentary:
;; `vterm-ai-resume' lists recent Claude Code and Codex sessions from
;; every configured account (Claude config dir / CODEX_HOME), and
;; resumes the chosen one in a new vterm in its working directory,
;; launched with the command that account uses (for example a shell
;; alias that sets CLAUDE_CONFIG_DIR).  A session that is running in a
;; vterm right now is offered as a jump to that buffer instead, so one
;; conversation never runs in two terminals.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'vterm-ai-data)

(defvar vterm-ai-enabled-providers)
(defvar vterm-ai-claude-config-dirs)
(defvar vterm-ai-codex-homes)
(defvar vterm-ai-dashboard--sessions)
(declare-function vterm-ai--load-providers "vterm-ai")
(declare-function vterm-ai-claude--transcript-path "vterm-ai-claude")
(declare-function vterm-ai-codex--sqlite "vterm-ai-codex")
(declare-function vterm-ai-codex--db-file "vterm-ai-codex")
(declare-function vterm-other-window "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(defcustom vterm-ai-resume-commands
  '(("~/.claude" . "claude")
    ("~/.codex" . "codex"))
  "Alist of (DIRECTORY . COMMAND) used to resume sessions.
DIRECTORY is a Claude config dir (see `vterm-ai-claude-config-dirs') or
a CODEX_HOME (see `vterm-ai-codex-homes').  COMMAND starts that agent
with that directory, for example a shell alias such as
\"claude-personal\".  Sessions from directories missing here are not
listed."
  :type '(alist :key-type directory :value-type string)
  :group 'vterm-ai)

(defcustom vterm-ai-resume-days 30
  "Only sessions active within this many days are listed."
  :type 'integer
  :group 'vterm-ai)

(defcustom vterm-ai-resume-send-delay 1.0
  "Seconds to wait after opening a vterm before typing the resume command."
  :type 'number
  :group 'vterm-ai)

;;; --- Candidates ---

(defun vterm-ai-resume--command-for (dir)
  "Return the resume command configured for DIR, or nil."
  (let ((target (file-name-as-directory (expand-file-name dir))))
    (cdr (cl-find-if (lambda (cell)
                       (equal (file-name-as-directory (expand-file-name (car cell)))
                              target))
                     vterm-ai-resume-commands))))

(defun vterm-ai-resume--account (dir)
  "Return a short account label for DIR, e.g. \"personal\" for ~/.claude_personal."
  (let ((base (file-name-nondirectory (directory-file-name (expand-file-name dir)))))
    (if (string-match "\\`\\.\\(?:claude\\|codex\\)_?\\(.*\\)\\'" base)
        (let ((name (match-string 1 base)))
          (if (string-empty-p name) "default" name))
      base)))

(defun vterm-ai-resume--claude-sessions (dir since)
  "Return Claude sessions of config DIR active after SINCE (epoch seconds).
Read from DIR/history.jsonl, which logs each prompt with its session id,
project directory, and time.  Each session is a plist with :agent :dir
:id :cwd :time :label, where :label is its last prompt that is not a
slash command.  Sessions whose transcript is gone cannot be resumed and
are skipped."
  (let ((file (expand-file-name "history.jsonl" (expand-file-name dir)))
        (table (make-hash-table :test 'equal)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (condition-case nil
                (let* ((obj (json-parse-string line :object-type 'alist
                                               :null-object nil))
                       (sid (alist-get 'sessionId obj))
                       (cwd (alist-get 'project obj))
                       (ts (alist-get 'timestamp obj))
                       (time (and (numberp ts) (/ ts 1000.0)))
                       (text (string-trim (or (alist-get 'display obj) ""))))
                  (when (and (stringp sid) (stringp cwd) time (> time since))
                    (let ((old (gethash sid table)))
                      (puthash sid
                               (list :agent "claude" :dir dir :id sid :cwd cwd
                                     :time (max time (or (plist-get old :time) 0))
                                     :label (if (or (string-empty-p text)
                                                    (string-prefix-p "/" text))
                                                (or (plist-get old :label) text)
                                              text))
                               table))))
              (error nil)))
          (forward-line 1))))
    (let (result)
      (maphash
       (lambda (sid s)
         (when (or (not (fboundp 'vterm-ai-claude--transcript-path))
                   (let ((vterm-ai-claude-config-dirs (list dir)))
                     (file-readable-p
                      (vterm-ai-claude--transcript-path (plist-get s :cwd) sid))))
           (push s result)))
       table)
      result)))

(defun vterm-ai-resume--codex-sessions (home since)
  "Return interactive Codex sessions of HOME updated after SINCE (epoch seconds).
Each is a plist like those of `vterm-ai-resume--claude-sessions'."
  (when (fboundp 'vterm-ai-codex--sqlite)
    (let ((out (vterm-ai-codex--sqlite
                (vterm-ai-codex--db-file home "state_5.sqlite")
                (format "SELECT id, cwd, updated_at, replace(replace(COALESCE(name, title), char(10), ' '), char(9), ' ') FROM threads WHERE source = 'cli' AND archived = 0 AND updated_at > %d ORDER BY updated_at DESC;"
                        (floor since)))))
      (when out
        (delq nil
              (mapcar (lambda (line)
                        (let ((parts (split-string line "\t")))
                          (when (>= (length parts) 4)
                            (list :agent "codex" :dir home :id (nth 0 parts)
                                  :cwd (nth 1 parts)
                                  :time (string-to-number (nth 2 parts))
                                  :label (nth 3 parts)))))
                      (split-string out "\n" t)))))))

(defun vterm-ai-resume--running-buffer (session)
  "Return the vterm buffer where SESSION runs now, or nil.
Uses the sessions last collected for the dashboard."
  (let ((live (cl-find-if
               (lambda (s)
                 (and (equal (vterm-ai-session-type s) (plist-get session :agent))
                      (equal (vterm-ai-session-session-id s) (plist-get session :id))))
               (bound-and-true-p vterm-ai-dashboard--sessions))))
    (and live
         (buffer-live-p (vterm-ai-session-vterm-buffer live))
         (vterm-ai-session-vterm-buffer live))))

(defun vterm-ai-resume--sessions ()
  "Return resumable sessions of every configured account, newest first."
  (let ((since (- (float-time) (* 86400 vterm-ai-resume-days)))
        result)
    (when (memq 'claude vterm-ai-enabled-providers)
      (dolist (dir (bound-and-true-p vterm-ai-claude-config-dirs))
        (when (vterm-ai-resume--command-for dir)
          (setq result (nconc (vterm-ai-resume--claude-sessions dir since) result)))))
    (when (memq 'codex vterm-ai-enabled-providers)
      (dolist (home (bound-and-true-p vterm-ai-codex-homes))
        (when (vterm-ai-resume--command-for home)
          (setq result (nconc (vterm-ai-resume--codex-sessions home since) result)))))
    (sort result (lambda (a b) (> (plist-get a :time) (plist-get b :time))))))

(defun vterm-ai-resume--shorten-path (path width)
  "Return PATH abbreviated to WIDTH columns, keeping its end (the project)."
  (let ((p (abbreviate-file-name path)))
    (if (<= (string-width p) width)
        p
      (let* ((mark "…")
             (keep (max 0 (- width (string-width mark)))))
        (concat mark (substring p (- (length p) keep)))))))

(defun vterm-ai-resume--candidate (session &optional account-width)
  "Return the completion string for SESSION.
ACCOUNT-WIDTH pads the account column so all candidates line up."
  (let ((running (vterm-ai-resume--running-buffer session)))
    (format (format "%%-%ds %%-6s %%-28s %%s  %%s%%s" (or account-width 12))
            (format "[%s]" (vterm-ai-resume--account (plist-get session :dir)))
            (plist-get session :agent)
            (vterm-ai-resume--shorten-path (plist-get session :cwd) 28)
            (format-time-string "%m/%d %H:%M" (plist-get session :time))
            (if running (propertize "(running) " 'face 'vterm-ai-status-busy) "")
            (truncate-string-to-width
             (replace-regexp-in-string "[\n\r\t]+" " " (or (plist-get session :label) ""))
             60 nil nil "…"))))

(defun vterm-ai-resume--command (session)
  "Return the shell command that resumes SESSION."
  (let ((cmd (vterm-ai-resume--command-for (plist-get session :dir)))
        (id (shell-quote-argument (plist-get session :id))))
    (if (equal (plist-get session :agent) "codex")
        (format "%s resume %s" cmd id)
      (format "%s --resume %s" cmd id))))

;;; --- Command ---

;;;###autoload
(defun vterm-ai-resume ()
  "Pick a recent agent session from any account and resume it in vterm.
A session already running in a vterm is shown there instead."
  (interactive)
  (when (fboundp 'vterm-ai--load-providers)
    (vterm-ai--load-providers))
  (let* ((sessions (vterm-ai-resume--sessions))
         (account-width
          (apply #'max 0 (mapcar (lambda (s)
                                   (+ 2 (string-width
                                         (vterm-ai-resume--account (plist-get s :dir)))))
                                 sessions)))
         (table (mapcar (lambda (s)
                          (cons (vterm-ai-resume--candidate s account-width) s))
                        sessions)))
    (unless table
      (user-error "No resumable sessions in the last %d days (see `vterm-ai-resume-commands')"
                  vterm-ai-resume-days))
    (let* ((choice (completing-read
                    "Resume session: "
                    (lambda (string pred action)
                      (if (eq action 'metadata)
                          '(metadata (display-sort-function . identity)
                                     (cycle-sort-function . identity))
                        (complete-with-action action table string pred)))
                    nil t))
           (session (cdr (assoc choice table)))
           (running (and session (vterm-ai-resume--running-buffer session)))
           (cwd (and session (plist-get session :cwd))))
      (cond
       ((null session) (user-error "No session selected"))
       (running (pop-to-buffer running))
       ((not (file-directory-p cwd))
        (user-error "Directory no longer exists: %s" cwd))
       (t
        (require 'vterm)
        (let* ((command (vterm-ai-resume--command session))
               (buf (let ((default-directory (file-name-as-directory cwd)))
                      (vterm-other-window t))))
          ;; Give the shell time to start so the typed command is not lost.
          (run-at-time vterm-ai-resume-send-delay nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (vterm-send-string command)
                             (vterm-send-return)))))
          (message "Resuming: %s" command)))))))

(provide 'vterm-ai-resume)
;;; vterm-ai-resume.el ends here

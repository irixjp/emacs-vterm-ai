;;; vterm-ai-dashboard.el --- Dashboard UI for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a card-style dashboard showing all AI agent sessions.

;;; Code:

(require 'cl-lib)
(require 'vterm-ai-data)

(defvar vterm-ai-refresh-interval)

(defface vterm-ai-status-busy
  '((t :foreground "orange" :weight bold))
  "Face for busy agent instances."
  :group 'vterm-ai)

(defface vterm-ai-status-idle
  '((t :foreground "green" :weight bold))
  "Face for idle agent instances."
  :group 'vterm-ai)

(defface vterm-ai-status-asking
  '((t :foreground "red" :weight bold :inverse-video t))
  "Face for agent instances waiting for user input."
  :group 'vterm-ai)

(defface vterm-ai-status-done
  '((t :foreground "deep sky blue" :weight bold))
  "Face for agent instances that finished a turn the user has not acted on."
  :group 'vterm-ai)

(defface vterm-ai-status-error
  '((t :foreground "red" :weight bold))
  "Face for agent instances that reported an error."
  :group 'vterm-ai)

(defcustom vterm-ai-dashboard-tick-interval 10
  "Seconds between updates of the elapsed times, or nil to disable.
Each update rewrites only the elapsed-time labels, and only while the
dashboard is shown on a visible frame.  State changes redraw at once
regardless of this interval."
  :type '(choice (integer :tag "Seconds") (const :tag "Disabled" nil))
  :group 'vterm-ai)

(defcustom vterm-ai-dashboard-compact t
  "When non-nil, size each card by its state for small windows.
ASKING, DONE, and ERROR get details, BUSY gets what it is doing, and
other states get one line.  `v' toggles this in the dashboard."
  :type 'boolean
  :group 'vterm-ai)

(defcustom vterm-ai-dashboard-stale-after 1800
  "Seconds of IDLE after which a session collapses into a summary line.
nil keeps every session visible.  `i' toggles showing them."
  :type '(choice (integer :tag "Seconds") (const :tag "Never" nil))
  :group 'vterm-ai)

(defface vterm-ai-card-asking
  '((((background dark)) :background "#3a2f05")
    (t :background "#fff3cc"))
  "Background of cards waiting on the user."
  :group 'vterm-ai)

(defface vterm-ai-card-done
  '((((background dark)) :background "#0f2e1b")
    (t :background "#e4f4e8"))
  "Background of cards that finished a turn."
  :group 'vterm-ai)

(defvar-local vterm-ai-dashboard--show-stale nil
  "Non-nil to show sessions that have been IDLE for a long time.")

(defcustom vterm-ai-dashboard-osc-debounce 0.3
  "Seconds to gather OSC 7501 status changes before one redraw."
  :type 'number
  :group 'vterm-ai)

(defface vterm-ai-no-buffer
  '((t :foreground "gray50"))
  "Face for sessions with no linked vterm buffer."
  :group 'vterm-ai)

(defface vterm-ai-session-name
  '((t :weight bold))
  "Face for session name."
  :group 'vterm-ai)

(defface vterm-ai-label
  '((t :foreground "gray60"))
  "Face for field labels."
  :group 'vterm-ai)

(defface vterm-ai-separator
  '((t :foreground "gray40"))
  "Face for separators between sessions."
  :group 'vterm-ai)

(defface vterm-ai-help-key
  '((t :inherit vterm-ai-label :weight bold))
  "Face for key names in the keybinding hint line."
  :group 'vterm-ai)

(defconst vterm-ai-dashboard--keybinding-hint
  '((("RET" . "Open") ("o" . "Other Win") ("d" . "Dired") ("D" . "Detail"))
    (("g" . "Refresh") ("n/p" . "Next/Prev") ("q" . "Quit"))
    (("i" . "Quiet idle") ("v" . "Compact/Full")))
  "Rows of (KEY . DESCRIPTION) shown as a hint below the header line.
Each inner list is rendered on its own line, since vterm-ai is meant
to be used in narrow windows.")

(defvar vterm-ai-dashboard--sessions nil
  "Current list of sessions displayed in the dashboard.")

(defvar vterm-ai-dashboard--timer nil
  "Refresh timer for the dashboard.")

(defvar vterm-ai-dashboard--tick-timer nil
  "Timer that redraws elapsed times.")

(defvar vterm-ai-dashboard--osc-timer nil
  "Pending debounced redraw after OSC 7501 changes.")

(defvar vterm-ai-dashboard--osc-unknown-buffer nil
  "Non-nil when a changed vterm buffer belongs to no displayed session.")

(defun vterm-ai-dashboard--status-text (status)
  "Return display text for STATUS string."
  (cond
   ((equal status "waiting") "ASKING")
   ((equal status "busy") "BUSY")
   ((equal status "running") "RUNNING")
   ((equal status "idle") "IDLE")
   ((equal status "done") "DONE")
   ((equal status "error") "ERROR")
   (t (upcase (or status "?")))))

(defun vterm-ai-dashboard--status-face (status)
  "Return the face for STATUS string."
  (cond
   ((equal status "waiting") 'vterm-ai-status-asking)
   ((equal status "busy") 'vterm-ai-status-busy)
   ((equal status "running") 'vterm-ai-status-busy)
   ((equal status "idle") 'vterm-ai-status-idle)
   ((equal status "done") 'vterm-ai-status-done)
   ((equal status "error") 'vterm-ai-status-error)
   (t 'default)))

(defun vterm-ai-dashboard--status-sort-priority (status)
  "Return the display sort priority for STATUS.
Lower values are shown first: ASKING, DONE, IDLE, then BUSY/RUNNING,
with any other status shown last."
  (cond
   ((equal status "waiting") 0)
   ((equal status "done") 1)
   ((equal status "idle") 2)
   ((equal status "busy") 3)
   ((equal status "running") 3)
   (t 4)))

(defun vterm-ai-dashboard--format-elapsed (since)
  "Return elapsed time from SINCE as M:SS or H:MM:SS, or nil."
  (when since
    (let* ((secs (max 0 (floor (- (float-time) since))))
           (h (/ secs 3600))
           (m (/ (% secs 3600) 60))
           (s (% secs 60)))
      (if (> h 0)
          (format "%d:%02d:%02d" h m s)
        (format "%d:%02d" m s)))))

(defun vterm-ai-dashboard--status-label (status since)
  "Return \"[STATUS ELAPSED]\" text for STATUS that began at SINCE."
  (let ((elapsed (vterm-ai-dashboard--format-elapsed since)))
    (format "[%s%s]" (vterm-ai-dashboard--status-text status)
            (if elapsed (concat " " elapsed) ""))))

(defun vterm-ai-dashboard--label-string (status since face)
  "Return the status label for STATUS that began at SINCE, shown in FACE.
A label with an elapsed time carries SINCE and STATUS as text properties
so `vterm-ai-dashboard--update-elapsed' can rewrite it in place."
  (let ((label (propertize (vterm-ai-dashboard--status-label status since)
                           'face face)))
    (when since
      (add-text-properties 0 (length label)
                           (list 'vterm-ai-since since 'vterm-ai-status status)
                           label))
    label))

(defun vterm-ai-dashboard--update-elapsed ()
  "Rewrite only the elapsed-time labels in the current buffer.
Labels whose text is unchanged are left alone, so a tick that changes
nothing modifies nothing."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (save-excursion
      (while (setq pos (text-property-not-all pos (point-max) 'vterm-ai-since nil))
        (let* ((end (or (next-single-property-change pos 'vterm-ai-since)
                        (point-max)))
               (new (vterm-ai-dashboard--status-label
                     (get-text-property pos 'vterm-ai-status)
                     (get-text-property pos 'vterm-ai-since))))
          (if (string= new (buffer-substring-no-properties pos end))
              (setq pos end)
            (let ((props (text-properties-at pos)))
              (goto-char pos)
              (delete-region pos end)
              (insert (apply #'propertize new props))
              (setq pos (point)))))))))

(defun vterm-ai-dashboard--sort-sessions (sessions)
  "Return SESSIONS ordered ASKING, DONE, IDLE, BUSY, then everything else.
Sessions with the same status keep their relative order."
  (let ((indexed (let ((i -1))
                   (mapcar (lambda (s) (cons (cl-incf i) s)) sessions))))
    (mapcar #'cdr
            (sort indexed
                  (lambda (a b)
                    (let ((pa (vterm-ai-dashboard--status-sort-priority
                               (vterm-ai-session-status (cdr a))))
                          (pb (vterm-ai-dashboard--status-sort-priority
                               (vterm-ai-session-status (cdr b)))))
                      (if (= pa pb)
                          (< (car a) (car b))
                        (< pa pb))))))))

(defun vterm-ai-dashboard--session-at-point ()
  "Return the session at point, or nil."
  (get-text-property (point) 'vterm-ai-session))

(defun vterm-ai-dashboard--wrap-text (text width)
  "Wrap TEXT to WIDTH columns, return as a single string with newlines."
  (if (or (null text) (string-empty-p text))
      ""
    (with-temp-buffer
      (insert text)
      (let ((fill-column width))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defun vterm-ai-dashboard--mode-display (mode)
  "Return display string for permission MODE."
  (cond
   ((or (not (stringp mode)) (string-empty-p mode)) nil)
   ((equal mode "plan") "Plan")
   ((equal mode "acceptEdits") "Accept Edits")
   ((equal mode "bypassPermissions") "YOLO")
   ((equal mode "default") "Default")
   (t mode)))

(defun vterm-ai-dashboard--card-format (session)
  "Return how SESSION's card is drawn: `full', `detail', `busy', or `line'.
In the compact view, cards that need a look (ASKING, DONE, ERROR) get
details, BUSY gets what it is doing, and the rest get a single line."
  (let ((status (vterm-ai-session-status session)))
    (cond
     ((not vterm-ai-dashboard-compact) 'full)
     ((member status '("waiting" "done" "error")) 'detail)
     ((equal status "busy") 'busy)
     (t 'line))))

(defun vterm-ai-dashboard--stale-p (session)
  "Return non-nil if SESSION has been IDLE for `vterm-ai-dashboard-stale-after'.
A session with no known start of the state is never stale."
  (let ((since (vterm-ai-session-state-since session)))
    (and vterm-ai-dashboard-stale-after
         since
         (equal (vterm-ai-session-status session) "idle")
         (> (- (float-time) since) vterm-ai-dashboard-stale-after))))

(defun vterm-ai-dashboard--reply-lines (text)
  "Return the display lines of reply TEXT without Markdown scaffolding."
  (let (lines)
    (dolist (line (split-string (or text "") "[\n\r]+"))
      (let ((clean (string-trim
                    (replace-regexp-in-string "\\`[ \t]*#+[ \t]+" "" line))))
        (unless (or (string-empty-p clean)
                    (string-match-p "\\`\\(?:```\\|~~~\\)" clean))
          (push clean lines))))
    (nreverse lines)))

(defun vterm-ai-dashboard--insert-field (label lines width max-lines)
  "Insert LABEL and LINES wrapped to WIDTH, at most MAX-LINES screen lines.
LINES is a string or a list of strings.  Cut text ends with an ellipsis."
  (let* ((indent (make-string (length label) ?\s))
         (text-width (max 1 (- width (length label) 2)))
         (source (if (listp lines)
                     lines
                   (list (replace-regexp-in-string "[\n\r]+" " " lines))))
         (wrapped (cl-mapcan
                   (lambda (line)
                     (split-string (vterm-ai-dashboard--wrap-text line text-width)
                                   "\n" t))
                   source))
         (shown (seq-take wrapped max-lines)))
    (when (and shown (> (length wrapped) max-lines))
      (setcar (last shown) (concat (car (last shown)) " …")))
    (insert (propertize label 'face 'vterm-ai-label) (or (car shown) "") "\n")
    (dolist (line (cdr shown))
      (insert indent line "\n"))))

(defun vterm-ai-dashboard--render-session (session width)
  "Render SESSION as a card into the current buffer.
WIDTH is the available window width.  The card's shape follows
`vterm-ai-dashboard--card-format'."
  (let* ((shape (vterm-ai-dashboard--card-format session))
         (status (vterm-ai-session-status session))
         (has-buffer (vterm-ai-session-vterm-buffer session))
         (status-face (if has-buffer
                          (vterm-ai-dashboard--status-face status)
                        'vterm-ai-no-buffer))
         (title (or (vterm-ai-session-title session) ""))
         (cwd (abbreviate-file-name (or (vterm-ai-session-cwd session) "")))
         (model (or (vterm-ai-session-model session) ""))
         (mode-display (vterm-ai-dashboard--mode-display
                        (vterm-ai-session-mode session)))
         (prompt (or (vterm-ai-session-last-prompt session) ""))
         (activity (vterm-ai-session-activity session))
         (reply (vterm-ai-session-last-reply session))
         (start (point)))
    ;; Line 1: [STATUS ELAPSED] type: title {mode}
    (insert (vterm-ai-dashboard--label-string
             status (vterm-ai-session-state-since session) status-face)
            " "
            (propertize (or (vterm-ai-session-type session) "") 'face 'vterm-ai-label)
            ": "
            (propertize (if (string-empty-p title)
                            (or (vterm-ai-session-name session) "")
                          title)
                        'face 'vterm-ai-session-name)
            ;; A non-default permission mode must stay visible in every shape.
            (if (and mode-display (not (eq shape 'full))
                     (not (equal mode-display "Default")))
                (propertize (format " {%s}" mode-display) 'face 'vterm-ai-label)
              "")
            "\n")
    (pcase shape
      ('full
       (insert (propertize "Path: " 'face 'vterm-ai-label) cwd "\n")
       (unless (string-empty-p model)
         (insert (propertize "Model: " 'face 'vterm-ai-label) model "\n"))
       (when mode-display
         (insert (propertize "Mode: " 'face 'vterm-ai-label) mode-display "\n"))
       (when (and activity (not (string-empty-p activity)))
         (vterm-ai-dashboard--insert-field "Now: " activity width 2))
       (when (and reply (equal status "done"))
         (vterm-ai-dashboard--insert-field
          "Reply: " (vterm-ai-dashboard--reply-lines reply) width 3))
       (vterm-ai-dashboard--insert-field
        "Prompt: " (if (string-empty-p prompt) "(none)" prompt) width 3))
      ('detail
       (insert (propertize "Path: " 'face 'vterm-ai-label) cwd "\n")
       (when (and activity (not (string-empty-p activity))
                  (member status '("waiting" "error")))
         (vterm-ai-dashboard--insert-field "Now: " activity width 2))
       (when (and reply (equal status "done"))
         (vterm-ai-dashboard--insert-field
          "Reply: " (vterm-ai-dashboard--reply-lines reply) width 3))
       (unless (string-empty-p prompt)
         (vterm-ai-dashboard--insert-field "Prompt: " prompt width 1)))
      ('busy
       (when (and activity (not (string-empty-p activity)))
         (vterm-ai-dashboard--insert-field "Now: " activity width 1))))
    ;; Subagents reported as OSC 7501 child records
    (dolist (child (vterm-ai-session-children session))
      (let* ((child-status (vterm-ai-data-osc-status (plist-get child :state)))
             (label (or (plist-get child :title) (plist-get child :msg)
                        (plist-get child :id))))
        (insert "  └ "
                (vterm-ai-dashboard--label-string
                 child-status (plist-get child :since)
                 (vterm-ai-dashboard--status-face child-status))
                " "
                (truncate-string-to-width label (max 1 (- width 20)) nil nil "…")
                "\n")))
    ;; Tint the cards that want a look; existing foreground faces win.
    (let ((tint (pcase status
                  ("waiting" 'vterm-ai-card-asking)
                  ("done" 'vterm-ai-card-done))))
      (when tint
        (add-face-text-property start (point) tint t)))
    ;; Tag the entire region with the session
    (put-text-property start (point) 'vterm-ai-session session)))

(defun vterm-ai-dashboard--insert-keybinding-hint (sep-width)
  "Insert the keybinding hint lines, underlined by a rule of SEP-WIDTH."
  (dolist (row vterm-ai-dashboard--keybinding-hint)
    (insert (propertize
             (mapconcat (lambda (kv)
                          (concat (propertize (car kv) 'face 'vterm-ai-help-key)
                                  ":" (cdr kv)))
                        row
                        "  ")
             'face 'vterm-ai-label)
            "\n"))
  (insert (propertize (make-string sep-width ?─) 'face 'vterm-ai-separator)
          "\n"))

(defun vterm-ai-dashboard--render (sessions)
  "Render all SESSIONS into the current buffer."
  (let* ((inhibit-read-only t)
         (saved-session (vterm-ai-dashboard--session-at-point))
         (saved-line (line-number-at-pos))
         (win (or (get-buffer-window (current-buffer)) (selected-window)))
         (width (window-width win))
         (sep-width (max 1 (min width 80)))
         ;; erase-buffer collapses window-start; keep each window's top line.
         (window-lines (mapcar (lambda (w)
                                 (cons w (line-number-at-pos (window-start w))))
                               (get-buffer-window-list (current-buffer) nil t))))
    (erase-buffer)
    (vterm-ai-dashboard--insert-keybinding-hint sep-width)
    (if (null sessions)
        (insert (propertize "No active AI agent sessions found.\n"
                            'face 'vterm-ai-label))
      (let* ((stale (cl-remove-if-not #'vterm-ai-dashboard--stale-p sessions))
             (shown (if vterm-ai-dashboard--show-stale
                        sessions
                      (cl-remove-if #'vterm-ai-dashboard--stale-p sessions)))
             (prev-format nil))
        (dolist (session shown)
          ;; One-line cards in a row need no rule between them.
          (let ((shape (vterm-ai-dashboard--card-format session)))
            (when (and prev-format
                       (not (and (eq shape 'line) (eq prev-format 'line))))
              (insert (propertize (make-string sep-width ?─)
                                  'face 'vterm-ai-separator)
                      "\n"))
            (setq prev-format shape))
          ;; A single malformed session (e.g. a provider surfacing an
          ;; unexpected value from its data source) must not blank out
          ;; the entire dashboard for every other session.
          (condition-case err
              (vterm-ai-dashboard--render-session session width)
            (error
             (insert (propertize
                      (format "[ERROR] rendering session failed: %s\n"
                              (error-message-string err))
                      'face 'vterm-ai-status-asking)))))
        ;; Sessions quiet for a long time collapse into one line.
        (when stale
          (insert (propertize
                   (format (if vterm-ai-dashboard--show-stale
                               "… %d quiet idle shown (%dm+)  i:hide\n"
                             "… %d quiet idle hidden (%dm+)  i:show\n")
                           (length stale)
                           (/ vterm-ai-dashboard-stale-after 60))
                   'face 'vterm-ai-label)))))
    ;; Restore cursor position
    (goto-char (point-min))
    (if saved-session
        (let ((target-pid (vterm-ai-session-pid saved-session))
              found)
          (while (and (not found) (not (eobp)))
            (let ((s (get-text-property (point) 'vterm-ai-session)))
              (if (and s (equal (vterm-ai-session-pid s) target-pid))
                  (setq found t)
                (forward-line 1)))))
      (forward-line (1- (min saved-line (line-number-at-pos (point-max))))))
    (dolist (cell window-lines)
      (set-window-start (car cell)
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- (cdr cell)))
                          (point))
                        t))))

(defun vterm-ai-dashboard--update-header (sessions)
  "Update the header line based on SESSIONS."
  (let ((count-busy (cl-count-if
                     (lambda (s) (equal (vterm-ai-session-status s) "busy"))
                     sessions))
        (count-idle (cl-count-if
                     (lambda (s) (equal (vterm-ai-session-status s) "idle"))
                     sessions))
        (count-asking (cl-count-if
                       (lambda (s) (equal (vterm-ai-session-status s) "waiting"))
                       sessions))
        (count-done (cl-count-if
                     (lambda (s) (equal (vterm-ai-session-status s) "done"))
                     sessions))
        (count-total (length sessions)))
    (setq header-line-format
          (let* ((counts (format " AI Sessions: %d total | %d busy | %d idle | %d done"
                                 count-total count-busy count-idle count-done))
                 (base (if (> count-asking 0)
                           (format "%s | %s" counts
                                   (propertize (format "%d ASKING" count-asking)
                                               'face 'vterm-ai-status-asking))
                         counts)))
            (format "%s  [%ds]" base vterm-ai-refresh-interval)))))

(defun vterm-ai-dashboard--refresh ()
  "Refresh dashboard data asynchronously."
  (let ((dashboard-buf (current-buffer)))
    (vterm-ai-data--collect-async
     (lambda (sessions)
       (when (buffer-live-p dashboard-buf)
         (with-current-buffer dashboard-buf
           (setq vterm-ai-dashboard--sessions
                 (vterm-ai-dashboard--sort-sessions sessions))
           (vterm-ai-dashboard--render vterm-ai-dashboard--sessions)
           (vterm-ai-dashboard--update-header vterm-ai-dashboard--sessions)))))))

(defun vterm-ai-dashboard--relayout ()
  "Re-render the last collected sessions without re-collecting data.
Used to keep the layout (e.g. separator width) in sync with the
current window width when it changes."
  (vterm-ai-dashboard--render vterm-ai-dashboard--sessions)
  (vterm-ai-dashboard--update-header vterm-ai-dashboard--sessions))

(defun vterm-ai-dashboard--refresh-if-alive ()
  "Refresh dashboard if the buffer still exists."
  (let ((buf (get-buffer "*vterm-ai*")))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (vterm-ai-dashboard--refresh))
      (vterm-ai-dashboard--stop-timer))))

;;; --- OSC 7501 driven redraw ---

(defun vterm-ai-dashboard--on-osc-change (buffer)
  "Schedule one redraw for a reported status change in vterm BUFFER."
  (when (get-buffer "*vterm-ai*")
    (unless (cl-some (lambda (s) (eq (vterm-ai-session-vterm-buffer s) buffer))
                     vterm-ai-dashboard--sessions)
      (setq vterm-ai-dashboard--osc-unknown-buffer t))
    (unless (timerp vterm-ai-dashboard--osc-timer)
      (setq vterm-ai-dashboard--osc-timer
            (run-with-timer vterm-ai-dashboard-osc-debounce nil
                            #'vterm-ai-dashboard--osc-redraw)))))

(defun vterm-ai-dashboard--osc-redraw ()
  "Reapply OSC 7501 records to the displayed sessions and redraw.
A change in a buffer that no session owns yet means a new agent, so
also run a full collection to discover it."
  (setq vterm-ai-dashboard--osc-timer nil)
  (let ((buf (get-buffer "*vterm-ai*"))
        (unknown vterm-ai-dashboard--osc-unknown-buffer))
    (setq vterm-ai-dashboard--osc-unknown-buffer nil)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if unknown
            (vterm-ai-dashboard--refresh)
          (dolist (session vterm-ai-dashboard--sessions)
            (let ((before (vterm-ai-session-status session)))
              (vterm-ai-data-apply-osc session)
              ;; Just finished: read the reply now, not at the next collection.
              (when (and (equal (vterm-ai-session-status session) "done")
                         (not (equal before "done")))
                (vterm-ai-data-reenrich session))))
          (setq vterm-ai-dashboard--sessions
                (vterm-ai-dashboard--sort-sessions vterm-ai-dashboard--sessions))
          (vterm-ai-dashboard--relayout))))))

(defun vterm-ai-dashboard--tick ()
  "Advance elapsed times while the dashboard is on a visible frame."
  (let ((buf (get-buffer "*vterm-ai*")))
    (if (not (buffer-live-p buf))
        (vterm-ai-dashboard--stop-timer)
      (when (get-buffer-window buf 'visible)
        (with-current-buffer buf
          (vterm-ai-dashboard--update-elapsed))))))

(defun vterm-ai-dashboard--start-timer (interval)
  "Start refresh timer with INTERVAL seconds."
  (vterm-ai-dashboard--stop-timer)
  (setq vterm-ai-dashboard--timer
        (run-with-timer 0 interval #'vterm-ai-dashboard--refresh-if-alive))
  (when vterm-ai-dashboard-tick-interval
    (setq vterm-ai-dashboard--tick-timer
          (run-with-timer vterm-ai-dashboard-tick-interval
                          vterm-ai-dashboard-tick-interval
                          #'vterm-ai-dashboard--tick)))
  (add-hook 'vterm-ai-osc-change-functions #'vterm-ai-dashboard--on-osc-change))

(defun vterm-ai-dashboard--stop-timer ()
  "Stop the refresh, tick, and pending OSC redraw timers."
  (dolist (sym '(vterm-ai-dashboard--timer
                 vterm-ai-dashboard--tick-timer
                 vterm-ai-dashboard--osc-timer))
    (when (timerp (symbol-value sym))
      (cancel-timer (symbol-value sym)))
    (set sym nil))
  (remove-hook 'vterm-ai-osc-change-functions #'vterm-ai-dashboard--on-osc-change))

(defun vterm-ai-goto-buffer ()
  "Switch to the vterm buffer for the session at point."
  (interactive)
  (let* ((session (vterm-ai-dashboard--session-at-point))
         (buf (and session (vterm-ai-session-vterm-buffer session))))
    (if buf
        (switch-to-buffer buf)
      (message "No vterm buffer linked to this session"))))

(defun vterm-ai-goto-buffer-other-window ()
  "Open the vterm buffer for the session at point in another window."
  (interactive)
  (let* ((session (vterm-ai-dashboard--session-at-point))
         (buf (and session (vterm-ai-session-vterm-buffer session))))
    (if buf
        (switch-to-buffer-other-window buf)
      (message "No vterm buffer linked to this session"))))

(defun vterm-ai-open-dired ()
  "Open dired in the working directory of the session at point."
  (interactive)
  (let ((session (vterm-ai-dashboard--session-at-point)))
    (if (not session)
        (message "No session at point")
      (let ((dir (vterm-ai-session-cwd session)))
        (if (and dir (file-directory-p dir))
            (dired dir)
          (message "Directory not found: %s" dir))))))

(defun vterm-ai-show-detail ()
  "Show detailed transcript for the session at point."
  (interactive)
  (let ((session (vterm-ai-dashboard--session-at-point)))
    (if (not session)
        (message "No session at point")
      (let ((detail (vterm-ai-data--get-detail session))
            (buf (get-buffer-create "*vterm-ai-detail*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert detail))
          (goto-char (point-min))
          (special-mode)
          (local-set-key (kbd "q") (lambda () (interactive) (quit-window t))))
        (switch-to-buffer buf)))))

(defun vterm-ai-next-session ()
  "Move point to the next session."
  (interactive)
  (let ((current (vterm-ai-dashboard--session-at-point))
        found)
    (save-excursion
      (while (and (not found) (not (eobp)))
        (forward-line 1)
        (let ((s (get-text-property (point) 'vterm-ai-session)))
          (when (and s (not (eq s current)))
            (setq found (point))))))
    (when found (goto-char found))))

(defun vterm-ai-prev-session ()
  "Move point to the previous session."
  (interactive)
  (let ((current (vterm-ai-dashboard--session-at-point))
        found)
    (save-excursion
      (while (and (not (bobp))
                  (eq (get-text-property (point) 'vterm-ai-session) current))
        (forward-line -1))
      (while (and (not (bobp))
                  (not (get-text-property (point) 'vterm-ai-session)))
        (forward-line -1))
      (let ((prev (get-text-property (point) 'vterm-ai-session)))
        (when prev
          (while (and (not (bobp))
                      (eq (get-text-property (point) 'vterm-ai-session) prev))
            (forward-line -1))
          (unless (eq (get-text-property (point) 'vterm-ai-session) prev)
            (forward-line 1))
          (setq found (point)))))
    (when found (goto-char found))))

(defvar vterm-ai-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vterm-ai-goto-buffer)
    (define-key map (kbd "o") #'vterm-ai-goto-buffer-other-window)
    (define-key map (kbd "d") #'vterm-ai-open-dired)
    (define-key map (kbd "D") #'vterm-ai-show-detail)
    (define-key map (kbd "g") #'vterm-ai-dashboard-revert)
    (define-key map (kbd "n") #'vterm-ai-next-session)
    (define-key map (kbd "p") #'vterm-ai-prev-session)
    (define-key map (kbd "i") #'vterm-ai-dashboard-toggle-quiet-idle)
    (define-key map (kbd "v") #'vterm-ai-dashboard-toggle-compact)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `vterm-ai-dashboard-mode'.")

(defun vterm-ai-dashboard-toggle-quiet-idle ()
  "Show or hide sessions that have been IDLE for a long time."
  (interactive)
  (setq vterm-ai-dashboard--show-stale (not vterm-ai-dashboard--show-stale))
  (vterm-ai-dashboard--relayout))

(defun vterm-ai-dashboard-toggle-compact ()
  "Switch this dashboard between the compact and the full card view."
  (interactive)
  (setq-local vterm-ai-dashboard-compact (not vterm-ai-dashboard-compact))
  (vterm-ai-dashboard--relayout))

(defun vterm-ai-dashboard-revert ()
  "Manually refresh the dashboard."
  (interactive)
  (vterm-ai-dashboard--refresh))

(define-derived-mode vterm-ai-dashboard-mode special-mode "VtermAI"
  "Major mode for the vterm-ai dashboard."
  ;; The buffer is regenerated, never edited; undo records would only pile up.
  (buffer-disable-undo)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (vterm-ai-dashboard--refresh)))
  (add-hook 'kill-buffer-hook #'vterm-ai-dashboard--stop-timer nil t)
  (add-hook 'window-configuration-change-hook
            #'vterm-ai-dashboard--relayout nil t))

(provide 'vterm-ai-dashboard)
;;; vterm-ai-dashboard.el ends here

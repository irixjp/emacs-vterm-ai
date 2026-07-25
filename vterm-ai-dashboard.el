;;; vterm-ai-dashboard.el --- Dashboard UI for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a card-style dashboard showing all AI agent sessions.

;;; Code:

(require 'cl-lib)
(require 'vterm-ai-data)

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

(defvar vterm-ai-dashboard--sessions nil
  "Current list of sessions displayed in the dashboard.")

(defvar vterm-ai-dashboard--timer nil
  "Refresh timer for the dashboard.")

(defun vterm-ai-dashboard--status-text (status)
  "Return display text for STATUS string."
  (cond
   ((equal status "waiting") "ASKING")
   ((equal status "busy") "BUSY")
   ((equal status "running") "RUNNING")
   ((equal status "idle") "IDLE")
   (t (upcase (or status "?")))))

(defun vterm-ai-dashboard--status-face (status)
  "Return the face for STATUS string."
  (cond
   ((equal status "waiting") 'vterm-ai-status-asking)
   ((equal status "busy") 'vterm-ai-status-busy)
   ((equal status "running") 'vterm-ai-status-busy)
   ((equal status "idle") 'vterm-ai-status-idle)
   (t 'default)))

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
   ((or (null mode) (string-empty-p mode)) nil)
   ((equal mode "plan") "Plan")
   ((equal mode "acceptEdits") "Accept Edits")
   ((equal mode "bypassPermissions") "YOLO")
   ((equal mode "default") "Default")
   (t mode)))

(defun vterm-ai-dashboard--render-session (session width)
  "Render SESSION as a multi-line card into the current buffer.
WIDTH is the available window width."
  (let* ((status (vterm-ai-session-status session))
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
         (prompt-clean (replace-regexp-in-string "[\n\r]+" " " prompt))
         (prompt-indent (make-string (length "Prompt: ") ?\s))
         (text-width (max 40 (- width (length prompt-indent) 2)))
         (start (point)))
    ;; Line 1: [STATUS] type: title
    (insert (propertize (format "[%s]" (vterm-ai-dashboard--status-text status))
                        'face status-face)
            " "
            (propertize (or (vterm-ai-session-type session) "") 'face 'vterm-ai-label)
            ": "
            (propertize (if (string-empty-p title)
                            (or (vterm-ai-session-name session) "")
                          title)
                        'face 'vterm-ai-session-name)
            "\n")
    ;; Path
    (insert (propertize "Path: " 'face 'vterm-ai-label) cwd "\n")
    ;; Model
    (unless (string-empty-p model)
      (insert (propertize "Model: " 'face 'vterm-ai-label) model "\n"))
    ;; Mode (only when not normal)
    (when mode-display
      (insert (propertize "Mode: " 'face 'vterm-ai-label) mode-display "\n"))
    ;; Prompt (wrapped to ~3 lines)
    (if (string-empty-p prompt-clean)
        (insert (propertize "Prompt: " 'face 'vterm-ai-label)
                (propertize "(none)" 'face 'vterm-ai-label)
                "\n")
      (let* ((wrapped (vterm-ai-dashboard--wrap-text prompt-clean text-width))
             (lines (split-string wrapped "\n"))
             (lines (seq-take lines 3)))
        (insert (propertize "Prompt: " 'face 'vterm-ai-label))
        (insert (car lines) "\n")
        (dolist (line (cdr lines))
          (insert prompt-indent line "\n"))))
    ;; Tag the entire region with the session
    (put-text-property start (point) 'vterm-ai-session session)))

(defun vterm-ai-dashboard--render (sessions)
  "Render all SESSIONS into the current buffer."
  (let ((inhibit-read-only t)
        (saved-session (vterm-ai-dashboard--session-at-point))
        (saved-line (line-number-at-pos))
        (width (max 80 (window-width))))
    (erase-buffer)
    (if (null sessions)
        (insert (propertize "No active AI agent sessions found.\n"
                            'face 'vterm-ai-label))
      (let ((first t))
        (dolist (session sessions)
          (if first
              (setq first nil)
            (insert (propertize (make-string (min width 80) ?─)
                                'face 'vterm-ai-separator)
                    "\n"))
          (vterm-ai-dashboard--render-session session width))))
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
      (forward-line (1- (min saved-line (line-number-at-pos (point-max))))))))

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
        (count-total (length sessions)))
    (setq header-line-format
          (if (> count-asking 0)
              (format " AI Sessions: %d total | %d busy | %d idle | %s"
                      count-total count-busy count-idle
                      (propertize (format "%d ASKING" count-asking)
                                  'face 'vterm-ai-status-asking))
            (format " AI Sessions: %d total | %d busy | %d idle"
                    count-total count-busy count-idle)))))

(defun vterm-ai-dashboard--refresh ()
  "Refresh dashboard data asynchronously."
  (let ((dashboard-buf (current-buffer)))
    (vterm-ai-data--collect-async
     (lambda (sessions)
       (when (buffer-live-p dashboard-buf)
         (with-current-buffer dashboard-buf
           (setq vterm-ai-dashboard--sessions sessions)
           (vterm-ai-dashboard--render sessions)
           (vterm-ai-dashboard--update-header sessions)))))))

(defun vterm-ai-dashboard--refresh-if-alive ()
  "Refresh dashboard if the buffer still exists."
  (let ((buf (get-buffer "*vterm-ai*")))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (vterm-ai-dashboard--refresh))
      (vterm-ai-dashboard--stop-timer))))

(defun vterm-ai-dashboard--start-timer (interval)
  "Start refresh timer with INTERVAL seconds."
  (vterm-ai-dashboard--stop-timer)
  (setq vterm-ai-dashboard--timer
        (run-with-timer 0 interval #'vterm-ai-dashboard--refresh-if-alive)))

(defun vterm-ai-dashboard--stop-timer ()
  "Stop the refresh timer."
  (when vterm-ai-dashboard--timer
    (cancel-timer vterm-ai-dashboard--timer)
    (setq vterm-ai-dashboard--timer nil)))

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
        (display-buffer buf '(display-buffer-below-selected
                              (window-height . 0.4)))))))

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
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `vterm-ai-dashboard-mode'.")

(defun vterm-ai-dashboard-revert ()
  "Manually refresh the dashboard."
  (interactive)
  (vterm-ai-dashboard--refresh))

(define-derived-mode vterm-ai-dashboard-mode special-mode "VtermAI"
  "Major mode for the vterm-ai dashboard."
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (vterm-ai-dashboard--refresh)))
  (add-hook 'kill-buffer-hook #'vterm-ai-dashboard--stop-timer nil t))

(provide 'vterm-ai-dashboard)
;;; vterm-ai-dashboard.el ends here

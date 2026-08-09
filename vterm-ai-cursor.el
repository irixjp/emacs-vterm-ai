;;; vterm-ai-cursor.el --- Cursor provider for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the Cursor provider: async process detection via ps/lsof
;; and status extraction from vterm buffer names.
;; Requires Cursor's status-indicators setting to be enabled.

;;; Code:

(require 'cl-lib)
(require 'vterm-ai-data)

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

;;; --- Enrich ---

(defun vterm-ai-cursor-enrich (session)
  "Enrich SESSION with status and title from the vterm buffer name."
  (let ((buf (vterm-ai-session-vterm-buffer session)))
    (when (and buf (buffer-live-p buf))
      (let ((parsed (vterm-ai-cursor--parse-buffer-name (buffer-name buf))))
        (when parsed
          (setf (vterm-ai-session-status session) (car parsed))
          (let ((title (cdr parsed)))
            (when (string-match "\\`vterm: \\(.+\\)" title)
              (setq title (match-string 1 title)))
            (setf (vterm-ai-session-title session) title))))))
  (setf (vterm-ai-session-last-prompt session) "(not available)"))

;;; --- Detail ---

(defun vterm-ai-cursor-detail (session)
  "Return a detailed string for SESSION."
  (format "Session: %s\nStatus:  %s\nCWD:     %s\nTitle:   %s\nPID:     %d\n"
          (or (vterm-ai-session-name session) "cursor")
          (or (vterm-ai-session-status session) "unknown")
          (or (vterm-ai-session-cwd session) "N/A")
          (or (vterm-ai-session-title session) "N/A")
          (vterm-ai-session-pid session)))

;;; --- Auto-register ---

(vterm-ai-register-provider
 '(:name "cursor"
   :get-sessions-async vterm-ai-cursor--get-sessions-async
   :enrich vterm-ai-cursor-enrich
   :detail vterm-ai-cursor-detail))

(provide 'vterm-ai-cursor)
;;; vterm-ai-cursor.el ends here

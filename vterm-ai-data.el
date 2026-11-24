;;; vterm-ai-data.el --- Data collection for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Common data layer: session struct, provider registry, process-tree
;; walking, vterm buffer matching, and async collect with generation
;; control to prevent stale results and process accumulation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vterm-ai-osc)

(cl-defstruct vterm-ai-session
  type
  pid
  session-id
  cwd
  name
  status
  started-at
  updated-at
  vterm-buffer
  shell-pid
  title
  last-prompt
  model
  mode
  ;; Filled from OSC 7501 reports (see `vterm-ai-data-apply-osc').
  activity
  blocked-kind
  children
  state-since
  ;; Where status came from: "osc", "agents", "hook", "title", or nil.
  status-source)

;;; --- Provider registry ---

(defvar vterm-ai-providers nil
  "List of registered provider plists.
Each plist has keys :name :get-sessions-async :enrich :detail.")

(defun vterm-ai-register-provider (provider)
  "Register PROVIDER (a plist with :name :get-sessions-async :enrich :detail)."
  (vterm-ai-unregister-provider (plist-get provider :name))
  (push provider vterm-ai-providers))

(defun vterm-ai-unregister-provider (name)
  "Remove the provider named NAME from the registry."
  (setq vterm-ai-providers
        (cl-remove-if (lambda (p) (equal (plist-get p :name) name))
                      vterm-ai-providers)))

(defun vterm-ai-data--find-provider (type)
  "Find the provider plist for TYPE."
  (cl-find-if (lambda (p) (equal (plist-get p :name) type))
              vterm-ai-providers))

;;; --- Process tree & vterm matching ---

(defun vterm-ai-data--get-ppid-table ()
  "Return a hash table mapping PID -> PPID from `ps'."
  (let ((table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (call-process "ps" nil t nil "-eo" "pid,ppid")
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (when (looking-at "\\s-*\\([0-9]+\\)\\s-+\\([0-9]+\\)")
          (puthash (string-to-number (match-string 1))
                   (string-to-number (match-string 2))
                   table))
        (forward-line 1)))
    table))

(defun vterm-ai-data--get-vterm-buffers ()
  "Return a hash table mapping shell-PID -> vterm buffer."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (eq major-mode 'vterm-mode)
                     (boundp 'vterm--process)
                     vterm--process
                     (process-live-p vterm--process))
            (puthash (process-id vterm--process) buf table)))))
    table))

(defun vterm-ai-data--find-vterm-buffer (agent-pid ppid-table vterm-table)
  "Walk PPID chain from AGENT-PID to find a matching vterm buffer."
  (let ((pid agent-pid)
        (max-depth 5)
        (depth 0)
        result)
    (while (and pid (not result) (< depth max-depth))
      (let ((ppid (gethash pid ppid-table)))
        (when ppid
          (let ((buf (gethash ppid vterm-table)))
            (when buf
              (setq result buf))))
        (setq pid ppid)
        (cl-incf depth)))
    result))

;;; --- Async collect with generation control ---

(defvar vterm-ai-data--collect-generation 0
  "Monotonically increasing counter.
Incremented on each collect-async call.  Sentinels check their
captured generation against this value and discard stale results.")

(defvar vterm-ai-data--collect-processes nil
  "List of currently running async discovery processes.
Killed when a new collect-async starts.")

(defun vterm-ai-data--kill-pending ()
  "Kill all pending collection processes and their buffers."
  (dolist (proc vterm-ai-data--collect-processes)
    (when (process-live-p proc)
      (delete-process proc))
    (let ((buf (process-buffer proc)))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf))))
  (setq vterm-ai-data--collect-processes nil))

(defun vterm-ai-data--collect-async (callback)
  "Collect sessions from all providers asynchronously.
Call CALLBACK with the session list when all providers finish.
If a previous collection is still running, kill it first.
Stale results from killed processes are discarded via generation check."
  (vterm-ai-data--kill-pending)
  (cl-incf vterm-ai-data--collect-generation)
  (let* ((my-gen vterm-ai-data--collect-generation)
         (providers vterm-ai-providers)
         (pending (length providers))
         (all-results nil))
    (if (zerop pending)
        (funcall callback nil)
      (dolist (provider providers)
        (let ((async-fn (plist-get provider :get-sessions-async))
              (type (plist-get provider :name)))
          (let ((proc
                 (funcall async-fn
                          (lambda (agents)
                            (when (= my-gen vterm-ai-data--collect-generation)
                              (push (cons type agents) all-results)
                              (cl-decf pending)
                              (when (zerop pending)
                                (vterm-ai-data--finalize
                                 all-results my-gen callback)))))))
            (cond
             ((processp proc) (push proc vterm-ai-data--collect-processes))
             ((listp proc)
              (dolist (p proc)
                (when (processp p)
                  (push p vterm-ai-data--collect-processes)))))))))))

(defun vterm-ai-data--finalize (all-results my-gen callback)
  "Build sessions from ALL-RESULTS and call CALLBACK.
MY-GEN is checked against the current generation to discard stale data."
  (when (= my-gen vterm-ai-data--collect-generation)
    (let* ((ppid-table (vterm-ai-data--get-ppid-table))
           (vterm-table (vterm-ai-data--get-vterm-buffers))
           sessions)
      (dolist (pair all-results)
        (let* ((type (car pair))
               (agents (cdr pair))
               (provider (vterm-ai-data--find-provider type))
               (enrich-fn (and provider (plist-get provider :enrich))))
          (dolist (agent (or agents nil))
            (let* ((pid (alist-get 'pid agent))
                   (session (make-vterm-ai-session
                             :type type
                             :pid pid
                             :session-id (alist-get 'sessionId agent)
                             :cwd (alist-get 'cwd agent)
                             :name (or (alist-get 'name agent) "")
                             :status (or (alist-get 'status agent) "unknown")
                             :started-at (alist-get 'startedAt agent)
                             :updated-at (alist-get 'updatedAt agent)
                             :status-source (alist-get 'statusSource agent)
                             :vterm-buffer (vterm-ai-data--find-vterm-buffer
                                            pid ppid-table vterm-table))))
              (when enrich-fn
                ;; A single provider's enrich function throwing (e.g. on
                ;; unexpected data from its own data source) must not
                ;; abort collection for every other session.
                (condition-case err
                    (funcall enrich-fn session)
                  (error
                   (message "vterm-ai: %s enrich failed for %s: %s"
                            type (vterm-ai-session-cwd session)
                            (error-message-string err)))))
              (vterm-ai-data-apply-osc session)
              (push session sessions)))))
      (setq vterm-ai-data--collect-processes nil)
      (funcall callback (nreverse sessions)))))

;;; --- OSC 7501 overlay ---

(defconst vterm-ai-data--osc-apps
  '(("claude" . "claude-code") ("codex" . "codex"))
  "Provider type -> the `app' its agent reports over OSC 7501.")

(defun vterm-ai-data-osc-status (state)
  "Map an OSC 7501 STATE to a dashboard status string."
  (pcase state
    ("working" "busy")
    ("blocked" "waiting")
    ("done" "done")
    ("idle" "idle")
    ("error" "error")
    (_ "running")))

(defun vterm-ai-data-apply-osc (session)
  "Overlay the OSC 7501 report of SESSION's vterm buffer onto SESSION.
Applied only when the reporting app matches the provider, so a buffer
that switched agents, or a crashed agent that never cleared its record,
cannot leak a stale state.  Safe to call repeatedly."
  (let* ((buf (vterm-ai-session-vterm-buffer session))
         (app (cdr (assoc (vterm-ai-session-type session) vterm-ai-data--osc-apps)))
         (root (and app (vterm-ai-osc-state buf))))
    (cond
     ((and root (equal (plist-get root :app) app))
      (setf (vterm-ai-session-status session)
            (vterm-ai-data-osc-status (plist-get root :state))
            (vterm-ai-session-activity session) (plist-get root :msg)
            (vterm-ai-session-blocked-kind session) (plist-get root :kind)
            (vterm-ai-session-state-since session) (plist-get root :since)
            (vterm-ai-session-children session) (vterm-ai-osc-children buf)
            (vterm-ai-session-status-source session) "osc"))
     ;; The record was cleared (agent exiting) since the last collection.
     ((equal (vterm-ai-session-status-source session) "osc")
      (setf (vterm-ai-session-status session) "running"
            (vterm-ai-session-activity session) nil
            (vterm-ai-session-blocked-kind session) nil
            (vterm-ai-session-state-since session) nil
            (vterm-ai-session-children session) nil
            (vterm-ai-session-status-source session) nil)))))

;;; --- Detail dispatch ---

(defun vterm-ai-data--get-detail (session)
  "Return detail string for SESSION by dispatching to its provider."
  (let* ((type (vterm-ai-session-type session))
         (provider (vterm-ai-data--find-provider type))
         (detail-fn (and provider (plist-get provider :detail))))
    (concat
     (format "Status source: %s\n"
             (or (vterm-ai-session-status-source session) "none"))
     (if detail-fn
         (funcall detail-fn session)
       (format "No detail available for provider: %s" type)))))

(provide 'vterm-ai-data)
;;; vterm-ai-data.el ends here

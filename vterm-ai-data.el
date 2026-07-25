;;; vterm-ai-data.el --- Data collection for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Common data layer: session struct, provider registry, process-tree
;; walking, vterm buffer matching, and async collect with generation
;; control to prevent stale results and process accumulation.

;;; Code:

(require 'cl-lib)
(require 'json)

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
  mode)

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
            (when (processp proc)
              (push proc vterm-ai-data--collect-processes))))))))

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
                             :vterm-buffer (vterm-ai-data--find-vterm-buffer
                                            pid ppid-table vterm-table))))
              (when enrich-fn
                (funcall enrich-fn session))
              (push session sessions)))))
      (setq vterm-ai-data--collect-processes nil)
      (funcall callback (nreverse sessions)))))

;;; --- Detail dispatch ---

(defun vterm-ai-data--get-detail (session)
  "Return detail string for SESSION by dispatching to its provider."
  (let* ((type (vterm-ai-session-type session))
         (provider (vterm-ai-data--find-provider type))
         (detail-fn (and provider (plist-get provider :detail))))
    (if detail-fn
        (funcall detail-fn session)
      (format "No detail available for provider: %s" type))))

(provide 'vterm-ai-data)
;;; vterm-ai-data.el ends here

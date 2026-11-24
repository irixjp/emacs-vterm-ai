;;; vterm-ai.el --- Dashboard for Claude Code instances in vterm  -*- lexical-binding: t; -*-

;; Author: vterm-ai contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (vterm "0.0.1"))
;; Keywords: processes, terminals, tools
;; URL: https://github.com/irixjp/emacs-vterm-ai

;;; Commentary:
;; Provides a dashboard buffer to monitor multiple Claude Code instances
;; running in Emacs vterm buffers.  Shows session status (busy/idle),
;; AI-generated titles, current prompts, and allows jumping to vterm buffers.
;;
;; Usage: M-x vterm-ai

;;; Code:

(require 'vterm-ai-data)
(require 'vterm-ai-dashboard)
(require 'vterm-ai-resume)

(defgroup vterm-ai nil
  "Dashboard for Claude Code instances in vterm."
  :group 'tools
  :prefix "vterm-ai-")

(defcustom vterm-ai-enabled-providers '(claude)
  "List of providers to enable.
Available providers: claude, codex, cursor, opencode."
  :type '(repeat (choice (const claude) (const codex) (const cursor) (const opencode)))
  :group 'vterm-ai)

(defcustom vterm-ai-refresh-interval 30
  "Refresh interval in seconds for the dashboard."
  :type 'integer
  :group 'vterm-ai)

(defcustom vterm-ai-osc-enable t
  "When non-nil, `vterm-ai' turns on `vterm-ai-osc-mode'.
Claude Code probes for OSC 7501 support only at startup, so sessions
started before the mode is on keep using hooks and titles.  Turn the
mode on in your init file to cover every session."
  :type 'boolean
  :group 'vterm-ai)

(defun vterm-ai--load-providers ()
  "Load provider modules listed in `vterm-ai-enabled-providers'."
  (dolist (provider vterm-ai-enabled-providers)
    (require (intern (format "vterm-ai-%s" provider)))))

;;;###autoload
(defun vterm-ai ()
  "Open the vterm-ai dashboard."
  (interactive)
  (vterm-ai--load-providers)
  (when (and vterm-ai-osc-enable (not vterm-ai-osc-mode))
    (vterm-ai-osc-mode 1))
  (let ((buf (get-buffer-create "*vterm-ai*")))
    (with-current-buffer buf
      (unless (eq major-mode 'vterm-ai-dashboard-mode)
        (vterm-ai-dashboard-mode))
      (vterm-ai-dashboard--start-timer vterm-ai-refresh-interval))
    (switch-to-buffer buf)))

(provide 'vterm-ai)
;;; vterm-ai.el ends here

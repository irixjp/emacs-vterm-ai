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
(require 'vterm-ai-claude)
(require 'vterm-ai-dashboard)

(defgroup vterm-ai nil
  "Dashboard for Claude Code instances in vterm."
  :group 'tools
  :prefix "vterm-ai-")

(defcustom vterm-ai-enable-codex nil
  "When non-nil, enable the Codex provider (experimental)."
  :type 'boolean
  :group 'vterm-ai)

(when vterm-ai-enable-codex
  (require 'vterm-ai-codex))

(defcustom vterm-ai-refresh-interval 10
  "Refresh interval in seconds for the dashboard."
  :type 'integer
  :group 'vterm-ai)

;;;###autoload
(defun vterm-ai ()
  "Open the vterm-ai dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*vterm-ai*")))
    (with-current-buffer buf
      (unless (eq major-mode 'vterm-ai-dashboard-mode)
        (vterm-ai-dashboard-mode))
      (vterm-ai-dashboard--start-timer vterm-ai-refresh-interval))
    (switch-to-buffer buf)))

(provide 'vterm-ai)
;;; vterm-ai.el ends here

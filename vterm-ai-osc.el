;;; vterm-ai-osc.el --- OSC 7501 program status for vterm-ai  -*- lexical-binding: t; -*-

;;; Commentary:
;; Agents such as Claude Code and Codex report their own state with the
;; Program Status Protocol (OSC 7501): idle / working / done / blocked /
;; error, an optional reason, a short message, and child records for
;; subagents.  Claude only reports after the terminal answers its
;; `ESC ] 7501 ; ?' probe, which vterm (libvterm) does not do.
;;
;; `vterm-ai-osc-mode' adds :before advice to `vterm--filter'.  The advice
;; answers the probe ahead of the DA1 reply that libvterm sends for the
;; query that follows it, and keeps the latest report per vterm buffer.
;; The output handed to vterm is never modified.
;;
;; Wire grammar follows Orca's implementation
;; (src/shared/program-status/protocol.ts in stablyai/orca).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar vterm-exit-functions)
(defvar vterm-ai-osc-mode)

(defgroup vterm-ai-osc nil
  "OSC 7501 program status for vterm-ai."
  :group 'vterm-ai
  :prefix "vterm-ai-osc-")

(defconst vterm-ai-osc--query-reply
  "\e]7501;?:states=idle,working,done,blocked,error:kinds=permission,question,auth\e\\"
  "Reply to the probe, listing the states and kinds this terminal accepts.")

(defconst vterm-ai-osc--states '("idle" "working" "done" "blocked" "error"))
(defconst vterm-ai-osc--kinds '("permission" "question" "auth"))

(defconst vterm-ai-osc--max-body-bytes 4087
  "The spec caps a sequence at 4096 bytes; the prefix and ST take 9.")
(defconst vterm-ai-osc--max-msg-decoded 2048)
(defconst vterm-ai-osc--max-title-decoded 192)
(defconst vterm-ai-osc--max-id-length 128)
(defconst vterm-ai-osc--max-id-depth 8)

(defcustom vterm-ai-osc-text-max-length 400
  "Messages and titles longer than this are cut for display."
  :type 'integer
  :group 'vterm-ai-osc)

(defconst vterm-ai-osc--seq-re "\e]7501;\\([^\a\e]*\\)\\(?:\a\\|\e\\\\\\)"
  "A complete OSC 7501 sequence ended by BEL or ST.")

(defconst vterm-ai-osc--prefix-tail-re
  "\e\\(?:]\\(?:7\\(?:5\\(?:0\\(?:1;?\\)?\\)?\\)?\\)?\\)?\\'"
  "A chunk ending in the first bytes of the OSC 7501 prefix.")

(defconst vterm-ai-osc--open-seq-re "\e]7501;[^\a\e]*\e?\\'"
  "A chunk ending inside an OSC 7501 body or between ESC and backslash.")

(defconst vterm-ai-osc--invisible-re
  "[\u200B-\u200F\u202A-\u202E\u2060-\u2069\uFEFF]"
  "Formatting characters that could disguise displayed text.")

(defvar vterm-ai-osc-change-functions nil
  "Abnormal hook run with the vterm buffer whose reported status changed.")

(defvar-local vterm-ai-osc--pending nil
  "Unfinished OSC 7501 bytes carried over from the previous chunk.")

(defvar-local vterm-ai-osc--root nil
  "Plist of the program's own record: :state :kind :msg :title :app :since.")

(defvar-local vterm-ai-osc--children nil
  "Alist (ID . PLIST) of child records in arrival order.")

;;; --- Parsing ---

(defun vterm-ai-osc--decode-text (encoded max-decoded)
  "Decode base64 UTF-8 ENCODED, or return :invalid.
Text over MAX-DECODED bytes or containing control characters is invalid."
  (let ((unpadded (replace-regexp-in-string "=+\\'" "" encoded)))
    (if (= (% (length unpadded) 4) 1)
        :invalid
      (let ((bytes (condition-case nil
                       (base64-decode-string
                        (concat unpadded
                                (make-string (% (- 4 (% (length unpadded) 4)) 4) ?=)))
                     (error nil))))
        (if (or (null bytes) (> (length bytes) max-decoded))
            :invalid
          (let ((text (decode-coding-string bytes 'utf-8-unix t)))
            (if (or (string-match-p "[[:cntrl:]\u007f-\u009f]" text)
                    (cl-some (lambda (c) (>= c #x3fff80)) text))
                :invalid
              text)))))))

(defun vterm-ai-osc--valid-id-p (id)
  "Return non-nil if ID is a valid record id."
  (let ((segments (split-string id "/")))
    (and (<= (length id) vterm-ai-osc--max-id-length)
         (<= (length segments) vterm-ai-osc--max-id-depth)
         (cl-every (lambda (s) (string-match-p "\\`[A-Za-z0-9_.+-]\\{1,32\\}\\'" s))
                   segments))))

(defun vterm-ai-osc-parse-body (body)
  "Parse BODY, the text between `ESC ] 7501 ;' and the terminator.
Return (query), (report . PLIST), or nil when the body must be ignored.
PLIST has :id (nil for the root record) :state and, when present,
:kind :progress :app :msg :title."
  (cond
   ((> (string-bytes body) vterm-ai-osc--max-body-bytes) nil)
   ((string= (string-trim body) "?") '(query))
   (t
    (let (pairs id msg title invalid)
      (dolist (pair (split-string body ":"))
        (let ((eq (string-search "=" pair)))
          (when (and eq (not invalid))
            (let ((key (string-trim (substring pair 0 eq)))
                  (val (string-trim (substring pair (1+ eq)))))
              (cond
               ;; A malformed id voids the report so it cannot overwrite the root.
               ((equal key "id")
                (if (vterm-ai-osc--valid-id-p val)
                    (setq id val)
                  (setq invalid t)))
               ((not (and (string-match-p "\\`[a-z]+\\'" key)
                          (string-match-p "\\`[A-Za-z0-9_.,+/=-]*\\'" val))))
               ((member key '("msg" "title"))
                (let ((text (vterm-ai-osc--decode-text
                             val (if (equal key "msg")
                                     vterm-ai-osc--max-msg-decoded
                                   vterm-ai-osc--max-title-decoded))))
                  (cond ((eq text :invalid) (setq invalid t))
                        ((equal key "msg") (setq msg text))
                        (t (setq title text)))))
               (t (push (cons key val) pairs)))))))
      (let ((state (cdr (assoc "state" pairs)))
            (kind (cdr (assoc "kind" pairs)))
            (progress (cdr (assoc "progress" pairs)))
            (app (cdr (assoc "app" pairs))))
        (when (and (not invalid)
                   state
                   (or (equal state "clear") (member state vterm-ai-osc--states)))
          (cons 'report
                (append
                 (list :id id :state state)
                 (when (and (equal state "blocked") (member kind vterm-ai-osc--kinds))
                   (list :kind kind))
                 (when (and (member state '("working" "blocked"))
                            progress
                            (string-match "\\`0*\\([0-9]\\{1,3\\}\\)\\'" progress)
                            (<= (string-to-number (match-string 1 progress)) 100))
                   (list :progress (string-to-number (match-string 1 progress))))
                 (when (and app (string-match-p "\\`[A-Za-z0-9_.+-]\\{1,32\\}\\'" app))
                   (list :app app))
                 (when msg (list :msg msg))
                 (when title (list :title title))))))))))

(defun vterm-ai-osc-display-text (text)
  "Return TEXT made safe and bounded for display, or nil when empty."
  (when text
    (let ((clean (string-trim (replace-regexp-in-string
                               vterm-ai-osc--invisible-re "" text))))
      (cond
       ((string-empty-p clean) nil)
       ((> (length clean) vterm-ai-osc-text-max-length)
        (concat (substring clean 0 (1- vterm-ai-osc-text-max-length)) "…"))
       (t clean)))))

;;; --- Per-buffer records ---

(defun vterm-ai-osc--record (report old now)
  "Build a record from REPORT, keeping OLD's :since while the state holds.
NOW is the time of the report."
  (let ((state (plist-get report :state)))
    (list :state state
          :kind (plist-get report :kind)
          :progress (plist-get report :progress)
          :app (or (plist-get report :app) (plist-get old :app))
          :msg (vterm-ai-osc-display-text (plist-get report :msg))
          :title (or (vterm-ai-osc-display-text (plist-get report :title))
                     (plist-get old :title))
          :since (if (and old (equal (plist-get old :state) state))
                     (plist-get old :since)
                   now))))

(defun vterm-ai-osc--same-p (a b)
  "Return non-nil when records A and B would display the same."
  (and a b
       (cl-every (lambda (k) (equal (plist-get a k) (plist-get b k)))
                 '(:state :kind :progress :msg :title))))

(defun vterm-ai-osc--apply-report (report)
  "Store REPORT in the current buffer.  Return non-nil if anything changed."
  (let ((id (plist-get report :id))
        (state (plist-get report :state))
        (now (float-time)))
    (if (null id)
        (cond
         ((equal state "clear")
          (when (or vterm-ai-osc--root vterm-ai-osc--children)
            (setq vterm-ai-osc--root nil
                  vterm-ai-osc--children nil)
            t))
         (t
          (let ((old vterm-ai-osc--root))
            (setq vterm-ai-osc--root (vterm-ai-osc--record report old now))
            ;; idle follows a start or an interrupt; children it left behind are stale.
            (when (and (equal state "idle") vterm-ai-osc--children)
              (setq vterm-ai-osc--children nil
                    old nil))
            (not (vterm-ai-osc--same-p old vterm-ai-osc--root)))))
      (let* ((cell (assoc id vterm-ai-osc--children))
             (old (cdr cell)))
        (if (equal state "clear")
            (when cell
              (setq vterm-ai-osc--children (delq cell vterm-ai-osc--children))
              t)
          (let ((new (vterm-ai-osc--record report old now)))
            (if cell
                (setcdr cell new)
              (setq vterm-ai-osc--children
                    (append vterm-ai-osc--children (list (cons id new)))))
            (not (vterm-ai-osc--same-p old new))))))))

(defun vterm-ai-osc--consume (process text)
  "Handle every OSC 7501 sequence in TEXT for PROCESS in the current buffer.
Keep an unfinished trailing sequence for the next chunk."
  (let ((start 0)
        changed)
    (while (string-match vterm-ai-osc--seq-re text start)
      (let ((body (match-string 1 text)))
        (setq start (match-end 0))
        (pcase (vterm-ai-osc-parse-body body)
          (`(query)
           (when (and process vterm-ai-osc-mode)
             (process-send-string process vterm-ai-osc--query-reply)))
          (`(report . ,report)
           (when (vterm-ai-osc--apply-report report)
             (setq changed t))))))
    (setq vterm-ai-osc--pending
          (let ((rest (cond
                       ((and (string-search "\e]7501;" text start)
                             (string-match vterm-ai-osc--open-seq-re text start))
                        (substring text (match-beginning 0)))
                       ((string-match vterm-ai-osc--prefix-tail-re text
                                      (max start (- (length text) 7)))
                        (substring text (match-beginning 0))))))
            ;; An open body past the cap can never complete.
            (and rest (<= (string-bytes rest) (+ vterm-ai-osc--max-body-bytes 9)) rest)))
    (when changed
      (run-hook-with-args 'vterm-ai-osc-change-functions (current-buffer)))))

(defun vterm-ai-osc--scan (process input)
  "Advice for `vterm--filter': read OSC 7501 from PROCESS's INPUT."
  (condition-case err
      (let ((buf (process-buffer process)))
        (when (and (buffer-live-p buf)
                   (or (buffer-local-value 'vterm-ai-osc--pending buf)
                       (string-search "\e]7501" input)
                       (string-search "\e" input (max 0 (- (length input) 7)))))
          (with-current-buffer buf
            (vterm-ai-osc--consume process
                                   (if vterm-ai-osc--pending
                                       (concat vterm-ai-osc--pending input)
                                     input)))))
    (error
     (message "vterm-ai-osc: %s" (error-message-string err)))))

(defun vterm-ai-osc--on-exit (buffer _event)
  "Forget the records of BUFFER when its process exits."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (or vterm-ai-osc--root vterm-ai-osc--children)
        (setq vterm-ai-osc--root nil
              vterm-ai-osc--children nil
              vterm-ai-osc--pending nil)
        (run-hook-with-args 'vterm-ai-osc-change-functions buffer)))))

;;; --- Public API ---

(defun vterm-ai-osc-state (buffer)
  "Return the root record plist reported in BUFFER, or nil."
  (and (buffer-live-p buffer)
       (buffer-local-value 'vterm-ai-osc--root buffer)))

(defun vterm-ai-osc-children (buffer)
  "Return child record plists of BUFFER, each with :id, in arrival order."
  (and (buffer-live-p buffer)
       (mapcar (lambda (cell) (cons :id (cons (car cell) (cdr cell))))
               (buffer-local-value 'vterm-ai-osc--children buffer))))

;;;###autoload
(define-minor-mode vterm-ai-osc-mode
  "Answer OSC 7501 probes in vterm and record program status reports.
Enable it before starting agents: Claude Code probes only at startup."
  :global t
  :group 'vterm-ai-osc
  (if vterm-ai-osc-mode
      (if (or (fboundp 'vterm--filter)
              (and (require 'vterm nil t) (fboundp 'vterm--filter)))
          (progn
            (advice-add 'vterm--filter :before #'vterm-ai-osc--scan)
            (add-hook 'vterm-exit-functions #'vterm-ai-osc--on-exit))
        (setq vterm-ai-osc-mode nil)
        (display-warning 'vterm-ai "vterm--filter not found; OSC 7501 disabled"))
    (advice-remove 'vterm--filter #'vterm-ai-osc--scan)
    (remove-hook 'vterm-exit-functions #'vterm-ai-osc--on-exit)))

(provide 'vterm-ai-osc)
;;; vterm-ai-osc.el ends here

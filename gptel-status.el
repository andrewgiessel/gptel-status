;;; gptel-status.el --- Compact status indicator for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Andrew Giessel
;; SPDX-License-Identifier: MIT

;; Author: Andrew Giessel <andrew.giessel@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8"))
;; Keywords: convenience, gptel
;; URL: https://github.com/andrewgiessel/gptel-status

;;; Commentary:
;; A bounded, fixed-width status indicator for gptel requests and delegated
;; work.  See README.md for integration APIs and status semantics.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gptel)

(defgroup gptel-status nil
  "Compact status indicators for gptel conversations."
  :group 'gptel)

(defface gptel-status-waiting-face '((t :inherit warning))
  "Face for a request waiting on the network." :group 'gptel-status)
(defface gptel-status-responding-face '((t :inherit font-lock-keyword-face))
  "Face for the response phase, which may precede visible text." :group 'gptel-status)
(defface gptel-status-tool-face '((t :inherit font-lock-function-name-face))
  "Face for tool execution." :group 'gptel-status)
(defface gptel-status-question-face '((t :inherit warning))
  "Face for a request awaiting confirmation." :group 'gptel-status)
(defface gptel-status-ready-face '((t :inherit success))
  "Face for a ready request." :group 'gptel-status)
(defface gptel-status-info-face '((t :inherit shadow))
  "Face for informational or empty status." :group 'gptel-status)
(defface gptel-status-abort-face '((t :inherit warning))
  "Face for an aborted request." :group 'gptel-status)
(defface gptel-status-error-face '((t :inherit error))
  "Face for an errored request." :group 'gptel-status)

(defcustom gptel-status-child-provider nil
  "Function returning child status plists for the current conversation buffer.
Each plist should contain :label, :state, :depth, and optionally :elapsed.
Providers may also include stable :id, :parent-id, and :order fields to request
stable depth-first ordering.  The adapter owns chat isolation and ancestry
aggregation; this package never inspects adapter-specific child registries."
  :type '(choice (const nil) function))
(defconst gptel-status-child-slot-count 2
  "Number of child status slots shown beside the parent indicator.")
(defcustom gptel-status-tooltip-limit 16000
  "Maximum characters in the indicator's accessible tooltip."
  :type 'integer)

(defvar-local gptel-status--fsms nil
  "Request FSMs currently associated with this buffer.")
(defvar-local gptel-status--native-status nil
  "Last native gptel status, stored as (MESSAGE . FACE).")
(defvar-local gptel-status--last-signature nil
  "Last visible status signature, used to avoid token-level dashboard refreshes.")
(defvar gptel-status-change-hook nil
  "Hook run after a request status changes.
Each function receives the affected buffer; adapter-owned children may map it
back to their chat buffer.")
(defvar gptel-status--default-child-provider nil
  "Child provider selected by the current adapter.")

(defconst gptel-status--active-states '(WAIT TYPE TPRE TOOL TRET))
(defconst gptel-status--glyphs
  '((waiting . ("◷" "o" gptel-status-waiting-face "nf-oct-clock"))
    (responding . ("⟳" ">" gptel-status-responding-face "nf-oct-sync"))
    (tool . ("⚒" "#" gptel-status-tool-face "nf-oct-tools"))
    (question . ("?" "?" gptel-status-question-face "nf-oct-question"))
    (ready . ("✓" "+" gptel-status-ready-face "nf-oct-check_circle"))
    (info . ("·" "!" gptel-status-info-face "nf-oct-info"))
    (abort . ("■" "x" gptel-status-abort-face "nf-oct-stop"))
    (error . ("ⓧ" "x" gptel-status-error-face "nf-oct-x_circle")))
  "Unicode fallback, narrow fallback, face, and optional Nerd Icon per state.")

(defun gptel-status--state-label (state)
  (or (cdr (assq state '((WAIT . "waiting") (TYPE . "responding")
                         (TPRE . "preparing tool") (TOOL . "executing tool")
                         (TRET . "receiving tool result"))))
      (if (symbolp state) (downcase (symbol-name state)) "unknown")))

(defun gptel-status--parent-state ()
  "Return (STATE DESCRIPTION) for the current buffer's request/native state."
  (let* ((fsms (seq-filter
                (lambda (fsm)
                  (and (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm)
                       (memq (gptel-fsm-state fsm) gptel-status--active-states)))
                gptel-status--fsms))
         (states (mapcar #'gptel-fsm-state fsms))
         (native gptel-status--native-status)
         (message (and native (string-trim (car native))))
         (face (cdr-safe native))
         (state
          (cond
           ((and message (string-match-p "\\`Run tools?\\?" message)) 'question)
           ((seq-some (lambda (s) (memq s '(TPRE TOOL TRET))) states) 'tool)
           ((memq 'WAIT states) 'waiting)
           ((memq 'TYPE states) 'responding)
           ((and message (string-match-p "\\`[Ww]aiting" message)) 'waiting)
           ((and message (string-match-p "\\`[Tt]yping" message)) 'responding)
           ((and message (string-match-p "\\`Calling tools?" message)) 'tool)
           ((and message (string-match-p "\\`\\(?:Error\\|Abort\\)" message))
            (if (string-match-p "\\`Abort" message) 'abort 'error))
           ((and face (eq face 'error)) 'error)
           ((and message (string-match-p "\\`Empty response" message)) 'info)
           ((or (and message (string-match-p "\\`Ready" message))
                (and message (string-match-p "\\`gptel" message))) 'ready)
           (message 'info)
           (t nil))))
    (when state
      (list state
            (cond
             ((eq state 'question) message)
             (fsms (mapconcat #'gptel-status--state-label
                              (delete-dups (mapcar #'gptel-fsm-state fsms)) ", "))
             (message (replace-regexp-in-string "\\`gptel[ :]*" "" message))
             (t (symbol-name state)))))))

(defun gptel-status--child-state (child)
  (let ((state (if (and (listp child) (plist-get child :state))
                   (plist-get child :state) child)))
    (cond ((memq state '(WAIT waiting)) 'waiting)
          ((memq state '(TYPE responding receiving)) 'responding)
          ((memq state '(TPRE TOOL TRET tool executing)) 'tool)
          ((memq state '(ABRT abort canceled)) 'abort)
          ((memq state '(ERR ERRS error failed)) 'error)
          ((memq state '(DONE ready finished)) 'ready)
          ((null state) 'waiting)
          (t 'info))))

(defun gptel-status--child-siblings-less-p (a b)
  "Return non-nil when child A should precede sibling B."
  (let ((oa (plist-get a :order))
        (ob (plist-get b :order))
        (ia (plist-get a :id))
        (ib (plist-get b :id)))
    (cond ((and (numberp oa) (numberp ob) (/= oa ob)) (< oa ob))
          ((and ia ib (not (equal ia ib)))
           (string-lessp (format "%s" ia) (format "%s" ib)))
          (t (string-lessp (format "%s" (plist-get a :label))
                           (format "%s" (plist-get b :label)))))))

(defun gptel-status--order-children (children)
  "Order CHILDREN depth-first when stable :id/:parent-id fields are present.
Otherwise preserve the historical depth-then-label ordering."
  (let ((ids (delq nil (mapcar (lambda (child) (plist-get child :id)) children))))
    (if (null ids)
        (sort (copy-sequence children)
              (lambda (a b)
                (let ((da (or (plist-get a :depth) 0))
                      (db (or (plist-get b :depth) 0)))
                  (if (= da db)
                      (gptel-status--child-siblings-less-p a b)
                    (< da db)))))
      (let (ordered visited)
        (cl-labels ((visit (child)
                      (let ((id (plist-get child :id)))
                        (unless (memq child visited)
                          (push child visited)
                          (push child ordered)
                          (dolist (descendant
                                   (sort
                                    (seq-filter
                                     (lambda (candidate)
                                       (equal (plist-get candidate :parent-id) id))
                                     children)
                                    #'gptel-status--child-siblings-less-p))
                            (visit descendant))))))
          (dolist (root (sort
                         (seq-filter
                          (lambda (child)
                            (not (member (plist-get child :parent-id) ids)))
                          children)
                         #'gptel-status--child-siblings-less-p))
            (visit root))
          ;; Malformed/cyclic ancestry must not make child statuses disappear.
          (dolist (child (sort (copy-sequence children)
                               #'gptel-status--child-siblings-less-p))
            (visit child)))
        (nreverse ordered)))))

(defun gptel-status-snapshot (&optional buffer)
  "Return a normalized status snapshot for BUFFER (defaults to current buffer).
The returned plist has :state, :label, :children, and :child-count.  Children
with identity metadata are ordered depth-first; other providers use depth/label."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((parent (gptel-status--parent-state))
           (children (and (functionp gptel-status-child-provider)
                          (condition-case nil
                              (funcall gptel-status-child-provider (current-buffer))
                            (error nil))))
           (children (gptel-status--order-children children)))
      (list :state (car parent) :label (cadr parent)
            :children children :child-count (length children)))))

(defun gptel-status--icon (state &optional narrow)
  "Return one display cell for STATE, with face and help-free properties."
  (let* ((spec (or (cdr (assq state gptel-status--glyphs))
                   (cdr (assq 'info gptel-status--glyphs))))
         (icon (nth 3 spec))
         (unicode (nth 0 spec))
         (fallback (nth 1 spec))
         (face (nth 2 spec))
         (text (if (and (not narrow) icon (fboundp 'nerd-icons-octicon))
                   (condition-case nil (nerd-icons-octicon icon) (error unicode))
                 (if (and (not narrow) (= (string-width unicode) 1)) unicode
                   (or fallback "?")))))
    (unless (= (string-width text) 1) (setq text (or fallback "?")))
    (setq text (copy-sequence text))
    (add-face-text-property 0 (length text) face t text)
    text))

(defun gptel-status--elapsed (child)
  (let ((seconds (plist-get child :elapsed)))
    (when (numberp seconds) (format "%.1fs" (max 0 seconds)))))

(defun gptel-status--tooltip (snapshot)
  (let* ((parent-state (plist-get snapshot :state))
         (children (plist-get snapshot :children))
         (lines (list (format "Parent: %s" (or (plist-get snapshot :label) "idle")))))
    (dolist (child children)
      (push (format "%s%s — %s%s%s"
                    (make-string (min 20 (max 0 (or (plist-get child :depth) 0))) ? )
                    (or (plist-get child :label) "child")
                    (gptel-status--state-label (plist-get child :state))
                    (if (gptel-status--elapsed child)
                        (concat " · " (gptel-status--elapsed child)) "")
                    (if (plist-get child :description)
                        (concat " · " (plist-get child :description)) "")) lines))
    (when (> (plist-get snapshot :child-count) gptel-status-child-slot-count)
      (push (format "%d child tasks total" (plist-get snapshot :child-count)) lines))
    (let ((text (mapconcat #'identity (nreverse lines) "\n")))
      (if (> (length text) gptel-status-tooltip-limit)
          (concat (substring text 0 (max 0 (- gptel-status-tooltip-limit 1))) "…")
        text))))

(defun gptel-status-render (snapshot)
  "Render SNAPSHOT as four cells: parent, two children, and overflow.
Rendering is independent of buffers and request lifecycle instrumentation."
  (let* ((state (plist-get snapshot :state))
         (children (plist-get snapshot :children))
         (slots (max 0 gptel-status-child-slot-count))
         (shown (seq-take children slots))
         (overflow (max 0 (- (length children) slots)))
         (tooltip (gptel-status--tooltip snapshot))
         (parent-cell (gptel-status--icon (or state 'info)))
         (child-cells (mapconcat (lambda (child)
                                   (gptel-status--icon
                                    (gptel-status--child-state child)))
                                 shown ""))
         (spaces (make-string (max 0 (- slots (length shown))) ? ))

         (overflow-cell (if (> overflow 0) "+" " ")))
    (add-text-properties 0 (length parent-cell) (list 'help-echo tooltip) parent-cell)
    (propertize (concat parent-cell child-cells spaces overflow-cell)
                'help-echo tooltip)))

(defun gptel-status-indicator (&optional buffer)
  "Return BUFFER's fixed-width indicator when it has gptel activity."
  (let* ((buffer (or buffer (current-buffer)))
         (snapshot (gptel-status-snapshot buffer)))
    (when (or (plist-get snapshot :state)
              (plist-get snapshot :children)
              (buffer-local-value 'gptel-mode buffer))
      (gptel-status-render snapshot))))

(defun gptel-status-notify-change (buffer)
  "Notify status consumers after adapter-owned state changes in BUFFER."
  (gptel-status--notify buffer))

(defun gptel-status--signature (buffer)
  "Return a stable state signature, excluding elapsed time and streamed text."
  (let ((snapshot (gptel-status-snapshot buffer)))
    (list (plist-get snapshot :state)
          (mapcar (lambda (child)
                    (list (plist-get child :label) (plist-get child :state)
                          (plist-get child :depth)))
                  (plist-get snapshot :children)))))

(defun gptel-status--notify (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (force-mode-line-update t)
      (let ((signature (gptel-status--signature buffer)))
        (unless (equal signature gptel-status--last-signature)
          (setq gptel-status--last-signature signature)
          (run-hook-with-args 'gptel-status-change-hook buffer))))))

(defun gptel-status--observe-transition (original fsm &rest args)
  "Track FSM transitions, including terminal error/abort states.
The installed gptel exposes no public transition hook; this isolated private
seam is needed for tool phases that its public response hooks do not cover."
  (unwind-protect (apply original fsm args)
    (when (and (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm))
      (let* ((info (gptel-fsm-info fsm))
             (buffer (plist-get info :buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (if (memq (gptel-fsm-state fsm) gptel-status--active-states)
                (cl-pushnew fsm gptel-status--fsms :test #'eq)
              (setq gptel-status--fsms (delq fsm gptel-status--fsms)))
            (gptel-status--notify buffer)))))))

(defun gptel-status--capture-native-status (original message &optional face)
  "Capture native gptel status while preserving upstream behavior.
There is no public status-formatting hook exposing errors and confirmation;
keep this private renderer seam isolated from the public snapshot API."
  (prog1 (funcall original message face)
    (when (bound-and-true-p gptel-mode)
      (setq-local gptel-status--native-status
                  (cons (substring-no-properties message) face))
      (gptel-status--notify (current-buffer)))))

(defun gptel-status--seed-active-requests ()
  (when (and (boundp 'gptel--request-alist) (fboundp 'gptel-fsm-p))
    (dolist (entry gptel--request-alist)
      (let* ((fsm (cadr entry))
             (info (and (gptel-fsm-p fsm) (gptel-fsm-info fsm)))
             (buffer (plist-get info :buffer)))
        (when (and info (buffer-live-p buffer)
                   (memq (gptel-fsm-state fsm) gptel-status--active-states))
          (with-current-buffer buffer
            (cl-pushnew fsm gptel-status--fsms :test #'eq)))))))

;;;###autoload
(define-minor-mode gptel-status-mode
  "Track gptel state for compact indicators."
  :global t :group 'gptel-status
  (if gptel-status-mode
      (progn
        (unless gptel-status-child-provider
          (setq gptel-status-child-provider gptel-status--default-child-provider))
        (when (fboundp 'gptel--fsm-transition)
          (advice-remove 'gptel--fsm-transition #'gptel-status--observe-transition)
          (advice-add 'gptel--fsm-transition :around #'gptel-status--observe-transition))
        (when (fboundp 'gptel--update-status)
          (advice-remove 'gptel--update-status #'gptel-status--capture-native-status)
          (advice-add 'gptel--update-status :around #'gptel-status--capture-native-status))
        (gptel-status--seed-active-requests))
    (advice-remove 'gptel--fsm-transition #'gptel-status--observe-transition)
    (advice-remove 'gptel--update-status #'gptel-status--capture-native-status)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (kill-local-variable 'gptel-status--fsms)
        (kill-local-variable 'gptel-status--native-status)
        (kill-local-variable 'gptel-status--last-signature)
        (force-mode-line-update t)))))

(defun gptel-status-enable ()
  "Enable gptel status tracking. Safe to call repeatedly."
  (interactive)
  (gptel-status-mode 1))

(defun gptel-status-disable ()
  "Disable gptel status tracking and remove installed advice."
  (interactive)
  (gptel-status-mode -1))

(provide 'gptel-status)
;;; gptel-status.el ends here

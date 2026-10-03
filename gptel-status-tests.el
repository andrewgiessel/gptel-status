;;; gptel-status-tests.el --- Tests for gptel-status -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Andrew Giessel
;; SPDX-License-Identifier: MIT

;;; Code:
(require 'ert)
(require 'gptel-status)
(require 'gptel-status-doom)

(ert-deftest gptel-status-doom-segment-is-padded-shared-indicator ()
  (let ((gptel-status-mode t))
    (cl-letf (((symbol-function 'gptel-status-indicator)
               (lambda (&optional _buffer) "abcd")))
      (should (equal (doom-modeline-segment--gptel-status) " abcd ")))))

(ert-deftest gptel-status-doom-require-does-not-select-or-mutate-layouts ()
  (let ((layouts (copy-tree doom-modeline--modelines))
        (format mode-line-format))
    (require 'gptel-status-doom nil t)
    (should (equal doom-modeline--modelines layouts))
    (should (equal mode-line-format format))))

(ert-deftest gptel-status-indicator-has-fixed-width-and-overflow ()
  (with-temp-buffer
    (setq-local gptel-status-child-provider
                (lambda (_buffer)
                  '((:label "alpha" :state WAIT :depth 1 :elapsed 2.1)
                    (:label "beta" :state TYPE :depth 2)
                    (:label "gamma" :state TOOL :depth 3))))
    (let ((indicator (gptel-status-indicator)))
      (should (= (string-width indicator) 4))
      (should (equal (substring-no-properties indicator 3 4) "+"))
      (should (string-match-p "gamma" (get-text-property 0 'help-echo indicator)))
      (should (string-match-p "3 child tasks" (get-text-property 0 'help-echo indicator))))))

(ert-deftest gptel-status-indicator-fallback-stays-one-cell ()
  (let ((gptel-status-child-provider nil))
    (with-temp-buffer
      (let ((indicator (gptel-status-render '(:state ready :children nil :child-count 0))))
        (should (= (string-width indicator) 4))
        (should (equal (substring-no-properties indicator 1) "   "))))))

(ert-deftest gptel-status-child-states-are-normalized ()
  (should (eq (gptel-status--child-state 'WAIT) 'waiting))
  (should (eq (gptel-status--child-state 'TYPE) 'responding))
  (should (eq (gptel-status--child-state 'TOOL) 'tool))
  (should (eq (gptel-status--child-state 'ABRT) 'abort))
  (should (eq (gptel-status--child-state 'ERR) 'error)))

(ert-deftest gptel-status-render-zero-one-two-many ()
  (dotimes (count 7)
    (let* ((children (cl-loop for i below count
                              collect (list :label (format "child-%d" i)
                                            :state 'TYPE :depth 1)))
           (text (gptel-status-render (list :state 'tool :children children
                                           :child-count count))))
      (should (= (string-width text) 4))
      (should (equal (substring-no-properties text 3) (if (> count 2) "+" " "))))))

(ert-deftest gptel-status-preserves-icon-font-for-parent-and-children ()
  (cl-letf (((symbol-function 'nerd-icons-octicon)
             (lambda (_) (propertize "x" 'face '(:family "Test Nerd Font")))))
    (let ((text (gptel-status-render
                 '(:state waiting :children ((:label "child" :state TYPE)) :child-count 1))))
      (dotimes (index 2)
        (should (member '(:family "Test Nerd Font") (get-text-property index 'face text)))))))

(ert-deftest gptel-status-snapshot-orders-children-and-tolerates-provider-errors ()
  (with-temp-buffer
    (let ((gptel-status-child-provider
           (lambda (_) '((:label "z" :depth 2) (:label "b" :depth 1) (:label "a" :depth 1)))))
      (should (equal (mapcar (lambda (c) (plist-get c :label))
                            (plist-get (gptel-status-snapshot) :children)) '("a" "b" "z"))))
    (let ((gptel-status-child-provider (lambda (_) (error "provider failed"))))
      (should-not (plist-get (gptel-status-snapshot) :children)))))

(ert-deftest gptel-status-idle-ordinary-buffer-is-hidden ()
  (with-temp-buffer
    (let ((gptel-status-child-provider nil))
      (should-not (gptel-status-indicator)))))

(ert-deftest gptel-status-confirmation-overrides-tool-phase ()
  (with-temp-buffer
    (setq-local gptel-status--fsms (list (gptel-make-fsm :state 'TOOL))
                gptel-status--native-status '("Run tools?" . warning))
    (should (eq (plist-get (gptel-status-snapshot) :state) 'question))))

(provide 'gptel-status-tests)
;;; gptel-status-tests.el ends here

;;; gptel-status-doom.el --- Doom Modeline segment for gptel-status -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Andrew Giessel
;; SPDX-License-Identifier: MIT
;; Author: Andrew Giessel <andrew.giessel@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel-status "0.1.0") (doom-modeline "3.0"))

;;; Commentary:
;; Optional segment-only Doom integration. Require this file, then define and
;; select whichever modeline layouts suit your configuration.

;;; Code:
(require 'gptel-status)
(require 'doom-modeline)

(doom-modeline-def-segment gptel-status
  "Padded fixed-width gptel request and delegated-task status indicator."
  (let ((indicator (gptel-status-indicator)))
    (when indicator (concat " " indicator " "))))

(provide 'gptel-status-doom)
;;; gptel-status-doom.el ends here

;;; +undo.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package vundo
  :demand t
  :bind
  (("C-x u" . my/vundo-open))
  :config
  ;; Appearance
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)

  ;; Undo limits
  (setq undo-limit 800000)           ;; default: ~160k
  (setq undo-strong-limit 12000000)  ;; default: ~12MB
  (setq undo-outer-limit 120000000)  ;; default: ~120MB

  ;; --- Fixed: prevent dedicated window error ---
  (defun my/vundo-open ()
    "Open vundo in a non-dedicated window."
    (interactive)
    (let ((display-buffer-alist
           '(("\\*vundo-tree\\*"
              ;; Open in a normal, non-dedicated side window
              (display-buffer-in-side-window)
              (side . right)
              (window-width . 0.3)
              (dedicated . nil)))))
      (vundo))))


(provide '+undo)
;;; +undo.el ends here

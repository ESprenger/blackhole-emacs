;;; +treemacs.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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


;; =============================================================================
;; TREEMACS
;; =============================================================================
(use-package treemacs
  :demand t
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))


(use-package lsp-treemacs
  :after (treemacs lsp)
  :custom (lsp-treemacs-sync-mode 1))


(provide '+treemacs)
;;; +treemacs.el ends here

;;; +org.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; ORG
;; =============================================================================

(use-package org
  :ensure nil
  :config
  ;; Directory setup
  (defvar +org-dir (expand-file-name "~/Documents/org/")
    "Primary Org directory.")
  (setq org-directory +org-dir)
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; Agenda files: recurse .org, ignore backups/hidden
  (setq org-agenda-files
        (let ((dir org-directory))
          (when (file-directory-p dir)
            (directory-files-recursively dir "\\`[^.].*\\.org\\'"))))

  ;; Skip per-file startup when agenda first visits files
  (setq org-agenda-inhibit-startup t)  ;; speeds agenda on first open [manual tip]

  ;; Global keys: these are autoloaded; no need to (require 'org) at init
  ;; (global-set-key (kbd "C-c l") #'org-store-link)
  ;; (global-set-key (kbd "C-c a") #'org-agenda)
  ;; (global-set-key (kbd "C-c c") #'org-capture)

  ;; Core behavior
  (setq org-log-done 'time)            ;; timestamp DONE
  (setq org-return-follows-link t)     ;; RET follows links [links manual]

  ;; Visuals
  (setq org-hide-emphasis-markers t)   ;; hide *bold* markers
  (setq org-startup-folded t)          ;; start folded (fast on big files)

  ;; Prefer startup variable for indent; avoid always-on hook cost
  (setq org-startup-indented t)        ;; equivalent to org-indent-mode at startup [Org Indent manual]

  ;; Large-file guard: dampen expensive minors in huge buffers
  (defun +org-large-buffer-setup ()
    "Tune Org features for large buffers."
    (when (and (derived-mode-p 'org-mode)
               (> (buffer-size) 500000)) ;; ~500KB threshold, adjust as needed
      ;; Back off costly visuals on very large files
      (when (bound-and-true-p org-indent-mode)
        (org-indent-mode -1))
      (setq-local org-hide-emphasis-markers nil)
      (setq-local org-fold-core-style 'overlays))) ;; simpler folding if available
  (add-hook 'org-mode-hook #'+org-large-buffer-setup)

  ;; Editing in src blocks
  (setq org-src-fontify-natively t)    ;; needed for native TAB in blocks [doc]
  (setq org-src-tab-acts-natively t)   ;; TAB delegates to language mode [doc]
  ;; If TAB sluggish in huge blocks, consider toggling per-mode or via a size check.

  ;; Soft-wrap and visual indent when not huge
  (add-hook 'org-mode-hook #'visual-line-mode))



(provide '+org)
;;; +org.el ends here

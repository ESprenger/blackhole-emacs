;;; +git.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; MAGIT
;; =============================================================================
(use-package transient
  :demand t)

(use-package magit
  :demand t
  :after transient
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; :init (setq magit-auto-revert-mode nil)
  :config
  ;; (add-hook 'magit-refresh-buffer-hook (defun +magit-invalidate-projectile-cache-h ()
  ;;                                        ;; Only invalidate the hot cache and nothing else (everything else is
  ;;                                        ;; expensive busy work, and we don't want to slow down magit's
  ;;                                        ;; refreshing).
  ;;                                        (let (projectile-require-project-root
  ;;                                              projectile-enable-caching
  ;;                                              projectile-verbose)
  ;;                                          (letf! ((#'recentf-cleanup #'ignore))
  ;;                                                 (projectile-invalidate-cache nil)))))
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t)
  (setopt magit-format-file-function #'magit-format-file-nerd-icons) ;; Turns on magit nerd-icons
  )

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE")))

(use-package magit-org-todos
  :after magit
  :config
  (magit-org-todos-autoinsert))

(provide '+git)
;;; +git.el ends here

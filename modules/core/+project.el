;;; +project.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

(use-package projectile
  :demand t
  :custom
  (projectile-completion-system 'default)
  (projectile-project-search-path '("~/Documents/workspace/repos"))
  (projectile-enable-caching 'persistent)
  (projectile-switch-project-action #'projectile-dired)
  :init
  (progn
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path))
  :config (projectile-mode))




(provide '+project)
;;; +project.el ends here

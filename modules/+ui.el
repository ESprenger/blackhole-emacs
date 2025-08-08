;;; +ui.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: August 01, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; DOOM THEME
;; (use-package doom-themes
;;   :straight t
;;   :demand t
;;   :custom
;;   (doom-themes-enable-bold t)
;;   (doom-themes-enable-italic t)
;;   ;; (doom-themes-treemacs-theme "doom-atom")
;;   :config
;;   (load-theme 'doom-challenger-deep t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-treemacs-config)
;;   (doom-themes-org-config))


;;; CATPPUCCIN-THEME
(use-package catppuccin-theme
  :straight t
  :config
  ;; Load the Catppuccin theme without prompting for confirmation.
  (load-theme 'catppuccin :no-confirm)
  ;; (setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha
  ;; (catppuccin-reload)
  )


;;; DOOM MODELINE
;; The `doom-modeline' package provides a sleek, modern mode-line
(use-package doom-modeline
  :straight t
  :defer t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-project-detection 'projectile)        ;; Enable project detection for displaying the project name.
  (doom-modeline-buffer-name t)                        ;; Show the buffer name in the mode line.
  (doom-modeline-vcs-max-length 25)                    ;; Limit the version control system (VCS) branch name length to 25 characters.
  (doom-modeline-height 30)
  (doom-modeline-minor-modes t)
  (doom-modeline-lsp t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-name t)
  (doom-modeline-column-zero-based t)
  (doom-modeline-project-name t)
  (doom-modeline-icon t))

;;; DASHBOARD
(use-package dashboard
  :straight t
  :defer t
  :hook (after-init . dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
              ("C-n" . 'dashboard-next-line)
              ("C-p" . 'dashboard-previous-line))
  :init (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-navigation-cycle t)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-image-banner-max-height 450)
  (dashboard-startup-banner (concat user-static-dir "blackhole_small.png"))
  (dashboard-footer-messages '(" Time teaches all things."))
  :config
  (setq dashboard-startupify-list '(dashboard-insert-newline
                                    dashboard-insert-newline
                                    dashboard-insert-newline
                                    dashboard-insert-banner
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  (setq dashboard-footer-icon (nerd-icons-sucicon "nf-custom-emacs"
                                                  :height 2.0
                                                  :v-adjust -0.1
                                                  :face 'font-lock-keyword-face))
  (setq
   dashboard-projects-backend 'projectile
   dashboard-projects-switch-function 'projectile-switch-project-by-name
   dashboard-items '((recents        . 5)
                     (agenda         . 5)
                     (projects       . 15)))
  (setq dashboard-item-shortcuts '((recents . "f")
                                   (agenda . "a")
                                   (projects . "p")))
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(nerd-icons-codicon "nf-cod-refresh" :height 2.0 :v-adjust -0.1)
            "Update"
            "Update Emacs"
            (lambda (&rest _) (straight-pull-all)) warning "| " " |")
           (,(nerd-icons-mdicon "nf-md-hammer_screwdriver" :height 2.0 :v-adjust -0.1)
            "Rebuild"
            "Rebuild Emacs"
            (lambda (&rest _) (straight-rebuild-all)) warning "" " |")))))


(provide '+ui)
;;; +ui.el ends here

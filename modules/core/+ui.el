;;; +ui.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; NERD-ICONS
;; =============================================================================
(use-package nerd-icons
  :demand t)

;; Icons in completion UI (with Marginalia or Corfu)
(use-package nerd-icons-completion
  :demand t
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Icons in dired
(use-package nerd-icons-dired
  :demand t
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; corfu
(use-package nerd-icons-corfu
  :demand t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ibuffer
(use-package nerd-icons-ibuffer
  :demand t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-color-icon t))

;; treemacs
(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; =============================================================================
;; THEME
;; =============================================================================
(use-package catppuccin-theme
  :demand t
  :config
  (load-theme 'catppuccin :no-confirm))

;; =============================================================================
;; DOOM
;; =============================================================================
;; (use-package doom-themes
;;   :straight t
;;   :custom
;;   ;; Global settings (defaults)
;;   (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
;;   (doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; for treemacs users
;;   (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   :config
;;   (load-theme 'doom-moonlight t)
;;
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package doom-modeline
  :defer t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-project-detection 'projectile)        ;; Enable project detection for displaying the project name.
  (doom-modeline-buffer-file-name-style 'truncate-nil)
  (doom-modeline-buffer-name t)                        ;; Show the buffer name in the mode line.
  (doom-modeline-vcs-max-length 25)                    ;; Limit the version control system (VCS) branch name length to 25 characters.
  (doom-modeline-height 30)
  ;; (doom-modeline-minor-modes t)
  (doom-modeline-lsp t)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-name t)
  (doom-modeline-column-zero-based t)
  (doom-modeline-project-name t)
  (doom-modeline-icon t)
  (doom-modeline-spc-face-overrides (list :family (face-attribute 'fixed-pitch :family)))
  )

;; =============================================================================
;; DASHBOARD
;; =============================================================================
(use-package dashboard
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
            (lambda (&rest _) (elpaca-update-all)) warning "| " " |")
           (,(nerd-icons-mdicon "nf-md-hammer_screwdriver" :height 2.0 :v-adjust -0.1)
            "Rebuild"
            "Rebuild Emacs"
            (lambda (&rest _) (elpaca-rebuild)) warning "" " |")))))


(provide '+ui)
;;; +ui.el ends here

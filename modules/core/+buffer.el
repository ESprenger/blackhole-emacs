;;; +buffer.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; IBUFFER
;; =============================================================================

(use-package ibuffer
  :ensure nil
  :demand t
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-expert t)
  :init
  (global-set-key (kbd "C-x C-b")  'my/ibuffer-toggle)
  (setq ibuffer-saved-filter-groups
	    (quote (("default"
		         ("Directories" (mode . dired-mode))
		         ("Org" (mode . org-mode))
                 ("Terminals" (or (mode . eshell-mode)
                                  (mode . vterm-mode)))
                 ("Python" (mode . python-ts-mode))
                 ("Elisp" (mode . emacs-lisp-mode))
                 ("Bash" (or (mode . sh-ts-mode)
                             (mode . bash-ts-mode)))
		         ("emacs" (or
			               (name . "^\\*scratch\\*$")
			               (name . "^\\*Messages\\*$")
                           (name . "^\\*Warnings\\*$")
                           (name . "^\\*Async-native-compile-log\\*$")))))))
  (setq display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-use-some-window)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Ibuffer*\\*"
                 (display-buffer-reuse-mode-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.35)
                 (slot . 0)
                 (window-parameters (no-delete-other-windows . t)))
               ))


;; =============================================================================
;; CASUAL
;; =============================================================================

(use-package casual
  :demand t
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)))

(provide '+buffer)
;;; +buffer.el ends here

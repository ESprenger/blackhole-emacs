;;; +embark.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; EMBARK
;; =============================================================================
;; Embark provides a powerful contextual action menu for Emacs

(use-package embark
  :demand t
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions*\*"
                 (display-buffer-in-side-window)
                 (window-width . 0.35)
                 (side . right)
                 (slot . 0))))

;; =============================================================================
;; EMBARK-CONSULT
;; =============================================================================
;; Embark-Consult provides a bridge between Embark and Consult

(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide '+embark)
;;; +embark.el ends here

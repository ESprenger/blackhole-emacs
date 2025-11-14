;;; +marginalia.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; MARGINALIA
;; =============================================================================
;; Marginalia enhances the completion experience in Emacs by adding
;; additional context to the completion candidates.

(use-package marginalia
  :demand t
  ;; :hook
  ;; (after-init . marginalia-mode)
  :init (marginalia-mode))


(provide '+marginalia)
;;; +marginalia.el ends here

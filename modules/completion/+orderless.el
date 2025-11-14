;;; +orderless.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; ORDERLESS
;; =============================================================================
;; Orderless enhances completion in Emacs by allowing flexible pattern matching.

(use-package orderless
  :demand t
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))


(provide '+orderless)
;;; +orderless.el ends here

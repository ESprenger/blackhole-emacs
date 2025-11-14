;;; +corfu.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; CORFU
;; =============================================================================
;; Corfu Mode provides a text completion framework for Emacs.

(use-package corfu
  :demand t
  :commands (corfu-mode global-corfu-mode)
  :custom
  (corfu-auto t)                         ;; Only completes when hitting TAB
  (corfu-cycle t)
  (corfu-auto-prefix 1)                  ;; Trigger completion after typing 1 character
  (corfu-separator ?\s)
  (corfu-min-width 90)
  (corfu-max-width 90)                   ;; Always have the same width
  (corfu-scroll-margin 5)                ;; Margin when scrolling completions
  (corfu-popupinfo-delay nil)            ;; Delay before showing documentation popup
  (text-mode-ispell-word-completion nil) ;; Disable Ispell completion function.
  ;; (read-extended-command-predicate #'command-completion-default-include-p) ;; Hide commands in M-x which do not apply to the current mode.
  (corfu-echo-documentation nil)         ;; Already use corfu-doc
  (completion-styles '(orderless basic))
  :init
  (global-corfu-mode)                    ;; Enable Corfu everywhere
  (corfu-history-mode t)                 ;; Remember completions
  (corfu-popupinfo-mode t)               ;; Show documentation group
  )

(provide '+corfu)
;;; +corfu.el ends here

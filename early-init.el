;;; early-init.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: July 31, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; =============================================================================
;; USER DEFINED LOCAL DIR
;; =============================================================================

(setq user-modules-dir (expand-file-name "modules/" user-emacs-directory))
(setq core-modules-dir (expand-file-name "core/" user-modules-dir))
(setq completion-modules-dir (expand-file-name "completion/" user-modules-dir))
(setq langs-modules-dir (expand-file-name "langs/" user-modules-dir))
(setq org-modules-dir (expand-file-name "org/" user-modules-dir))
(setq funcs-modules-dir (expand-file-name "funcs/" user-modules-dir))


(setq user-templates-dir (expand-file-name "templates/" user-emacs-directory))
(setq user-snippets-dir (expand-file-name "snippets/" user-emacs-directory))
(setq user-static-dir (expand-file-name "static/" user-emacs-directory))
(setq user-emacs-directory (expand-file-name "var/" user-emacs-directory)) ; Redirect files to ../var/

;; Increase the garbage collection threshold
(setq gc-cons-threshold (* 32 1024 1024))
;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 2))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(defvar minimal-emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

;; Debug on error
(setq debug-on-error minimal-emacs-debug)

(setq load-prefer-newer t)
(setq native-comp-async-query-on-exit t)

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)
  )

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (add-to-list 'custom-theme-load-path (expand-file-name "static/themes/" user-emacs-directory))

(setq package-enable-at-startup nil) ;; Disables the default package manager.
(setq package-user-dir "~/.config/emacs/var")

(provide 'early-init)
;;; early-init.el ends here

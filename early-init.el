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

;; Increase the garbage collection threshold
(setq gc-cons-threshold #x40000000)
;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 4))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(defvar minimal-emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

;; Debug on error
(setq debug-on-error minimal-emacs-debug)

(setq load-prefer-newer t)
(setq native-comp-async-query-on-exit t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (add-to-list 'custom-theme-load-path (expand-file-name "static/themes/" user-emacs-directory))

(setq user-modules-dir (expand-file-name "modules/" user-emacs-directory))
(setq user-templates-dir (expand-file-name "templates/" user-emacs-directory))
(setq user-snippets-dir (expand-file-name "snippets/" user-emacs-directory))
(setq user-static-dir (expand-file-name "static/" user-emacs-directory))
(setq user-emacs-directory (expand-file-name "var/" user-emacs-directory)) ; Redirect files to ../var/

(setq package-enable-at-startup nil) ;; Disables the default package manager.

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)))

(setq package-enable-at-startup nil) ;; Disables the default package manager.

(provide 'early-init)
;;; early-init.el ends here

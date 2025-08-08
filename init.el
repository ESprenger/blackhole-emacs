;;; init.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(require 'package)

(use-package emacs
  :ensure nil
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (debug-on-error t)                              ;; Full stack trace if error loading
  (auto-save-default t)                           ;; Enable automatic saving of buffers.
  (auto-save-no-message t)                        ;; Inhibit auto-save message
  (auto-save-include-big-deletions t)             ;; Do not auto-disable auto-save after deleting large chunks of text
  (kill-buffer-delete-auto-save-files t)
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (comment-multi-line t)                          ;; Enable multi line commenting
  (comment-empty-lines t)                         ;; Enable commenting empty lines in region
  (compilation-ask-about-save nil)                ;; Compilation settings
  (compilation-always-kill t)                     ;; Compilation settings
  (compilation-scroll-output 'first-error)        ;; Compilation settings
  (confirm-nonexistent-file-or-buffer nil)        ;; Skip confirmation prompts when creating a new file or buffer
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (custom-buffer-done-kill t)
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (electric-indent-mode -1)
  (electric-pair-mode 1)
  (enable-recursive-minibuffers t)                ;; Enable nested minibuffers
  (eval-expression-print-length nil)              ;; Disable truncation of printed s-expressions in the message buffer
  (eval-expression-print-level nil)               ;; Disable truncation of printed s-expressions in the message buffer
  (find-file-suppress-same-file-warnings t)
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files t)                           ;; Enable creation of backup files.
  (delete-old-versions t)                         ;; Delete excess backup versions silently
  (version-control t)                             ;; Use version numbers for backup files
  (kept-new-versions 5)
  (kept-old-versions 5)
  (kill-do-not-save-duplicates t)                 ;; Remove duplicates from the kill ring to reduce clutter
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision
  ;; (read-answer-short t)                           ;; Enable short answers
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'mode)
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (sentence-end-double-space nil)                 ;; Disable old practice of end-of-line practicing
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (truncate-string-ellipsis "…")                  ;; Enable smaller string ellipsis
  (undo-limit (* 13 160000))                      ;; Undo/Redo limits
  (undo-strong-limit (* 13 240000))               ;; Undo/Redo limits
  (undo-outer-limit (* 13 24000000))              ;; Undo/Redo limits
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (visible-bell t)                                ;; Enable blinking bell
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.
  (whitespace-line-column nil)                    ;; Enable use the value of 'fill-column'
  (debug-on-error t)
  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.

  :config
  (setq user-full-name    "Evan Sprenger"
        user-mail-address "evan.sprenger@gmail.com")
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (setq auto-save-list-file-prefix
        (expand-file-name "autosave/" user-emacs-directory))
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" user-emacs-directory))

  (setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

  (setq world-clock-list
        '(("America/Los_Angeles" "San Francisco")
          ("America/Chicago" "Houston")
          ("America/New_York" "New York")
          ("Europe/London" "London")))

  (setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

  ;; Setq options and font settings
  (set-face-attribute 'default nil
		              :font "JetBrainsMono NF"
		              :height 110
		              :weight 'medium)
  (set-face-attribute 'variable-pitch nil
		              :font "Ubuntu"
		              :height 120
		              :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
		              :font "JetBrainsMono NF"
		              :height 110
		              :weight 'medium)
  (set-face-attribute 'font-lock-comment-face nil
		              :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
		              :slant 'italic)
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-11"))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default line-spacing 0.12)
  (setq-default fill-column 80)
  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t)
  (setq-default electric-indent-chars '(?\n ?\^?))
  (setq-default frame-title-format '("EMACS"))

  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.

  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.

  (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)
  )

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(defvar module-list '(
                      "+defaults"
                      "+completion"
                      "+tools"
                      "+langs"
                      "+org"
                      "+shells"
                      "+keymaps"
                      "+ui"
                      ))

(dolist (module module-list)
  (load (expand-file-name (concat user-modules-dir module))))


(provide 'init)
;;; init.el ends here

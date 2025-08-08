;;; +defaults.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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


;;; BEGINNING-OF-LINE
;; Cycle "C-a" between beginning of line and beginnig of indentation
(defun es/point-at-indentation ()
  "Return non-nil if point is at indentation, nil otherwise."
  (= (save-excursion (back-to-indentation) (point)) (point)))

(defun es/beginning-of-line-or-indentation ()
  "Toggle between beginning of line and point of indentation."
  (interactive)
  (if (es/point-at-indentation)
      (beginning-of-line)
    (back-to-indentation)))

(define-key (current-global-map) [remap move-beginning-of-line] 'es/beginning-of-line-or-indentation)

;;; IBUFFER
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-expert t)
  :init (setq ibuffer-saved-filter-groups
	          (quote (("default"
		               ("dired" (mode . dired-mode))
		               ("org" (mode . org-mode))
		               ("emacs" (or
			                     (name . "^\\*scratch\\*$")
			                     (name . "^\\*Messages\\*$")
                                 (name . "^\\*Warnings\\*$")
                                 (name . "^\\*Async-native-compile-log\\*$"))))))))

(use-package ibuffer-sidebar
  :straight (ibuffer-sidebar :type git :host github :repo "jojojames/ibuffer-sidebar")
  :commands (ibuffer-sidebar-toggle-sidebar)
  ;; :custom
  ;; (ibuffer-sidebar-use-custom-font t)
  ;; (ibuffer-sidebar-face `(:family "Helvetica" :height 110))
  :bind (("C-x C-b" . ibuffer-sidebar-toggle-sidebar)))


;;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :ensure t
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))


;;; WHICH-KEY
(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 12)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 1.5)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " → " ))


;;; HELPFUL
(use-package helpful
  :ensure t
  :straight t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))


;; WINDOW
(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ;; ("\\*\\(lsp-help\\)\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ;; ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . 1))
     )))


;;; DIRED
(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^flycheck_.*"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-open
  :straight t
  :after dired
  :config
  (setq dired-open-extensions '(("csv" . "libreoffice --calc")
                                ("xls" . "libreoffice --calc")
                                ("xlsx" . "libreoffice --calc")
                                ("doc" . "libreoffice --writer")
                                ("docx" . "libreoffice --writer")
                                ("pdf" . "okular"))))


;; UNDO
(use-package undo-fu
  :straight t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-/") 'undo-fu-only-redo))

;; Save and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
  :straight t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))


;;; NERD ICONS
;; The `nerd-icons' package provides a set of icons for use in Emacs.
(use-package nerd-icons
  :straight (nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el")
  :defer t)


;;; NERD ICONS DIRED
;; The `nerd-icons-dired' package integrates nerd icons into the Dired mode,
(use-package nerd-icons-dired
  :straight t
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))


;;; WHITESPACE
(use-package stripspace
  :straight t
  :commands stripspace-local-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))


;;; YASNIPPET
(use-package yasnippet
  :straight t
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  ;; :custom
  ;; (yas-snippet-dirs '(user-snippets-dir))
  )


;;; DOOM SNIPPETS
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git
                           :host github
                           :repo "doomemacs/snippets"
                           :files ("*.el" "*")))


(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :straight t
  :custom
  (auto-insert-query nil)
  (auto-insert-directory user-templates-dir)
  (auto-insert-mode 1)
  :config
  (add-hook 'find-file-hook 'auto-insert)
  (define-auto-insert "\\.el$" ["default-elisp.el" autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh.el" autoinsert-yas-expand])
  (define-auto-insert "main.py$" ["main-py.el" autoinsert-yas-expand]))

(provide '+defaults)
;;; +defaults.el ends here

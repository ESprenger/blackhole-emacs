;;; +tools.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: August 01, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; INDENT-GUIDE
;; The `indent-guide' package provides visual indicators for indentation levels
(use-package indent-guide
  :straight t
  :defer t
  :hook
  (prog-mode . indent-guide-mode))


;;; DOTENV
;; A simple major mode to provide .env files with color highlighting
(use-package dotenv-mode
  :straight t
  :defer t)


;;; MAGIT
;; powerful Git interface for Emacs
(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons) ;; Turns on magit nerd-icons
  )


;;; TREESIT
;; Treesit-auto simplifies the use of Tree-sitter grammars in Emacs
(use-package treesit-auto
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))


;;; APELEIA
;; Automated code formatting
(use-package apheleia
  :straight (apheleia :host github :repo "radian-software/apheleia")
  :defer t
  :hook (prog-mode . apheleia-mode))

;;; TREEMACS
;; NeoTree like viewer
(use-package treemacs
  :straight t
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))


;;; PROJECTILE
;; Project management
(use-package projectile
  :straight t
  :custom
  (projectile-completion-system 'default)
  (projectile-project-search-path '("~/Documents/workspace/repos"))
  (projectile-enable-caching 'persistent)
  (projectile-switch-project-action #'projectile-dired)
  :init
  (progn
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path))
  :config (projectile-mode))


;;; FLYCHECK
(use-package flycheck
  :straight t
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-idle-change-delay 2.0)
  (flycheck-keymap-prefix "\3f"))


;;; lsp
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  ;; (setq )
  :hook ((lsp-completion-mode . lsp-mode-setup-completion) ;; setup orderless completion style.
         (lsp-mode . lsp-enable-which-key-integration)
         (clojure-ts-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c c l")
  (lsp-enable-text-document-color t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-completion-provider :none) ;; we use corfu!
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind nil)
  :config
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory)))



(use-package lsp-ui
  :straight t
  :hook (lsp-deferred . lsp-ui-mode)
  :custom
  (lsp-enable-which-key-integration t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-delay 2)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-treemacs
  :straight t
  :custom (lsp-treemacs-sync-mode 1))


;;; DAP
(use-package dap-mode
  :straight t
  :after lsp-mode
  :custom
  (dap-auto-configure-mode t)
  (dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
  :config
  ;; Python
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template "Python Run"
                               (list :type "python"
                                     :args "-i"
                                     :cwd nil
                                     :env '(("DEBUG" . "1"))
                                     :target-module nil
                                     :request "launch"
                                     :name "Python Run"))
  (dap-register-debug-template "Python :: Attach to running process"
                               (list :type "python"
                                     :request "attach"
                                     :processId "${command:pickProcess}"
                                     :name "Python :: Attach to running process"))
  ;; OTHER

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )
(provide '+tools)
;;; +tools.el ends here

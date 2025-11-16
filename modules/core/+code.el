;;; +code.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; FLYCHECK
;; =============================================================================

(use-package flycheck
  :demand t
  :custom
  (flycheck-idle-change-delay 2.0)
  (flycheck-keymap-prefix "\3f")
  :init
  (global-flycheck-mode))

;; =============================================================================
;; LSP
;; =============================================================================
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (defun lsp-update-modeline (&rest _)
    "Update modeline with lsp state."
    (let* ((workspaces (lsp-workspaces))
           (face (if workspaces 'success 'warning))
           (label (if workspaces "LSP Connected" "LSP Disconnected")))
      (setq lsp-modeline-icon (concat
                               " "
                               (nerd-icons-faicon "nf-fa-rocket")
                               ;; (+modeline-format-icon 'faicon "nf-fa-rocket" "" face label -0.0575)
                               " "))
      (add-to-list 'global-mode-string
                   '(t (:eval lsp-modeline-icon))
                   'append)))
  ;; (add-hook 'lsp-before-initialize-hook #'lsp-update-modeline)
  ;; (add-hook 'lsp-after-initialize-hook #'lsp-update-modeline)
  ;; (add-hook 'lsp-after-uninitialized-functions #'lsp-update-modeline)x
  ;; (add-hook 'lsp-before-open-hook #'lsp-update-modeline)
  ;; (add-hook 'lsp-after-open-hook #'lsp-update-modeline)
  :hook ((lsp-completion-mode . lsp-mode-setup-completion) ;; setup orderless completion style.
         (lsp-mode . lsp-enable-which-key-integration)
         (python-ts-mode . lsp-deferred)
         (clojure-ts-mode . lsp-deferred)
         (ocaml-ts-mode . lsp-deferred)
         (bash-ts-mode . lsp-deferred)
         (json-ts-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (toml-ts-mode . lsp-deferred)
         (markdown-ts-mode . lsp-deferred)
         )
  :custom
  (lsp-keymap-prefix "C-c c l")
  (lsp-enable-text-document-color t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-completion-provider :none) ;; we use corfu!
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind nil)
  (lsp-signature-auto-activate nil)
  (lsp-enable-folding nil)
  (lsp-enable-folding nil)
  :config
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
  )


(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-deferred . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-enable-which-key-integration t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-delay 1.25
        lsp-ui-doc-max-width 200
        lsp-ui-doc-max-height 200
        lsp-ui-peek-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(use-package consult-lsp
  :after (lsp-mode consult)
  :init
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lsp-treemacs-errors-list] #'consult-lsp-diagnostics))

;; =============================================================================
;; DAP
;; =============================================================================
;; (use-package dap-mode
;;   :straight t
;;   :hook (lsp-deferred . dap-mode)
;;   :custom
;;   (dap-auto-configure-mode t)
;;   (dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
;;   :config
;;   ;; Python
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   (dap-register-debug-template "Python Run"
;;                                (list :type "python"
;;                                      :args "-i"
;;                                      :cwd nil
;;                                      :env '(("DEBUG" . "1"))
;;                                      :target-module nil
;;                                      :request "launch"
;;                                      :name "Python Run"))
;;   (dap-register-debug-template "Python :: Attach to running process"
;;                                (list :type "python"
;;                                      :request "attach"
;;                                      :processId "${command:pickProcess}"
;;                                      :name "Python :: Attach to running process"))
;;   ;; OTHER
;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra)))
;;   )

;; =============================================================================
;; DAPE
;; =============================================================================
(use-package dape
  :preface (setq dape-key-prefix (kbd "C-c d"))
  :custom
  (dape-breakpoint-global-mode +1)
  (dape-buffer-windows-arrangement 'right)
  (dape-cwd-function #'projectile-project-root)
  :config
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)
  ;; Python Debug Config
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adpater")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default))
  )

(use-package repeat
  :ensure nil
  :custom
  (repeat-mode +1))

(provide '+code)
;;; +code.el ends here

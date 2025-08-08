;;; +keymaps.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: August 04, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package general
  :ensure t
  :straight t
  :demand t
  :config
  (general-create-definer global-leader-key
    :prefix "C-c")

  (general-create-definer local-leader-key
    :prefix "C-c l")

  (global-leader-key
    "a" 'embark-act

    ;; "c" (cons "code" (make-sparse-keymap))
    "c" '(:ignore t :wk "code")
    "c l" (cons "lsp" lsp-command-map)

    "d" (cons "debugger" (make-sparse-keymap))
    "d d" 'dap-hydra
    "d a" (cons "add-breakpoint" 'dap-breakpoint-add)

    "e" (cons "eval" (make-sparse-keymap))

    "f" '(:ignore t :wk "flycheck")
    "&" '(:ignore t :wk "yasnippet")

    "p" (cons "projectile" projectile-command-map)

    "o" (cons "open" (make-sparse-keymap))
    "o s" (cons "shells" (make-sparse-keymap))
    "o s e" 'eshell
    "o s v" 'vterm
    "o s p" (cons "vterm-in-project" 'projectile-run-vterm)
    "o t" (cons "treemacs" (make-sparse-keymap))
    "o t t" '("treemacs" . treemacs)
    "o t a" '("add-project" . treemacs-add-project-to-workspace)
    "o t r" '("remove-project" . treemacs-remove-project-from-workspace)

    "v" (cons "git/version-control" (make-sparse-keymap))
    "v g" 'magit-status
    "v b" 'magit-blame
    "v c" 'magit-clone
    "v d" 'magit-dispatch
    "v f" 'magit-file-dispatch
    "v l" 'magit-log-buffer-file
    )

  (local-leader-key 'python-ts-mode-map
    "" '(:ignore t :wk "leader")
    "t" (cons "test" (make-sparse-keymap))
    "t p" 'python-pytest-dispatch
    "t r" 'python-pytest-repeat
    "t f" 'python-pytest-file

    "i" (cons "import" (make-sparse-keymap))
    "i i" (cons "Insert missing" 'pyimport-insert-missing)
    "i r" (cons "Remove unused" 'pyimport-remove-unused)
    "i s" (cons "Sort" 'py-isort-buffer)

    "n" 'numpydoc-generate
    "c" '("conda-activate" . conda-env-activate))

  ;; (local-leader-key 'clojure-ts-mode-map)
  )



(provide '+keymaps)
;;; +keymaps.el ends here

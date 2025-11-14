;;; +keymaps.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; GENERAL KEYBINDINGS
;; =============================================================================

(use-package general
  :ensure (:wait t)
  :config
  (general-create-definer global-leader-key
    :prefix "C-c")

  (general-create-definer local-leader-key
    :prefix "C-c l")

  (global-leader-key
    "." (cons "embark" (make-sparse-keymap))
    ". a" '("embark act" . embark-act)
    ". d" '("embark dwim" . embark-dwim)
    ". b" '("embark bindings" . embark-bindings)


    "y" (cons "yasnippet" (make-sparse-keymap))
    "y i" '("insert" . yas-insert-snippet)
    "y d" '("describe" . yas-describe-tables)
    "y n" '("new" . yas-new-snippet)
    "y v" '("visit file" . yas-visit-snippet-file)

    "c" '(:ignore t :wk "code")
    ;; "c l" (cons "lsp" lsp-command-map)

    "d" '(:ignore t :wk "debugger")

    "f" '(:ignore t :wk "flycheck")

    "p" (cons "project" projectile-command-map)
    "p s" '(:ignore t :wk "search")
    "p 4" '(:ignore t :wk "find")
    "p 5" '(:ignore t :wk "find")
    "p c" '(:ignore t :wk "compile")
    "p x" '(:ignore t :wk "run")

    "o" (cons "open" (make-sparse-keymap))
    "o e" '("eshell" . my/eshell-toggle)
    "o v" '("vterm" . my/vterm-toggle)
    "o p" '("vterm-in-project" . projectile-run-vterm)
    "o t" (cons "treemacs" (make-sparse-keymap))
    "o t t" '("toggle" . treemacs)
    "o t a" '("add-project" . treemacs-add-project-to-workspace)
    "o t r" '("remove-project" . treemacs-remove-project-from-workspace)
    "o t c" '("show call hierarchy" . lsp-treemacs-call-hierarchy)
    "o t h" '("help" . treemacs-advanced-helpful-hydra)

    "O" (cons "Org" (make-sparse-keymap))
    "O a" '("agenda" . org-agenda)
    "O f" '("org capture file" . my/open-capture-file)
    "O l" '("link" . org-store-link)
    "O c" (cons "capture" (make-sparse-keymap))
    "O c c" '("capture" . org-capture)
    "O c j" '("capture JOURNAL ENTRY" . (lambda () (interactive) (org-capture nil "jj")))
    "O c n" '("capture QUICK NOTE" . (lambda () (interactive) (org-capture nil "nn")))
    "O c t" '("capture TASK" . (lambda () (interactive) (org-capture nil "tt")))

    "v" (cons "git/version-control" (make-sparse-keymap))
    "v g" 'magit-status
    "v b" 'magit-blame
    "v c" 'magit-clone
    "v d" 'magit-dispatch
    "v f" 'magit-file-dispatch
    "v l" 'magit-log-buffer-file)

  (local-leader-key 'emacs-lisp-mode-map
    "" '(:ignore t :wk "leader")
    "b" '("Compile Buffer" . elisp-byte-compile-buffer)
    "e" '("Eval Region/Buffer" . elisp-eval-region-or-buffer)
    "f" '("Compille File" . elisp-byte-compile-file))

  (local-leader-key 'org-mode-map
    "" '(:ignore t :wk "leader")
    "t" (cons "todo" (make-sparse-keymap))
    "t n" '("next" . my/org-todo-next)
    "t i" '("in progress" . my/org-todo-in-progress)
    "t w" '("waiting" . my/org-todo-waiting)
    "t d" '("done" . my/org-todo-done)
    "t c" '("cancel" . my/org-todo-cancel)
    "p" 'org-priority
    "e" 'org-set-effort
    )

  (local-leader-key 'markdown-mode-map
    "" '(:ignore t :wk "leader")
    "l" '("live preview" . markdown-live-preview-mode)
    "p" '("preview buffer" . +markdown-preview-buffer)
    "r" '("preview refresh" . +markdown-preview-refresh)
    )

  (local-leader-key 'python-ts-mode-map
    "" '(:ignore t :wk "leader")
    "t" (cons "test" (make-sparse-keymap))
    "t f" 'python-pytest-file
    "t p" 'python-pytest-dispatch
    "t r" 'python-pytest-repeat
    "C-t" '(:ignore t :wk "skeletons")
    "TAB" '(:ignore t :wk "python-imports")

    "i" (cons "import" (make-sparse-keymap))
    "i i" (cons "Insert missing" 'pyimport-insert-missing)
    "i r" (cons "Remove unused" 'pyimport-remove-unused)
    "i s" (cons "Sort" 'py-isort-buffer)

    "c" '("conda-activate" . conda-env-activate)
    "n" 'numpydoc-generate
    "p" (cons "poetry" 'poetry))

  ;; (local-leader-key 'clojure-ts-mode-map)
  )

(provide '+keymaps)
;;; +keymaps.el ends here

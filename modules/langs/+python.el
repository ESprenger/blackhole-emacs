;;; +python.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

;;; PYTHON


(defvar my/python-bindings
  (list
   '("C-c C-p" 'run-python)
   '("C-c C-j" 'imenu)))

(use-package python-mode
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)))

(defun process-python-keybind-remaps ()
  ;; Run
  (my/process-keybind-edit "C-c C-p" "C-c l C-p" 'run-python python-ts-mode-map)
  (my/process-keybind-edit "C-c C-j" "C-c l C-j" 'imenu-python python-ts-mode-map)
  ;; Indent
  (my/process-keybind-edit "C-c <" "C-c l <" 'python-indent-shift-left python-ts-mode-map)
  (my/process-keybind-edit "C-c >" "C-c l >" 'python-indent-shift-right python-ts-mode-map)
  ;; Shell Send
  (my/process-keybind-edit "C-c C-b" "C-c l C-b" 'python-shell-send-block python-ts-mode-map)
  (my/process-keybind-edit "C-c C-c" "C-c l C-c" 'python-shell-send-buffer python-ts-mode-map)
  (my/process-keybind-edit "C-c C-d" "C-c l C-d" 'python-describe-at-point python-ts-mode-map)
  (my/process-keybind-edit "C-c C-e" "C-c l C-e" 'python-shell-send-statement python-ts-mode-map)
  (my/process-keybind-edit "C-c C-f" "C-c l C-f" 'python-eldoc-at-point python-ts-mode-map)
  (my/process-keybind-edit "C-c C-l" "C-c l C-l" 'python-shell-send-file python-ts-mode-map)
  (my/process-keybind-edit "C-c C-r" "C-c l C-r" 'python-shell-send-region python-ts-mode-map)
  (my/process-keybind-edit "C-c C-s" "C-c l C-s" 'python-shell-send-string python-ts-mode-map)
  ;; Skeletong
  (my/process-keybind-edit "C-c C-t c" "C-c l C-t c" 'python-skeleton-class python-ts-mode-map)
  (my/process-keybind-edit "C-c C-t d" "C-c l C-t d" 'python-skeleton-def python-ts-mode-map)
  (my/process-keybind-edit "C-c C-t t" "C-c l C-t t" 'python-skeleton-try python-ts-mode-map)
  (unbind-key "C-c C-t f" python-ts-mode-map)
  ;; (my/process-keybind-edit  "C-c l C-t f" 'python-skeleton-for python-ts-mode-map)
  (unbind-key "C-c C-t i" python-ts-mode-map)
  ;; (my/process-keybind-edit "C-c C-t i" "C-c l C-t i" 'python-skeleton-if python-ts-mode-map)
  (unbind-key "C-c C-t m" python-ts-mode-map)
  ;; (my/process-keybind-edit "C-c C-t m" "C-c l C-t m" 'python-skeleton-import python-ts-mode-map)

  (unbind-key "C-c C-t w" python-ts-mode-map)
  ;; (my/process-keybind-edit "C-c C-t w" "C-c l C-t w" 'python-skeleton-while python-ts-mode-map)
  ;; Misc
  (my/process-keybind-edit "C-c C-v" "C-c l C-v" 'python-check python-ts-mode-map)
  (my/process-keybind-edit "C-c C-z" "C-c l C-z" 'python-shell-switch-to-shell python-ts-mode-map)
  ;; Imports
  (my/process-keybind-edit "C-c TAB a" "C-c l TAB a" 'python-add-import python-ts-mode-map)
  (my/process-keybind-edit "C-c TAB f" "C-c l TAB a" 'python-fix-imports python-ts-mode-map)
  (my/process-keybind-edit "C-c TAB r" "C-c l TAB r" 'python-remove-import python-ts-mode-map)
  (my/process-keybind-edit "C-c TAB s" "C-c l TAB s" 'python-sort-imports python-ts-mode-map)
  )

(add-hook 'python-ts-mode-hook 'process-python-keybind-remaps)

;; (add-hook 'python-ts-mode-hook (lambda () (local-set-key (kbd "C-c l") python-ts-mode-map)))

;; (defun my/python-keybinds ()
;;   (let (my-keybinds '('("C-c C-p" "C-c l C-p" 'run-python)
;;                       '("C-c C-j" "C-c l C-j" 'imenu)))
;;     (dolist (n-entry my-keybinds)
;;       (my/process-keybind-edit (nth 0 n-entry) (nth 1 n-entry) (nth 2 n-entry) python-ts-mode-map))))

;; (add-hook 'python-ts-mode-hook (lambda () (my/process-keybind-edit "C-c C-p" "C-c l C-p" 'run-python python-ts-mode-map)))
;; (add-hook 'python-ts-mode-hook (lambda () (my/python-keybinds)))

(use-package conda
  :demand t
  :custom
  (conda-anaconda-home "~/anaconda3")
  (conda-env-home-directory "~/anaconda3")
  (conda-env-subdirectory "envs")
  :config
  (progn
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    (conda-env-activate "base"))
  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))

(use-package numpydoc
  :demand t
  :after python)

(use-package python-pytest
  :demand t
  :after python)

(use-package pyimport
  :demand t
  :after python)

(use-package py-isort
  :demand t
  :after python)

(use-package poetry
  :demand t)

(use-package lsp-pyright
  :hook (python-ts-mode . (lambda ()
			                (require 'lsp-pyright)
			                (lsp-deferred)))
  :config
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
  (setq lsp-clients-python-library-directories '("/usr/bin/python3" "~/anaconda3/pkgs"))
  (setq lsp-pyright-venv-path "~/anaconda3/envs")
  (setq lsp-pyright-disable-language-service nil)
  (setq lsp-pyright-disable-organize-imports t)
  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-auto-import-completions t))


(provide '+python)
;;; +python.el ends here

;;; +langs.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

;;; PYTHON
(setopt python-shell-interpreter "~/anaconda3/bin/python") ;; or ~/anaconda3/bin/python or python

(use-package conda
  :straight t
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
  :straight t
  :after python)

(use-package python-pytest
  :straight t
  :after python)

(use-package pyimport
  :straight t
  :after python)

(use-package py-isort
  :straight t
  :after python)

(use-package lsp-pyright
  :straight t
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

;;; CLOJURE

;;; OCAML
;; (add-to-list 'load-path "~/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

;; (use-package tuareg
;;   :straight t
;;   :custom
;;   (tuareg-opam-insinuate t))
;;
;; (use-package dune-format
;;   :straight t)

(provide '+langs)
;;; +langs.el ends here

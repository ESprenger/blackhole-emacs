;;; +yasnippet.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; YASNIPPET
;; =============================================================================
(use-package yasnippet
  :demand t
  :commands
  (yas-minor-mode-on
   yas-expand
   yas-expand-snippet
   yas-lookup-snippet
   yas-insert-snippet
   yas-new-snippet
   yas-visit-snippet-file
   yas-activate-extra-mode
   yas-deactivate-extra-mode
   yas-maybe-expand-abbrev-key-filter)
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  :custom (yas-use-menu 'abbreviate)
  ;; (yas-snippet-dirs '(user-snippets-dir))
  :config
  ;; Bind in +keymaps.el
  (unbind-key "C-c & C-n" yas-minor-mode-map)
  (unbind-key "C-c & C-s" yas-minor-mode-map)
  (unbind-key "C-c & C-v" yas-minor-mode-map))

;; =============================================================================
;; DOOM SNIPPETS
;; =============================================================================
(use-package doom-snippets
  :ensure (:type git :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
  :after yasnippet)

;; =============================================================================
;; AUTOINSERT
;; =============================================================================
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-query nil)
  (auto-insert-directory user-templates-dir)
  (auto-insert-mode 1)
  :config
  (add-hook 'find-file-hook 'auto-insert)
  (define-auto-insert "\\.el$" ["default-elisp.el" autoinsert-yas-expand])
  (define-auto-insert "\\.sh$" ["default-sh.el" autoinsert-yas-expand])
  (define-auto-insert "main.py$" ["main-py.el" autoinsert-yas-expand]))

;; =============================================================================
;; COMPLETION AT POINT
;; =============================================================================
(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide '+yasnippet)
;;; +yasnippet.el ends here

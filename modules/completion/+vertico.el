;;; +vertico.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; VERTICO
;; =============================================================================
;; In minibuffer completion, such as when entering commands or file paths,
;; Vertico helps by showing a dynamic list of potential completions, making
;; it easier to choose the correct one without typing out the entire string.

(use-package vertico
  :demand t
  :custom
  (vertico-count 13)                    ;; Number of candidates to display in the completion list.
  (vertico-cycle t)                     ;; Do not cycle through candidates when reaching the end of the list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char))
  :config
  (defun my/vertico-sort-cat-alpha (files)
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (setq vertico-sort-function #'my/vertico-sort-cat-alpha)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
                   "  ")
                 cand)))
  :init
  (vertico-mode))


(provide '+vertico)
;;; +vertico.el ends here

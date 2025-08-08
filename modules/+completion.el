;;; +completion.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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


;;; CORFU
;; Corfu Mode provides a text completion framework for Emacs.
(use-package corfu
  :straight t
  :commands (corfu-mode global-corfu-mode)
  :custom
  (corfu-auto t)                         ;; Only completes when hitting TAB
  (corfu-cycle t)
  (corfu-auto-prefix 1)                  ;; Trigger completion after typing 1 character
  (corfu-separator ?\s)
  (corfu-min-width 80)
  (corfu-max-width 80)                   ;; Always have the same width
  (corfu-scroll-margin 5)                ;; Margin when scrolling completions
  (corfu-popupinfo-delay nil)            ;; Delay before showing documentation popup
  (text-mode-ispell-word-completion nil) ;; Disable Ispell completion function.
  ;; (read-extended-command-predicate #'command-completion-default-include-p) ;; Hide commands in M-x which do not apply to the current mode.
  (corfu-echo-documentation nil)         ;; Already use corfu-doc
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package nerd-icons-corfu
;;   :straight t
;;   :after corfu
;;   ;; :after (:all corfu)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



;;; CAPE
;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu
(use-package cape
  :straight t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; VERTICO
;; In minibuffer completion, such as when entering commands or file paths,
;; Vertico helps by showing a dynamic list of potential completions, making
;; it easier to choose the correct one without typing out the entire string.
(use-package vertico
  :straight t
  :hook
  (after-init . vertico-mode)           ;; Enable vertico after Emacs has initialized.
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
                 cand))))


;;; ORDERLESS
;; Orderless enhances completion in Emacs by allowing flexible pattern matching.
(use-package orderless
  :straight t
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))


;;; MARGINALIA
;; Marginalia enhances the completion experience in Emacs by adding
;; additional context to the completion candidates.
(use-package marginalia
  :straight t
  :hook
  (after-init . marginalia-mode))


;;; CONSULT
;; Consult provides powerful completion and narrowing commands for Emacs.
(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; Drop-in Replacements
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s s" . isearch-query-replace)    ; Alternative: rebind C-s to use
         ("C-s" . consult-line)               ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi)       ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         )
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results.
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))


;;; EMBARK-CONSULT
;; Embark-Consult provides a bridge between Embark and Consult
(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.


;;; EMBARK
;; Embark provides a powerful contextual action menu for Emacs
(use-package embark
  :straight t
  :after embark-consult
  :bind (("C-c a" . embark-act)         ;; pick some comfortable binding
         ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings`
  )


;;; NERD ICONS COMPLETION
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide '+completion)
;;; +completion.el ends here

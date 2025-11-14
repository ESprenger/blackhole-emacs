;;; +markdown.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; JSON
;; =============================================================================
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'lsp-deferred)


;; =============================================================================
;; YAML
;; =============================================================================
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-hook 'yaml-ts-mode-hook #'lsp-deferred)


;; =============================================================================
;; MARKDOWN
;; =============================================================================
(use-package markdown-mode
  :demand t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc -f gfm -t html5 --mathjax --highlight-style=github")
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . +markdown-setup)
         (gfm-mode . visual-line-mode)
         (gfm-mode . +markdown-setup))
  :custom
  ;; Core settings for inline rendering
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.8 1.5 1.3 1.1 1.0 1.0))
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  ;; Built-in live preview settings
  (markdown-split-window-direction 'right)  ; Preview on right side
  :config
  ;; Keybindings
  ;; (define-key markdown-mode-map (kbd "C-c C-c l") #'markdown-live-preview-mode)
  ;; (define-key markdown-mode-map (kbd "C-c C-c p") #'funmacs-markdown-preview-buffer)
  ;; (define-key markdown-mode-map (kbd "C-c C-c r") #'funmacs-markdown-preview-refresh)
  )

;; Setup function for markdown mode
(defun +markdown-setup ()
  "Setup markdown with inline rendering."
  (setq-local line-spacing 0.2)

  ;; Enable markup hiding after buffer loads
  (run-with-idle-timer 0.1 nil
                       (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (when (or (eq major-mode 'markdown-mode)
                                       (eq major-mode 'gfm-mode))
                               (markdown-toggle-markup-hiding 1)))))
                       (current-buffer))

  ;; Display inline images if in graphical environment
  (when (display-graphic-p)
    (run-with-idle-timer 0.2 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (when (or (eq major-mode 'markdown-mode)
                                         (eq major-mode 'gfm-mode))
                                 (markdown-toggle-inline-images)))))
                         (current-buffer))))

;; GitHub-style preview in Emacs buffer using shr
(defun +markdown-preview-buffer ()
  "Preview markdown in Emacs buffer with GitHub styling."
  (interactive)
  (let ((filename buffer-file-name)
        (preview-buffer "*Markdown Preview*"))
    (message "Rendering GitHub-style Markdown preview...")
    ;; Generate HTML with GitHub styling
    (shell-command-on-region
     (point-min)
     (point-max)
     (concat "pandoc -f gfm -t html5 --standalone --mathjax "
             "--highlight-style=github "
             "--css=https://cdn.jsdelivr.net/npm/github-markdown-css@5.5.0/github-markdown-dark.min.css "
             "--metadata title=\"Markdown Preview\"")
     preview-buffer)
    ;; Render in other window
    (save-selected-window
      (switch-to-buffer-other-window preview-buffer)
      (let ((document (libxml-parse-html-region (point-min) (point-max)))
            (url (concat "file://" (or filename default-directory))))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))
        (read-only-mode 1)))
    (message "Preview rendered in %s" preview-buffer)))

;; Auto-refresh preview
(defun +markdown-preview-refresh ()
  "Refresh the markdown preview buffer if it exists."
  (interactive)
  (when (get-buffer "*Markdown Preview*")
    (+markdown-preview-buffer)))

;; Auto-refresh on save
(defun +markdown-auto-refresh-preview ()
  "Auto-refresh preview on save."
  (when (and (or (eq major-mode 'markdown-mode)
                 (eq major-mode 'gfm-mode))
             (get-buffer "*Markdown Preview*"))
    (+markdown-preview-buffer)))

(add-hook 'after-save-hook #'+markdown-auto-refresh-preview)

;; Reveal markup on current line when editing
(defvar +markdown-current-line '(0 . 0)
  "(start . end) of current line in current buffer.")
(make-variable-buffer-local '+markdown-current-line)

(defun +markdown-unhide-current-line (limit)
  "Font-lock function to reveal markup on current line."
  (let ((start (max (point) (car +markdown-current-line)))
        (end (min limit (cdr +markdown-current-line))))
    (when (< start end)
      (remove-text-properties start end
                              '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

(defun +markdown-refontify-on-linemove ()
  "Post-command-hook to refontify when moving lines."
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car +markdown-current-line)))))
    (setq +markdown-current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 3))))

(defun +markdown-enable-smart-hiding ()
  "Enable markup hiding with reveal-on-edit behavior."
  (interactive)
  (markdown-toggle-markup-hiding 1)
  (font-lock-add-keywords nil '((+markdown-unhide-current-line)) t)
  (add-hook 'post-command-hook #'+markdown-refontify-on-linemove nil t)
  (font-lock-flush))

;; Add to markdown hook
(add-hook 'markdown-mode-hook #'+markdown-enable-smart-hiding)
(add-hook 'gfm-mode-hook #'+markdown-enable-smart-hiding)

;; Valign for table alignment
(use-package valign
  :demand t
  :hook ((markdown-mode . valign-mode)
         (gfm-mode . valign-mode))
  :custom
  (valign-fancy-bar t))

;; Prettier heading faces
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-2 ((t (:height 1.4 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-3 ((t (:height 1.2 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-4 ((t (:height 1.15 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-5 ((t (:height 1.1 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-6 ((t (:height 1.05 :weight semi-bold :foreground "#79c0ff")))))

(provide '+markdown)
;;; +markdown.el ends here

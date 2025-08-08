;;; +shells.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: July 22, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; ESHELL
(defmacro with-read-only-face (str &rest properties)
  "Add face PROPERTIES to STR."
  (declare (indent 1))
  `(propertize ,str 'face (list ,@properties) 'read-only t 'rear-nonsticky '(read-only)))

(defun eshell-git-prompt-powerline-venv ()
  (let ((segment-separator "\xe0b0")
        (branch            "\xe0a0")
        (detached          "\x27a6")
        (cross             "\x2718")
        dir git git-face)
    (setq dir
          (propertize
           (concat
            " "
            (unless (eshell-git-prompt-exit-success-p)
              (concat cross " "))
            (eshell-git-prompt-powerline-dir)
            " ")
           'face 'eshell-git-prompt-powerline-dir-face 'read-only t))
    (setq git
          (when (eshell-git-prompt--git-root-dir)
            (setq git-face
                  (if (eshell-git-prompt--collect-status)
                      'eshell-git-prompt-powerline-not-clean-face
                    'eshell-git-prompt-powerline-clean-face))
            (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
            (propertize
             (concat " "
                     (-if-let (branch-name eshell-git-prompt-branch-name)
                         (concat branch " " branch-name)
                       (concat detached " "(eshell-git-prompt--commit-short-sha)))
                     " ")
             'face git-face 'read-only t)))
    (concat
     ;; This is the actuall important stuff, everthing else is the same
     (when conda-env-current-name
       (concat
        (with-read-only-face conda-env-current-name
          :background "#EB6134")
        (with-read-only-face segment-separator
          :foreground "#EB6134"
          :background (face-background 'eshell-git-prompt-powerline-dir-face))))
     (if git
         (concat dir
                 (with-read-only-face segment-separator
                   :foreground (face-background 'eshell-git-prompt-powerline-dir-face)
                   :background (face-background git-face))
                 git
                 (with-read-only-face segment-separator
                   :foreground (face-background git-face)))
       (concat dir
               (with-read-only-face segment-separator
                 :foreground (face-background 'eshell-git-prompt-powerline-dir-face))))
     (with-read-only-face (concat "\n" segment-separator)
       :foreground (face-background 'eshell-git-prompt-powerline-dir-face))
     (propertize "$" 'invisible t 'read-only t)
     (with-read-only-face " "))))

(defconst eshell-git-prompt-powerline-venv-regexp "^[^$\n]*\\\$ ")


(use-package eshell
  :ensure nil
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-history-size         100000
          eshell-buffer-maximum-lines 100000
          eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t
          eshell-scroll-to-bottom-on-output t)
    (setq eshell-visual-commands '("htop" "top" "tail" "ssh"))))

(use-package eshell-git-prompt
  :ensure t
  :straight t
  :after eshell
  :config
  (add-to-list 'eshell-git-prompt-themes
               '(powerline-plus
                 eshell-git-prompt-powerline-venv
                 eshell-git-prompt-powerline-regexp))
  (eshell-git-prompt-use-theme 'powerline-plus))


;;; VTERM
(use-package vterm
  :ensure t
  :straight t
  :commands vterm
  :config
  (add-to-list 'load-path (concat user-emacs-directory "straight/build/vterm/")))


;;; POPPER
(use-package popper
  :straight t
  :bind (("M-o"   . popper-toggle)
         :map popper-mode-map
         ("M-n" . popper-cycle)
         ("M-p" . popper-cycle-backwards))
  :init
  (setq popper-reference-buffers
        '("^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*vterm.*\\*$"  vterm-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(provide '+shells)
;;; +shells.el ends here

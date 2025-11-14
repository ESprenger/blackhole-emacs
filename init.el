;;; init.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Evan Sprenger
;;
;; Author: Evan Sprenger <evan.sprenger@gmail.com>
;; Created: July 31, 2025
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Bootstraps `elpaca.el'
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(setq debug-on-error t)

(defun +load-packages (dir modules)
  (dolist (module modules)
    (load (expand-file-name (concat dir module)))))

(defvar funcs-module-list
  '("+edit_keybindings"
    "+popup"))

(defvar core-module-list
  '(;; "+packages"
    "+defaults"
    "+git"
    "+help"
    "+undo"
    "+buffer"
    "+treesitter"
    "+formatting"
    "+dir"
    "+yasnippet"
    "+project"
    "+code"
    "+treemacs"
    ))

(defvar completion-module-list
  '("+vertico"
    "+orderless"
    "+embark"
    "+consult"
    "+marginalia"
    "+corfu"
    "+cape"
    ))

(defvar langs-module-list
  '("+python"
    "+markdown"
    ;; "+ocaml"
    ;; "+clojure"
    ;; "+cpp"
    "+latex"
    "+terminals"
    ))

(defvar org-module-list
  '("+org"
    "+org-modern"
    "+org-agenda"
    "+org-todo"
    "+org-capture"))

(defvar closing-module-list
  '("+ui"
    "+keymaps"))


(+load-packages funcs-modules-dir funcs-module-list)
(+load-packages core-modules-dir core-module-list)
(+load-packages completion-modules-dir completion-module-list)
(+load-packages langs-modules-dir langs-module-list)
(+load-packages org-modules-dir org-module-list)
(+load-packages core-modules-dir closing-module-list)

(setq custom-file (expand-file-name "+custom.el" core-modules-dir))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file  ;; 'noerror
                                                   )))

(provide 'init)
;;; init.el ends here

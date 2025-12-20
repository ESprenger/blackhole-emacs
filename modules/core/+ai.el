;;; +ai.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

(use-package gptel
  :demand t
  :config
  (setq gptel-default-mode 'org-mode
        gptel-model "deepseek-coder:33b"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(deepseek-coder:33b)))
  (add-to-list 'display-buffer-alist
               '("\\Ollama\*"
    	         (display-buffer-in-side-window)
    	         (window-width . 0.40)
    	         (side . right)
    	         (slot . 0)))
  )

(defun my/start-ollama ()
  (gptel "Ollama"))

(defun my/ollama-toggle ()
  (interactive)
  ;; (my/hide-or-call-buffer "Ollama" #'my/start-ollama)
  (my/hide-or-call-buffer "Ollama" (lambda () (gptel "Ollama"))))


(provide '+ai)
;;; +ai.el ends here

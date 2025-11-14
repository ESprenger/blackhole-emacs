;;; +edit_keybindings.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

(defun my/add-local-leader (keybind)
  (concat (substring keybind 0 3) " l " (substring keybind 4)))

(defun my/process-keybind-edit (old-keybind new-keybind func mode-map)
  (unbind-key old-keybind mode-map)
  (define-key mode-map (kbd new-keybind) func))

(defun my/remap-major-keybinds (keybinds mode-map)
  (dolist (var keybinds)
    (let* ((old-keybind (car var))
           (new-keybind (my/add-local-leader old-keybind))
           ;; (func (car (cdr var)))
           )
      (my/process-keybind-edit old-keybind new-keybind (car (cdr var)) mode-map)
      )))


(provide '+edit_keybindings)
;;; +edit_keybindings.el ends here

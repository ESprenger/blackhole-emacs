;;; +popup.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

(defun buffer-exists (buffername) (buffer-live-p (get-buffer buffername)))

(defun my/buffer-pop-hide (name kill-buffer-true)
  "Hide the existing pop up window with named NAME."
  (let ((buffer (get-buffer-window name)))
    (delete-window (select-window buffer))
    (if kill-buffer-true
        (kill-buffer name))))

(defun my/buffer-pop-toggle (name func &optional kill-buffer-true)
  "Toggle buffer pop up window."
  (if (buffer-exists name)
      (if (get-buffer-window name)
          (my/buffer-pop-hide name kill-buffer-true)
        (funcall func))
    (funcall func)))

(defun my/ibuffer-toggle ()
  (interactive)
  (my/buffer-pop-toggle "*Ibuffer*" #'ibuffer))

(defun my/eshell-toggle ()
  (interactive)
  (my/buffer-pop-toggle "*eshell*" #'eshell))

(defun my/vterm-toggle ()
  (interactive)
  (my/buffer-pop-toggle "*vterm*" #'vterm))

(defun my/snippet-table-describe-toggle ()
  (interactive)
  (my/buffer-pop-toggle "*YASnippet Tables*" #'yas-describe-tables t))




(provide '+popup)
;;; +popup.el ends here

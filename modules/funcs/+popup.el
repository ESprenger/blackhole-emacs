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


(defun my/hide-or-call-buffer (name creation-fn &optional kill-buffer-true)
  "Hide or call CREATION-FN with NAME."
  (interactive "sBuffer name:\nsFunction to create buffer as bottom/side window: ")
  (if (get-buffer name)
      ;; Buffer already exists, hide it if in window.
      (let ((window-name (get-buffer-window name)))
        (if window-name
            (progn
              (delete-window window-name)
              (if kill-buffer-true
                  (kill-buffer name)))
          (switch-to-buffer (get-buffer name))))
    ;; Buffer doesn't exist, call the creation function and show it.
    (funcall creation-fn)
    (switch-to-buffer (get-buffer name))))


(provide '+popup)
;;; +popup.el ends here

;;; +treesitter.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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

(setq treesit-language-source-alist
      '(;; Core Languages - Official tree-sitter repos (100% verified)
        (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (java . ("https://github.com/tree-sitter/tree-sitter-java"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))

        ;; tree-sitter-grammars organization (verified)
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
        (toml . ("https://github.com/tree-sitter-grammars/tree-sitter-toml"))
        (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))

        ;; Community maintained (tested and working)
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        ))

(defun myemacs--compiler-available-p ()
  (or (executable-find "gcc")
      (executable-find "clang")
      (executable-find "cc")))

(defun myemacs-install-missing-grammars ()
  "Install all missing tree-sitter grammars with detailed error reporting."
  (interactive)
  (if (myemacs--compiler-available-p)
      (let ((installed 0)
            (failed 0)
            (skipped 0)
            (failed-langs '()))
        (dolist (grammar treesit-language-source-alist)
          (let ((lang (car grammar)))
            (if (treesit-language-available-p lang)
                (progn
                  (setq skipped (1+ skipped))
                  (message "[Emacs] ✓ Grammar for %s already installed" lang))
              (condition-case err
                  (progn
                    (message "[Emacs] Installing tree-sitter grammar for %s..." lang)
                    (treesit-install-language-grammar lang)
                    (setq installed (1+ installed))
                    (message "[Emacs] ✓ Successfully installed %s" lang))
                (error
                 (setq failed (1+ failed))
                 (push lang failed-langs)
                 (message "[Emacs] ✗ Failed to install %s: %s" lang (error-message-string err)))))))
        (message "[Emacs] ========================================")
        (message "[Emacs] Installation complete: %d installed, %d failed, %d already present"
                 installed failed skipped)
        (when failed-langs
          (message "[Emacs] ⚠ Failed languages: %s" (mapconcat #'symbol-name (reverse failed-langs) ", ")))
        (message "[Emacs] ========================================"))
    (message "[Emacs] No C compiler found; skipping tree-sitter installs.")))

;; attempt install at startup but safe
(ignore-errors (myemacs-install-missing-grammars))

(setq major-mode-remap-alist
      '(;; System & Shell
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)

        ;; C Family
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)

        ;; Web Development
        (json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (toml-mode . toml-ts-mode)
        (conf-toml-mode . toml-ts-mode)

        ;; Scripting Languages
        (python-mode . python-ts-mode)

        ;; Systems Programming
        (rust-mode . rust-ts-mode)
        (rustic-mode . rust-ts-mode)

        ;; Functional Languages
        (ocaml-mode . ocaml-ts-mode)

        ;; JVM Languages
        (java-mode . java-ts-mode)

        ;; Build Systems & Config
        (cmake-mode . cmake-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)

        ;; Documentation & Markup
        (markdown-mode . markdown-ts-mode)
        (gfm-mode . markdown-ts-mode)

        ;; Other
        (sql-mode . sql-ts-mode)))


(provide '+treesitter)
;;; +treesitter.el ends here

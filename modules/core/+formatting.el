;;; +formatting.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; RAINBOW-DELIMITERS
;; =============================================================================
(use-package rainbow-delimiters
  :demand t
  :hook (prog-mode . rainbow-delimiters-mode))

;; =============================================================================
;; INDENT-GUIDE
;; =============================================================================
;; The `indent-guide' package provides visual indicators for indentation levels

(use-package indent-guide
  :demand t
  :hook
  (prog-mode . indent-guide-mode))

;; =============================================================================
;; STRIP WHITESPACE
;; =============================================================================
(use-package stripspace
  :demand t
  :commands stripspace-local-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))


;; =============================================================================
;; DOTENV
;; =============================================================================
;; A simple major mode to provide .env files with color highlighting
(use-package dotenv-mode
  :demand t)

;; =============================================================================
;; APHELEIA
;; =============================================================================
(use-package apheleia
  :demand t
  :config
  ;; ============================================================
  ;; Systems Programming Languages
  ;; ============================================================

  ;; C/C++/Objective-C - clang-format (built-in)
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--assume-filename" filepath))
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)

  ;; Rust - rustfmt (built-in)
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rustic-mode apheleia-mode-alist) 'rustfmt)

  ;; ============================================================
  ;; Web Development Languages
  ;; ============================================================

  ;; ============================================================
  ;; Hybrid Approach: Biome (Fast) + Prettier (Comprehensive)
  ;; ============================================================

  ;; ──────────────────────────────────────────────────────────
  ;; Biome - Use for JS/TS/JSX/TSX/JSON (25x faster)
  ;; ──────────────────────────────────────────────────────────

  (setf (alist-get 'biome apheleia-formatters)
	    '("biome" "format"
          "--indent-style=space"
          "--indent-width=2"
          "--stdin-file-path" filepath))

  ;; JSON/JSONC with Biome
  (setf (alist-get 'json-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'jsonc-mode apheleia-mode-alist) 'biome)

  ;; ──────────────────────────────────────────────────────────
  ;; Prettier - Use for everything else
  ;; ──────────────────────────────────────────────────────────

  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  ;; HTML/XML
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'mhtml-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'xml-mode apheleia-mode-alist) 'prettier)

  ;; YAML (not supported by Biome)
  (setf (alist-get 'yaml-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier)

  ;; Markdown (not supported by Biome)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'gfm-mode apheleia-mode-alist) 'prettier)

  ;; ──────────────────────────────────────────────────────────
  ;; Optional: Prettier with Tailwind plugin
  ;; ──────────────────────────────────────────────────────────

  (setf (alist-get 'prettier-tailwind apheleia-formatters)
        '("prettier" "--plugin" "prettier-plugin-tailwindcss"
          "--stdin-filepath" filepath))

  ;; Uncomment to use Tailwind-aware formatting:
  ;; (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier-tailwind)
  ;;

  ;; ============================================================
  ;; Python
  ;; ============================================================

  ;; Ruff - Modern, fast formatter (Black-compatible) with import sorting
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--silent" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--fix" "--silent" "--stdin-filename" filepath "-"))

  ;; Use Ruff for both import sorting and formatting (chained)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))

  ;; ============================================================
  ;; Functional Languages
  ;; ============================================================

  ;; OCaml - ocamlformat (built-in)
  (setf (alist-get 'ocamlformat apheleia-formatters)
        '("ocamlformat" "-" "--name" filepath "--enable-outside-detected-project"))
  (setf (alist-get 'caml-mode apheleia-mode-alist) 'ocamlformat)
  (setf (alist-get 'tuareg-mode apheleia-mode-alist) 'ocamlformat)

  ;; Lisp - lisp-indent (built-in)
  (setf (alist-get 'lisp-indent apheleia-formatters)
        'apheleia-indent-lisp-buffer)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)
  (setf (alist-get 'lisp-mode apheleia-mode-alist) 'lisp-indent)
  (setf (alist-get 'scheme-mode apheleia-mode-alist) 'lisp-indent)

  ;; ============================================================
  ;; JVM Languages
  ;; ============================================================

  ;; Java - google-java-format (built-in)
  ;; (setf (alist-get 'google-java-format apheleia-formatters)
  ;;       '("google-java-format" "-"))
  ;; (setf (alist-get 'java-mode apheleia-mode-alist) 'google-java-format)
  ;; (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format)

  ;; ============================================================
  ;; Shell
  ;; ============================================================

  ;; Shell/Bash - shfmt (built-in)
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i" "2" "-"))
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)

  ;; ============================================================
  ;; Configuration/Markup Languages
  ;; ============================================================

  ;; TOML - prettier with plugin (built-in)
  ;; (setf (alist-get 'prettier-toml apheleia-formatters)
  ;;       '("prettier" "--plugin" "prettier-plugin-toml" "--stdin-filepath" filepath))
  ;; (setf (alist-get 'toml-mode apheleia-mode-alist) 'prettier-toml)
  ;; (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'prettier-toml)
  (setf (alist-get 'taplo apheleia-formatters)
	    '("taplo" "fmt" "-"))
  (setf (alist-get 'toml-mode apheleia-mode-alist) 'taplo)
  (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)


  ;; LaTeX - latexindent (built-in)
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "--logfile=/dev/null"))
  (setf (alist-get 'latex-mode apheleia-mode-alist) 'latexindent)
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist) 'latexindent)

  ;; Markdown - prettier (built-in)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)

  ;; ============================================================
  ;; Hardware Description Languages
  ;; ============================================================

  ;; Verilog - iStyle (built-in)
  (setf (alist-get 'istyle-verilog apheleia-formatters)
        '("iStyle"))
  (setf (alist-get 'verilog-mode apheleia-mode-alist) 'istyle-verilog)

  ;; ============================================================
  ;; Configuration Options
  ;; ============================================================

  :custom
  ;; Respect Emacs indentation settings
  (apheleia-formatters-respect-indent-level t)
  ;; Hide log buffers from buffer list
  (apheleia-hide-log-buffers t)
  ;; Only log errors
  (apheleia-log-only-errors t)
  ;; Remote file formatting
  (apheleia-remote-algorithm 'local)
  :init
  ;; Enable globally
  (apheleia-global-mode +1))

(provide '+formatting)
;;; +formatting.el ends here

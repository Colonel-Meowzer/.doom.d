;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; pep8 formatting on save
(package! py-autopep8)

;; (SPC o D) for quick access to docker-compose
(package! docker-compose-mode)

;; allows for multiple cursors extra for mass editing
(package! evil-mc-extras)

;; add reST docstrings for python via sphinx
(package! sphinx-doc)

(package! org-bullets)

(package! docker-tramp)

(package! multiple-cursors)

;; poly-mode for rmd-mode
(package! poly-R)
(package! poly-markdown)

;; docstring template for numpy-style docs
(package! numpydoc)
(package! zen-mode)

;; a better syntax highlighter
(package! tree-sitter)
(package! tree-sitter-langs)

(package! ejc-sql)

(package! polymode)
(package! jinja2-mode)
 (package! dbt-mode :recipe
   (:host github :repo "Colonel-Meowzer/dbt-mode"))


(package! org-download)
(package! org-ref)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

;; https://github.com/org-roam/org-roam-ui/README.md
(unpin! org-roam)
(package! org-roam-ui)

(package! logview)

(package! ox-pandoc)

(package! chatgpt-shell)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! dbt-mode
  :recipe (:host github :repo "CyberShadow/dbt-mode"))
(package! code-cells
  :recipe (:host github :repo "astoff/code-cells.el"))
(package! jupyter)

(package! nvm)

(package! direnv)

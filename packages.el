;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (SPC o D) for quick access to docker-compose
(package! docker-compose-mode)

;; allows for multiple cursors extra for mass editing
(package! evil-mc-extras)

;; add reST docstrings for python via sphinx
(package! sphinx-doc)

(package! org-bullets)

(package! docker-tramp)

;; poly-mode for rmd-mode
(package! poly-R)
(package! poly-markdown)

;; docstring template for numpy-style docs
(package! numpydoc)


(package! polymode)
(package! jinja2-mode)

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
(package! code-cells
  :recipe (:host github :repo "astoff/code-cells.el"))
(package! jupyter)

(package! nvm)

(package! company-org-block)

(package! lsp-ui)

;;(package! emacs-direnv)

(package! orderless)

(package! ob-async)
(package! org-pretty-table :recipe(:host github :repo "Fuco1/org-pretty-table" :files ("*.el")))

(package! mc-extras)

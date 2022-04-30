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

;; a better syntax highlighter
(package! tree-sitter)
(package! tree-sitter-langs)

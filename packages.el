;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Python IDE-like experience
(package! elpy)

;; syntax checker
(package! flycheck)

;; pep8 formatting on save
(package! py-autopep8)

;; (SPC o D) for quick access to docker-compose
(package! docker-compose-mode)

;; allows for multiple cursors extra for mass editing
(package! evil-mc-extras)

;; add reST docstrings for python via sphinx
(package! sphinx-doc)

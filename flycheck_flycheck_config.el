;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(def-package! elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(elpy-enable)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

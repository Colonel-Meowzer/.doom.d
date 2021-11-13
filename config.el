;;; ~/.doom.d/config.el -- Base Configs

;; Place your private configuration here
(load-theme 'wheatgrass t)

;; enable shift-select in org mode
(setq org-support-shift-select t)

;; Set up elpy minor mode to be enabled when for python-mode activated.
;; This needs to be done before elpy-enable
;;
;; Most of these configs were taken directly from Elpy documentation.

;; autocode completetion via autopep8 on save. It's pretty sweet.
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; use jupyter as the interactive shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --no-color-info"
      python-shell-prompt-detect-failure-warning nil
      )
;; Set Java 8 for using Apache Spark on MacOS
;;(setenv "JAVA_HOME"
;;        (shell-command-to-string "echo $(/usr/libexec/java_home -v 1.8) | tr -d '\n'"))

;; org-confluence export
;; (require 'org-confluence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(require 'org)
(require 'ox-latex)
;; "minted" is a python library that allows for syntax highlighting when
;; exporting to pdf. This needs to be installed in the main python environment.
;;
;; The following configurations enable this functionality.
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; I forgot what these do.
(setq org-src-fontify-natively t)

;; enable execution of 'dot' sourceblocks in org-mode so we can generate simple diagrams.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (dot . t)))

;; display the time on all buffers by default
;;(display-time)

(setq org-roam-directory "~/org")
(after! python-mode
  (set-ligatures! 'python-mode
    ;; Functional
    :lambda        "lambda"
    :def           "def"
    :map           "dict"
    ;; Types
    :null          "None"
    :true          "True"
    :false         "False"
    :int           "int"
    :float         "float"
    :str           "str"
    :bool          "bool"
    :list          "list"
    ;; Flow
    :not           "not"
    :in            "in"
    :not-in        "not in"
    :and           "and"
    :or            "or"
    :for           "for"
    :some          "some"
    :return        "return"
    :yield         "yield"
    ;; Other
    :tuple         "tuple"))

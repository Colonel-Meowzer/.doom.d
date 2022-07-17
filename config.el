;;; ~/.doom.d/config.el -- Base Configs

;; Place your private configuration here
(load-theme 'doom-zenburn t)

;; make sure numpydoc is available for python-mode
(use-package numpydoc
  :ensure t
  :after python
  :bind (:map python-mode-map
         ("C-c C-n" . numpydoc-generate)))


;; Add directories with .pyroot to the python path
;; this is to allow working in mono-repos for python-pytest
;; https://github.com/wbolster/emacs-python-pytest
(add-hook 'python-mode-hook
          (lambda ()
            (when-let ((r (locate-dominating-file default-directory ".pyroot")))
              (setq python-pytest-executable
                    (concat "PYTHONPATH=" r " " "pytest")))))
;; Set $DICPATH to "$HOME/Library/Spelling" for hunspell.
;; (setenv
;;   "DICPATH"
;;   (concat (getenv "HOME") "/Library/Spelling")
;;  )

;; enable shift-select in org mode
(setq org-support-shift-select t)

;; no line numbers in org-mode
(defun nolinum ()
  (display-line-numbers-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)

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
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
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


;; enable execution of 'dot' sourceblocks in org-mode so we can generate simple diagrams.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (dot . t)))


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

;; add to $DOOMDIR/config.el
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

;; ORG HEADINGS (https://zzamboni.org/post/beautifying-org-mode-in-emacs/)

;; hide marker symbols. e.g. **, //
(setq org-hide-emphasis-markers t)

;; different header sizes for different header levels
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; fix indentation when using variable-pitch-mode
(add-hook 'after-init-hook
          (lambda ()
            (require 'org-indent)       ; for org-indent face
            (set-face-attribute 'org-indent nil
                                :inherit '(org-hide fixed-pitch))))
;; customize fonts for various headers
(let* ((variable-tuple
        (cond ;; ((x-list-fonts "Robotic Mono")         '(:font "Robotic Mono"))
         ;; ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
         ;; ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
         ;; ((x-list-fonts "Verdana")         '(:font "Verdana"))
         ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
         (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline t))))))

;; customize fonts for various headers
;; (let* (
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ))))
;;      `(org-level-7 ((t (,@headline ))))
;;      `(org-level-6 ((t (,@headline ))))
;;      `(org-level-5 ((t (,@headline ))))
;;      `(org-level-4 ((t (,@headline :height 1.1))))
;;      `(org-level-3 ((t (,@headline :height 1.15))))
;;      `(org-level-2 ((t (,@headline :height 1.2))))
;;      `(org-level-1 ((t (,@headline :height 1.25))))
;;      `(org-document-title ((t (,@headline :height 1.5 :underline t))))))



;;(custom-theme-set-faces
;;   'user
;;   '(variable-pitch ((t (:family "Roboto Mono" :height 140 :weight thin))))
;;   '(fixed-pitch ((t ( :family "Roboto Mono" :height 120)))))

;; set monospace fonts for certain things
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; enable tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package! ejc-sql)

;; TODO: Add (ejc-create-connection ...)
(use-package! ejc-company)
(after! ejc-sql-mode
 (set-company-backend! 'ejc-company-backend))
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (company-mode t)))
(setq ejc-complete-on-dot t)

(global-set-key (kbd "C-c eb") 'ejc-get-temp-editor-buffer)

(use-package! dbt-mode
  :init
  (map! :after dbt-mode
        :localleader
        :map dbt-mode-map
        :desc "DBT Debug" "d" #'dbt-debug
        :desc "DBT Run All" "r a" #'dbt-run
        :desc "DBT Run Buffer" "r r" #'dbt-run-buffer
        :desc "DBT Build All" "b a" #'dbt-build
        :desc "DBT Build Buffer" "b b" #'dbt-build-buffer
        :desc "DBT Compile" "c c" #'dbt-compile
        :desc "DBT Compile & Open" "c o" #'dbt-open-compiled
        :desc "DBT Clean" "x" #'dbt-clean
        :desc "DBT Test All" "t" #'dbt-test))

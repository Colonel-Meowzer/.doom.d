;;; ~/.doom.d/config.el -- Base Configs

;; Place your private configuration here
(load-theme 'doom-zenburn t)

;; (set-frame-font "Fira Code" nil t)
(set-frame-font "FiraCode Nerd Font" nil t)
;; (set-frame-font "DejaVu Sans ExtraLight" nil t)

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
;;(require 'py-autopep8)
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; use jupyter as the interactive shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --no-color-info --pylab=osx"
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

(setq auth-sources '("~/.authinfo"))
;; enable execution of 'dot' sourceblocks in org-mode so we can generate simple diagrams.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)
   (dot . t)))


(setq org-roam-directory "~/org")
(global-prettify-symbols-mode t)
;; (set-fontset-font "fontset-default" '(#x1d4d5 . #x1d4e3) "Symbola")
(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; Base Types
           ("int" .      #x2124)
           ("float" .    #x211d)
           ("str" .      #x1d54A)
           ("True" .     #x1d4e3)
           ("False" .    #x1d4d5)
           ;; Mypy
           ("Tuple" .    #x2a02)
           ("Set" .      #x2126)
           ("Any" .      #x2754)
           ("Union" .    #x22c3)))))

;; (after! python-mode
;;   (set-ligatures! 'python-mode
;;     ;; Functional
;;     :lambda        "lambda"
;;     :def           "def"
;;     :map           "dict"
;;     ;; Types
;;     :null          "None"
;;     :true          "True"
;;     :false         "False"
;;     :int           "int"
;;     :float         "float"
;;     :str           "str"
;;     :bool          "bool"
;;     :list          "list"
;;     :map           "map"
;;     ;; Flow
;;     :not           "not"
;;     :in            "in"
;;     :not-in        "not in"
;;     :and           "and"
;;     :or            "or"
;;     :for           "for"
;;     :some          "some"
;;     :return        "return"
;;     :yield         "yield"
;;     ;; Other
;;     :tuple         "Tuple"))

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

;; TODO: Figure out how to incorporate the interactive version above

(defun sql-format-fix ()
  "Custom formatter for sql using sqlfluff"
  (interactive)
  (let (
        (choices '("ansi" "athena" "bigquery" "clickhouse" "databricks" "db2" "exasol" "hive" "mysql" "oracle" "postgres" "redshift" "snowflake" "soql" "sparksql" "sqlite" "teradata" "tsql"))
        )
    ;; (message (format "sqlfluff fix --dialect %s %s" (completing-read "sql-dialect:" choices) (buffer-file-name)))
    (async-shell-command
     (format "sqlfluff fix -f --dialect %s %s" (completing-read "sql-dialect: " choices) (buffer-file-name))
     )
    )
  )

(defun sql-format-lint ()
  "Custom formatter for sql using sqlfluff"
  (interactive)
  (let (
        (choices '("ansi" "athena" "bigquery" "clickhouse" "databricks" "db2" "exasol" "hive" "mysql" "oracle" "postgres" "redshift" "snowflake" "soql" "sparksql" "sqlite" "teradata" "tsql"))
        )
    ;; (message (format "sqlfluff fix --dialect %s %s" (completing-read "sql-dialect:" choices) (buffer-file-name)))
    (async-shell-command
     (format "sqlfluff lint --dialect %s %s" (completing-read "sql-dialect: " choices) (buffer-file-name))
     )
    )
  )

(define-key sql-mode-map (kbd "C-c ff") 'sql-format-fix)
(define-key sql-mode-map (kbd "C-c fl") 'sql-format-lint)
;; dbt key bindings
(define-key sql-mode-map (kbd "C-c dd") 'dbt-debug)
(define-key sql-mode-map (kbd "C-c dra") 'dbt-run)
(define-key sql-mode-map (kbd "C-c drr") 'dbt-run-buffer)
(define-key sql-mode-map (kbd "C-c dba") 'dbt-build)
(define-key sql-mode-map (kbd "C-c dbb") 'dbt-build-buffer)

(define-key sql-mode-map (kbd "C-c dcc") 'dbt-compile)
(define-key sql-mode-map (kbd "C-c dco") 'dbt-open-compiled)
(define-key sql-mode-map (kbd "C-c dx") 'dbt-clean)
(define-key sql-mode-map (kbd "C-c dt") 'dbt-test)

(setq +format-on-save-enabled-modes
      '(not python-mode         ; black has differing versions and is not uniform across repos.
        ))

;; org-download
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)


(defun magit-add-current-buffer-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(global-set-key (kbd "C-c g y") 'magit-add-current-buffer-to-kill-ring)

(defun civis-sql (sql)
  (interactive)
  (let ((buffer (generate-new-buffer "*query-result*")))
    (with-current-buffer buffer (csv-mode))
    (with-current-buffer buffer (toggle-truncate-lines))
    (call-process-shell-command (format "source %s && civis sql -d redshift-dr -c '%s' -n 5" (getenv "CREDENTIAL_FILE") sql) nil buffer 0)
    (switch-to-buffer buffer)))


(defun civis-sql-send-buffer (start end)
  (interactive)
  (let ((buffer (generate-new-buffer "*query-result*")))
    (with-current-buffer buffer (csv-mode))
    (with-current-buffer buffer (toggle-truncate-lines))
    (call-process-shell-command (format "source %s && civis sql -d redshift-dr -c '%s' -n 5" (getenv "CREDENTIAL_FILE") (buffer-substring-no-properties start end)) nil buffer 0)
    (switch-to-buffer buffer)))

(global-set-key (kbd "C-c c s") 'civis-sql-send-buffer)


;; https://github.com/org-roam/org-roam-ui/README.md
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    ;;:after org
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

(setq company-frontends '(company-tng-frontend company-box-frontend))

;; This opens up all files on startup by default. Make sure to archive the directory every now and then
(setq org-agenda-files '("~/org/daily/"))
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package dbt-mode
  ;; Customize `sql-product' to set the flavor of the SQL syntax.
  :custom (sql-product 'postgres))

(use-package! nvm
  :config
  ;; Optionally set a default node version
  (nvm-use "21"))


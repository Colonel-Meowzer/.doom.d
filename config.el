;;; ~/.doom.d/config.el -- Base Configs

(load-theme 'doom-nord-aurora t)

(setq doom-nord-aurora-brighter-comments t
      doom-nord-aurora-brighter-modeline t
      doom-nord-aurora-comment-bg t
      doom-nord-aurora-region-highlight "frost")

(if (featurep :system 'windows)
    (setq doom-font
          (font-spec :family "FiraCode NF" :size 12 :weight 'regular)))

(if (featurep :system 'macos)
    (setq doom-font
          (font-spec :family "Hack Nerd Font Mono" :size 12 :weight 'regular)))

;; make sure numpydoc is available for python-mode
(use-package numpydoc
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

;; Set up elpy minor mode to be enabled when for python-mode activated.
;; This needs to be done before elpy-enable
;;
;; Most of these configs were taken directly from Elpy documentation.

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

;; (require 'org)
;; (require 'ox-latex)
;; "minted" is a python library that allows for syntax highlighting when
;; exporting to pdf. This needs to be installed in the main python environment.
;;
;; The following configurations enable this functionality.
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)

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
   (dot . t)
   (sh . t)
   (python . t)))


;; Associate .jinja files with jinja2-mode
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))

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


(setq +format-on-save-enabled-modes
      '(not python-mode         ; black has differing versions and is not uniform across repos.
        not sql-mode            ; sql-formatting has a ways to go
        ))


(defun magit-add-current-buffer-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))


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

(setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  ;; :config (add-to-list `copilot-indentation-alist `((sql-mode 4) (emacs-lisp-mode 2)))
  )

(use-package! nvm
  :config
  ;; Optionally set a default node version
  (nvm-use "21"))


(use-package! lsp-mode)
(setq magit-git-executable "/usr/local/bin/git")

(setq dap-auto-configure-features '(locals tooltip repl))
(dap-auto-configure-mode)
(after! dap-mode
  ;; if you installed debugpy, you need to set this
  ;; https://github.com/emacs-lsp/dap-mode/issues/306
  (setq dap-python-debugger 'debugpy))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
(after! lsp-ui
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t))

(use-package direnv
  :config
  (direnv-mode))

;; Bind keys to scroll a buffer without moving the cursor
;; to the edge of the buffer.
(global-set-key [(s down)] (lambda () (interactive) (scroll-down 1)))
(global-set-key [(s up)] (lambda () (interactive) (scroll-up 1)))


;; Vertico configs
;; Enable vertico
(use-package! vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package! savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package! emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package! orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package! ob-async)

(use-package! org-pretty-table
  :config
  (add-hook 'org-mode-hook 'org-pretty-table-mode))

;; Add a custom keybinding for copying links
(map! :leader
      ;; :desc "Direnv Allow" "p E a" #'direnv-allow
      :desc "Link Management"
      (:prefix-map ("l" . "links")
       :desc "Copy Link" "y"   #'link-hint-copy-link
       :desc "Open Link" "l"   #'link-hint-open-link))

;; Add conda keybindings to python mode
(map!
 :localleader
 :after python
 :map python-mode-map
 (:prefix-map ("c" . "conda")
  :desc "Activate"   "a" #'conda-env-activate
  :desc "Deactivate" "d" #'conda-env-deactivate))

;; LSP keybindings
(map!
 :leader
 (:prefix-map ("c l" . "language server")
  :desc "start" "l" #'lsp
  :desc "dap-hydra" "d" #'dap-hydra)
 (:prefix-map ("p E" . "direnv")
  :desc "Allow" "a" #'direnv-allow))


(map! :leader
      :desc "Git"
      (:prefix-map ("g" . "git")
       :desc "Copy Branch Name" "," #'magit-add-current-buffer-to-kill-ring))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      :desc "Edit Lines"
      (:prefix-map ("e" . "edit")
       :desc "Edit Beginnings of Lines" "b" #'mc/edit-beginnings-of-lines
       :desc "Edit Ends of Lines" "e" #'mc/edit-ends-of-lines
       :desc "Edit Lines" "l" #'mc/edit-lines))

(defun org-babel-execute:civis_sql (body params)
  "Execute a block of Nim code with org-babel."
  (let ((in-file (org-babel-temp-file "n" ".nim"))
        (verbosity (or (cdr (assq :verbosity params)) 0)))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (format "civis sql -d %s" verbosity
             (org-babel-process-file-name in-file))
     "")))

(with-eval-after-load 'org (global-org-modern-mode))

;; enable shift-select in org mode
(setq org-support-shift-select t)

;; no line numbers in org-mode
(defun nolinum ()
  (display-line-numbers-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)

;; org-download
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;;; ~/.doom.d/config.el -- Base Configs

;; Place your private configuration here

;; Set up python environments
(def-package! elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; elpy configs
 (elpy-enable)

;; use flycheck not flymake with elpy
 (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; autocode completetion via autopep8
 (require 'py-autopep8)
 (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; use jupyter as the interactive shell
 (setq python-shell-interpreter "jupyter-console"
      python-shell-interpreter-args "--simple-prompt"
      python-shell-prompt-detect-failure-warning nil
      elpy-shell-echo-output nil)
 (add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(setenv "JAVA_HOME"
        (shell-command-to-string "echo $(/usr/libexec/java_home -v 1.8) | tr -d '\n'"))

;; fix buggy autocomplete
 (with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))


(setq org-support-shift-select 1)
;; org-confluence export
;; (require 'org-confluence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(display-time)

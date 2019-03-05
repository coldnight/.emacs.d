;;; init-minor-modes.el -- 次级模式
;;;
;;; Commentary:
;;;
;;; 配置次级模式, 包括:
;;;
;;; - projectile-mode
;;;
;;; Code:

;; 启用 projectile
(projectile-mode)

(setq projectile-globally-ignored-directories
      (append '(".git"
                ".svn"
                ".tox"
                ".venv"
                ".gradle"
                ".meghanada"
                "out"
                "repl"
                "target"
                "venv")
              projectile-globally-ignored-directories))

(setq projectile-globally-ignored-files
      (append '(".DS_Store"
                "*.gz"
                "*.pyc"
                "*.jar"
                "*.tar.gz"
                "*.tgz"
                "*.zip"
                )
              projectile-globally-ignored-files))

;; helm-projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/el-get/yasnippet-snippets/snippets/"
        "~/.emacs.d/snippets"))
(yas-global-mode 1)

;; .rst 文件禁用 flycheck
(setq-default flycheck-disabled-checkers '(rst))


;; 启用 pipenv
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(provide 'init-minor-modes)
;;; init-minor-modes.el ends here

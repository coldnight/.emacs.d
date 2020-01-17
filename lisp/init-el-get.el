;;; init-el-get.el -- 配置 el-get
;;;
;;; Commentary:
;;;
;;;    配置 el-get
;;; Code:
;;;

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(remove-hook 'el-get-post-install-hooks 'el-get-post-install-notification)
(require 'el-get-elpa)

;; (el-get 'sync)
(el-get-bundle ansi-color)
(el-get-bundle autopair)
(el-get-bundle company-mode)
;; (el-get-bundle elpa:jedi-core)
(el-get-bundle company-jedi :depends (company-mode))
;; (el-get-bundle eclim)
(el-get-bundle exec-path-from-shell)
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)
(el-get-bundle git-gutter)
(el-get-bundle gradle-mode)
(el-get-bundle highlight-indentation)
(el-get-bundle indent-guide)
(el-get-bundle js2-mode)
(el-get-bundle rjsx-mode)
(el-get-bundle json-mode)
(el-get-bundle json-reformat)
(el-get-bundle less-css-mode)
(el-get-bundle magit)
(el-get-bundle markdown-mode)
(el-get-bundle material-theme)
(el-get-bundle projectile)
;; Use Python.el
;; (el-get-bundle python-mode)
;; (el-get-bundle smart-mode-line)
(el-get-bundle smex)
(el-get-bundle yaml-mode)
;; (el-get-bundle mu4e)
;; (el-get-bundle mu4e-alert)
(el-get-bundle spacemacs-theme)
(el-get-bundle doom-themes)
(el-get-bundle pos-tip)
(el-get-bundle all-the-icons)
(el-get-bundle neotree)
(el-get-bundle request)
(el-get-bundle v2ex-mode)
(el-get-bundle helm)
(el-get-bundle helm-projectile)
(el-get-bundle helm-ag)
(el-get-bundle helm-dash)
(el-get-bundle helm-swoop)
(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)
(el-get-bundle with-editor)
(el-get-bundle web-mode)
(el-get-bundle rust-mode)
(el-get-bundle htmlize)  ;; For org-mode to export HTML
(el-get-bundle meghanada)
(el-get-bundle dracula-theme)
(el-get-bundle multi-term)
(el-get-bundle eterm-256color)
(el-get-bundle github:coldnight/emacs-urlview
  :features urlview)
(el-get-bundle php-mode)
(el-get-bundle go-mode)
(el-get-bundle kotlin-mode)
;; (el-get-bundle org-journal)
(el-get-bundle blacken)
(el-get-bundle lsp-mode)
(el-get-bundle pyvenv)
(el-get-bundle use-package)
(el-get-bundle pipenv)
(el-get-bundle elfeed)
;; (el-get-bundle github:vreeze/eboy
;;   :features eboy)
(el-get-bundle github:emacs-lsp/dap-mode
  :features dap-mode
  :depends (tree-mode lsp-java bui markdown-mode))
(el-get-bundle typescript-mode)
(el-get-bundle tide)
(el-get-bundle github:emacs-lsp/helm-lsp
  :features helm-lsp
  :depends(lsp-mode))

(cond ((string-equal system-type "darwin")
  (progn
    (el-get-bundle emojify))))

(el-get-bundle github:caiorss/org-wiki)
(el-get-bundle github:emacs-dashboard/emacs-dashboard
  :features dashboard
  :depends (page-break-lines))
(el-get-bundle org-pomodoro)
(el-get-bundle dockerfile-mode)
(el-get-bundle github:seagle0128/doom-modeline
  :features doom-modeline
  :depends(all-the-icons shrink-path))
(el-get-bundle cmake-mode)

(el-get-bundle chinese-fonts-setup)

(provide 'init-el-get)
;; init-el-get.el ends here

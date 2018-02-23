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
;; (el-get 'sync)
(el-get-bundle ansi-color)
(el-get-bundle autopair)
(el-get-bundle company-mode)
(el-get-bundle company-jedi)
(el-get-bundle eclim)
(el-get-bundle exec-path-from-shell)
(el-get-bundle flycheck)
(el-get-bundle git-gutter)
(el-get-bundle gradle-mode)
(el-get-bundle highlight-indentation)
(el-get-bundle indent-guide)
(el-get-bundle js2-mode)
(el-get-bundle json-mode)
(el-get-bundle json-reformat)
(el-get-bundle less-css-mode)
(el-get-bundle magit)
(el-get-bundle markdown-mode)
(el-get-bundle material-theme)
(el-get-bundle projectile)
(el-get-bundle python-mode)
(el-get-bundle smart-mode-line)
(el-get-bundle smex)
(el-get-bundle yaml-mode)
(el-get-bundle mu4e)
(el-get-bundle mu4e-alert)
(el-get-bundle spacemacs-theme)
(el-get-bundle doom-themes)
(el-get-bundle pos-tip)
(el-get-bundle all-the-icons)
(el-get-bundle neotree)
(el-get-bundle request)
(el-get-bundle v2ex-mode)
(el-get-bundle helm)

(provide 'init-el-get)
;; init-el-get.el ends here

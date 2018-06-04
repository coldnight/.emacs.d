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

;; helm-projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/el-get/yasnippet-snippets/snippets/"
        "~/.emacs.d/snippets"))
(yas-global-mode 1)

(provide 'init-minor-modes)

;;; init-minor-modes.el ends here

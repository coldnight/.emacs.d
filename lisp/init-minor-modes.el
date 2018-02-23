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

(provide 'init-minor-modes)

;;; init-minor-modes.el ends here

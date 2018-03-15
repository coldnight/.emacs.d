;;; init-key-bind.el -- 按键绑定
;;;
;;; Commentary:
;;;
;;; 配置按键绑定
;;;
;;;
;;; Code:
;;;

;; windmove
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)

;; smex
;; (require 'smex)
;; (global-set-key (kbd "M-x") 'smex)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c s n") 'neotree-toggle)

(require 'mu4e)
(global-set-key (kbd "C-c s m") 'mu4e)

;; helm-ag
(global-set-key (kbd "C-r") 'helm-do-ag-this-file)
(global-set-key (kbd "C-s") 'helm-do-ag-this-file)
(global-set-key (kbd "C-c a a") 'helm-do-ag)
(global-set-key (kbd "C-c a p") 'helm-do-ag-project-root)
(global-set-key (kbd "C-c a f") 'helm-do-ag-this-file)
(provide 'init-key-bind)
;; init-key-bind.el ends here

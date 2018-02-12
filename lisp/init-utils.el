;;; init-utils.el -- 初始化一些小工具
;;;
;;; Commentary:
;;;
;;;    - autopair
;;;
;;; Code:
;;;

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


;; Smex
(require 'smex)
(smex-initialize)

(provide 'init-utils)
;; init-utils.el ends here

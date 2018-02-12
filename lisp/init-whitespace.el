;;; init-whitespace.el -- 配置 whitespace
;;; Commentary:
;;;
;;;    初始化 whitespace 相关
;;;
;;; Code:

(require 'whitespace)
;; Whitespace
(setq whitespace-line-column 79)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

; 自动删除行尾空白
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(provide 'init-whitespace)
;; init-whitespace.el ends here

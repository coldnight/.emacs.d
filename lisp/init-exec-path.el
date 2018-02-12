;;; init-exec-path.el -- 从 Shell 读取 $PATH 环境变量
;;;
;;; Commentary:
;;;
;;; 配置 exec-path-from-shell
;;;
;;; Code:
;;;
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

(provide 'init-exec-path)
;; init-exec-path.el ends here

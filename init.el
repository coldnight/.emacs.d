;;; init.el -- Emacs 配置文件
;;;
;;; Commentary:
;;;
;;; Emacs 配置文件
;;;
;;; Code:
;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst secret-el-path (expand-file-name ".secret.el" user-emacs-directory))

(if(file-exists-p secret-el-path)
 (load-file secret-el-path))

(require 'init-common)
(require 'init-el-get)
(require 'init-utils)

;; Whitespace
(require 'init-whitespace)

;; 从 shell 的 $PATH 中获取命令
(require 'init-exec-path)

;; 初始化主模式
(require 'init-major-modes)

;; 次模式
(require 'init-minor-modes)

;; 外观
(require 'init-appearance)

;; mu4e 配置
(require 'init-mu4e)

;;配置 erc
(require 'init-erc)

;; 按键绑定
(require 'init-key-bind)

(require 'init-helm)
(require 'init-org-mode)

;; init.el ends here

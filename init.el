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
;; (package-initialize)

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
(require 'init-shell)

;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "463a24ebf2922855c36637f46c53903a4737cf57038e7a9fba700456e3bd27f2" "3d20cf0dbc6465a02c468abf2d9b8c17e88b20fbc05a04205a829285da28799d" default)))
 '(meghanada-cache-in-project nil)
 '(meghanada-class-completion-matcher "fuzzy")
 '(meghanada-completion-matcher "fuzzy")
 '(uniquify-buffer-name-style nil nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

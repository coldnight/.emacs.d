;;; init-builtin-mode.el -- Emacs 内置模式
;;;
;;; Commentary:
;;;
;;;    内置模式
;;;
;;; Code:
;;;

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(menu-bar-mode -1)            ;; 取消菜单栏
(scroll-bar-mode -1)          ;; 取消滚动条（在 Emacs 26 中无效）
(tool-bar-mode -1)            ;; 取消工具栏
(fset 'yes-or-no-p 'y-or-n-p) ;; 按y或space表示yes,n表示no
(global-font-lock-mode t)     ;; 语法高亮
(show-paren-mode t)           ;; 打开括号匹配显示模式
(mouse-avoidance-mode 'animate) ;; 鼠标靠近光标指针时,让鼠标自动让开
(auto-compression-mode 1) ;; 打开压缩文件时自动解压缩
(global-auto-revert-mode 1)       ;; 自动重载更改的文件
(blink-cursor-mode -1)            ;; 指针不要闪
(toggle-truncate-lines t)         ;; 当一行文字太长时,不自动换行
;; (column-number-mode t)            ;; 在minibuffer上面的状态栏显示文件的行号,列号
;; (line-number-mode t)              ;; Modeline 中显示行号

;; 显示行号
(if (>= emacs-major-version 26)
    ;; Emacs 26 中更快的行号显示
    (progn
      ;; No fringe when display-line-numbers-mode enabled
      (add-hook 'display-line-numbers-mode-hook (lambda () (fringe-mode '(0 . 0))))
      (global-display-line-numbers-mode t)
      ;; hide line-numbers in dashboard buffer because we are defered
      (display-line-numbers-mode -1))
  ;; No fringe when display-line-numbers-mode enabled
  (add-hook 'linum-mode-hook (lambda () (fringe-mode '(0 . 0))))
  (global-linum-mode t))

;; 禁用时间显示
(display-time-mode -1)

(use-package subword :straight (:type built-in) :defer 3 :config (global-subword-mode 1))

;; 记住上次打开文件光标的位置
(use-package saveplace
  :straight (:type built-in)
  :defer 3
  :config
  (save-place-mode 1))

(use-package whitespace
  :straight (:type built-in)
  :hook
  (prog-mode . whitespace-mode)
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-line-column 79)
  (whitespace-style '(face lines-tail))
  (delete-trailing-lines t))

(provide 'init-builtin-mode)
;;; init-builtin-mode.el ends here

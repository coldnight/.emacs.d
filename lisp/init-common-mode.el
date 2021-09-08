;;; init-common-appearance.el -- Emacs 基本模式
;;;
;;; Commentary:
;;;
;;;    基本模式
;;;
;;; Code:
;;;

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
(column-number-mode t)            ;; 在minibuffer上面的状态栏显示文件的行号,列号
(line-number-mode t)              ;; Modeline 中显示行号

;; 显示行号
(if (>= emacs-major-version 26)
    ;; Emacs 26 中更快的行号显示
    (progn
      (global-display-line-numbers-mode t)
      ;; hide line-numbers in dashboard buffer because we are defered
      (display-line-numbers-mode -1))
  (global-linum-mode t))

;; 时间显示设置
;; <s>启用时间显示设置,在minibuffer上面的那个杠上</s>
;; 禁用时间显示
(display-time-mode -1)
;; (setq
;;  display-time-24hr-format t  ;; 时间使用24小时制
;;  display-time-day-and-date t ;; 时间显示包括日期和具体时间
;;  display-time-use-mail-icon t ;; 时间栏旁边启用邮件设置
;;  display-time-interval 10 ;; 时间的变化频率
;;  setq display-time-format "%A %H:%M" ;; 显示时间的格式
;;  )
(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have
(global-subword-mode 1)

(require 'saveplace)
(save-place-mode 1)               ;; 记住上次打开文件光标的位置

(provide 'init-common-mode)
;;; init-common-appearance.el ends here

;;; init-commont.el -- Emacs 基本配置
;;;
;;; Commentary:
;;;
;;;    基本配置
;;; Code:
;;;

;; 中文显示
(set-language-environment "utf-8")
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq
 default-process-coding-system '(utf-8 . utf-8)
 locale-coding-system 'utf-8
 file-name-coding-system 'utf-8
 default-buffer-file-coding-system 'utf-8
 slime-net-coding-system 'utf-8-unix)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")                           ; Iterate through CamelCase words

;; 基本设置
(setq-default
 indicate-buffer-boundaries 'left ;; 在窗口边缘上显示一个小箭头指示当前 buffer 的边界
 kill-whole-line t)  ;; C-k时,同时删除该行

(setq
 fringes-outside-margins t   ;; fringe 放在外面
 echo-keystrokes 0.1         ;; 尽快显示按键序列
 system-time-locale "zh_CN"  ;; 设置系统时间显示格式
 tab-always-indent 'complete ;; Tab 键优先格式化再补全
 font-lock-global-modes '(not shell-mode text-mode) ;; 设置语法高亮.除shell-mode和text-mode之外的模式
 mouse-yank-at-point t       ;; 不在鼠标点击的地方插入剪贴板内容
 kill-ring-max 200           ;; 设置 kill ring 个数
 default-fill-column 60      ;; 把fill-column设为60.让文字更好读
 enable-recursive-minibuffers t  ;; 递归的使用minibuffer
 scroll-margin 3             ;; 在靠近屏幕边沿 3 行时就开始滚动,可很好看到上下文
 scroll-conservatively 10000 ;; 防止页面滚动时跳动
 select-enable-clipboard t   ;; 允许emacs和外部程序进行粘贴
 track-eol t                 ;; 当光标在行尾上下移动的时候,始终保持在行尾
 next-line-add-newlines nil  ;; 按C-n或down时不添加新行
 ;; emacs启动时显示的内容可以通过变量initial-scratch-message来设置
 initial-scratch-message nil
 dired-listing-switches "-vha" ;;  dired 列出文件的参数（man ls）
 show-paren-style 'parenthesis ;; 括号匹配时高亮显示另一边的括号，而不是跳到另一个括号处
 )
;; 在标题栏显示buffer的名字和项目
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

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
(line-number-mode t)              ;;设定显示文件的参数,以版本/人性化的显示,就是ls的参数
(global-linum-mode t)             ;; 显示行号
(require 'saveplace)
(save-place-mode 1)               ;; 记住上次打开文件光标的位置


;; 设置4个空格缩进
(setq-default indent-tabs-mode nil)
(setq tab-width 4) ; or any other preferred value

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

;;
(setq-local default-directory "~/.emacs.d/data/autosave")
(setq-default auto-save-default t)

;; key bindings
;; windmove
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)

;; Making Buffer Names Unique
;; 当寻找一个同名的文件,改变两个buffer的名字,前面加上目录名
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; make a temp buffer
(defun my/generate-buffer ()
  "Generate a tmp buffer."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(global-set-key (kbd "C-c g b") 'my/generate-buffer)


(defun my/open-remark-org()
  "Open my own remark in 'org-mode'."
  (interactive)
  (find-file "~/Documents/org-modes/remark.org"))

(global-set-key (kbd "C-c s r") 'my/open-remark-org)

;; C/C++
(require 'cc-vars)
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4
      indent-tabs-mode t)

(defvar usr-bin-path "/usr/bin/"
  "The /usr/bin path to custom in each OS.")
(message "%s" "initialized common configuration!")


;; Copied from https://tecosaur.github.io/emacs-config/
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have
(global-subword-mode 1)

(provide 'init-common)
;;; init-common.el ends here

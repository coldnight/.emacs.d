;;; init.el -- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Use use-package and straight to manage Emacs configuration
;;;
;;; Code:
;;;

;; straight.el to manage package
(eval-when-compile
  ;; custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (if (file-exists-p custom-file)
      (load custom-file))

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (defconst secret-el-path (expand-file-name ".secret.el" user-emacs-directory))

  (if(file-exists-p secret-el-path)
      (load-file secret-el-path)
    (defconst secret-wakatime-api-key "")))

;; use package
(straight-use-package 'use-package)

;; ;; 开启 use-package 记录包加载时间
;; ;; see also: https://emacstalk.github.io/post/004/
(setq use-package-verbose t)


;;; Common
(use-package init-local-early)
(use-package init-common)
(use-package init-my
  :bind
  ("C-c s p" . my/url-proxy-toggle)
  ("C-c s g" . my/generate-buffer))

(use-package init-darwin
  :after init-builtin-mode
  :if (memq window-system '(mac ns)))

(use-package init-linux
  :after init-builtin-mode
  :if (memq window-system '(x)))

;;; Appearance
(use-package init-builtin-mode :defer 1 :after init-common)
;; (use-package init-theme-doom)
;; (use-package init-theme-nano)
;; (use-package init-theme-bespoke)
(use-package init-theme-modus)
(use-package init-screen)

(use-package emojify
  :straight (emojify :host github :repo "iqbalansari/emacs-emojify"
                     :files ("*.el" "data"))
  :commands emojify-mode
  :custom
  (emojify-emoji-styles '(unicode github)))

;; (use-package cnfonts
;;   :straight (cnfonts :host github :repo "tumashu/cnfonts")
;;   :after init-darwin
;;   :custom
;;   (cnfonts--custom-set-fontnames
;;    '(
;;      ("MonoLisa Nasy" "Victor Mono")
;;      ("WenQuanYi Zen Hei Mono" "STKaiti" "Hiragino Sans GB")
;;      ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB")))
;;
;;   (cnfonts--custom-set-fontsizes
;;    '(
;;      (14  16.5 16.5)
;;      ))
;;   :init
;;   (cnfonts-enable))


(when window-system
  ;; Auto generated by cnfonts
  ;; <https://github.com/tumashu/cnfonts>
  (set-face-attribute
   'default nil
   :font (font-spec :name "-*-MonoLisa Nasy-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 14.0))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-*-WenQuanYi Zen Hei Mono-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 16.5)))
  (use-package ultra-scroll-mac
    :if (eq window-system 'mac)
    :straight (ultra-scroll-mac :host github :repo "jdtsmith/ultra-scroll-mac")
    :init
    (setq scroll-conservatively 101) ; important for jumbo images
    :config
    (ultra-scroll-mac-mode 1)))

(use-package all-the-icons :straight t)



;;; Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package vterm
  :straight t
  ;; commands to defer
  :commands (vterm vterm-buffer)
  :after multi-vterm
  :hook
  (vterm-mode . (lambda ()
                  (when (< emacs-major-version 29)
                    (linum-mode -1))
                  (when (>= emacs-major-version 26)
                    (display-line-numbers-mode -1))))
  :custom
  (vterm-max-scrollback 100000)
  :config
  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)))

(use-package vterm-toggle
	:straight t
	:after vterm
	:custom
	(vterm-toggle-fullscreen-p nil)
	(vterm-toggle-cd-auto-create-buffer nil)
	:bind
	("C-c v t" . vterm-toggle-cd)
	:config
	(add-to-list 'display-buffer-alist
							 '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package multi-vterm
  :straight t
  :bind
  ("C-c v v" . multi-vterm)
  ("C-c v P" . multi-vterm-project)
  ("C-c v n" . multi-vterm-next)
  ("C-c v p" . multi-vterm-prev)
  ("C-c v d" . multi-vterm-dedicated-toggle))

;;; Enhance Emacs
(use-package goto-line-preview :straight t
  :bind (("M-g g" . goto-line-preview)))

(use-package which-key
  :straight t
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (which-key-show-early-on-C-h t)
  :init
  (which-key-mode))

(use-package posframe :defer 1 :straight t)

;; (use-package init-helm)
(use-package init-ivy)

(use-package urlview
  :straight (:host github :repo "coldnight/emacs-urlview" :branch "master")
  :bind
  ("C-c u v" . uv/browse-url))

;; Effective
(use-package magit
  :straight (magit :host github :repo "magit/magit")
  :bind
  ("C-c m s" . magit-status)
  ("C-c m p" . magit-push-current)
  ("C-c m c" . magit-branch-checkout)
  ("C-c m b" . magit-branch-and-checkout)
  ("C-c m f" . magit-fetch)
  ("C-c m m" . magit-merge)
  ("C-c m r" . magit-rebase))

(use-package projectile
  :straight t
  :custom
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  ;; see https://github.com/syl20bnr/spacemacs/issues/11381#issuecomment-481239700
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (setq projectile-globally-ignored-directories
        (append '(".git"
                  ".svn"
                  ".tox"
                  ".venv"
                  ".gradle"
                  ".meghanada"
                  ".clangd"
                  ".cargo"
                  ".rustup"
                  "/usr/"
                  "eln-cache"
                  "straight"
                  "out"
                  "repl"
                  "target"
                  "venv")
                projectile-globally-ignored-directories))

  (setq projectile-globally-ignored-files
        (append '(".DS_Store"
                  "*.gz"
                  "*.pyc"
                  "*.jar"
                  "*.tar.gz"
                  "*.tgz"
                  "*.zip"
                  "*.eln"
                  "*.elc"
                  )
                projectile-globally-ignored-files))
  (projectile-register-project-type 'hugo '("config.toml" "archetypes" "content")
                                    :project-file "config.toml"
				                            :compile "hugo"
				                            :test "open http://localhost:1313/"
				                            :run "hugo server -D --disableFastRender --navigateToChanged"))
;; Programming Tools
(use-package init-programming)

;;; Language Server Mode
(use-package init-lsp :after (init-builtin-mode flycheck))

;; Org Mode
(use-package init-org :after init-builtin-mode)

;;; Misc
;; speedup
;; M-x esup
(use-package esup :straight t
  :commands esup)

;; backup
(use-package backup-walker
  :straight t
  :defer 3
  :hook
  (before-save . (lambda () (setq buffer-backed-up nil)))
  :custom
  ;; 备份设置
  (make-backup-files t)
  (vc-make-backup-files t)
  ;; 启用版本控制,
  (version-control t)
  ;; 备份最原始的版本两次,记第一次编辑前的文档,和第二次编辑前的文档
  (kept-old-versions 2)
  ;; 备份最新的版本 256 次,理解同上
  (kept-new-versions 256)
  ;; 删掉不属于以上1,2中版本的版本
  (delete-old-versions t)
  ;; 备份设置方法,直接拷贝
  (backup-by-copying t)
  :config
  ;; 设置备份文件的路径
  (defvar-local backup-dir (concat user-emacs-directory "backups") "Backup directory.")
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))
  (add-to-list 'backup-directory-alist `(".*" . ,backup-dir))

  ;; tramp for remote edit
  (setq tramp-backup-directory-alist backup-directory-alist))

(use-package leetcode
  :straight (leetcode :host github :repo "kaiwk/leetcode.el")
  :commands leetcode
  :custom
  (leetcode-prefer-language "cpp"))

(if (not (string= secret-wakatime-api-key ""))
    (use-package wakatime-mode
      :straight t
      :defer 4
      :custom
      (wakatime-api-key secret-wakatime-api-key)
      :init
      (global-wakatime-mode)))

(use-package init-optional)
(use-package init-mu4e :after org)
(use-package init-writing)
(use-package init-local)

(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)

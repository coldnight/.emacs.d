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

;;; Common
(use-package init-common)

(use-package init-darwin
  :if (memq window-system '(mac ns)))

(use-package init-linux
  :if (memq window-system '(x)))

;;; Appearance
(use-package doom-themes
  :ensure t
  :straight (doom-themes :host github :repo "hlissner/emacs-doom-themes"
                         :files ("*.el" "themes"))
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-nord t)
  (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nyan-mode
  :straight t
  :init
  (nyan-mode 1))

(use-package doom-modeline
  :straight t
  :after nyan-mode
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-all)
  :init
  (doom-modeline-mode 1))

(use-package emojify
  :straight (emojify :host github :repo "iqbalansari/emacs-emojify"
                     :files ("*.el" "data"))
  :hook
  (after-init . global-emojify-mode)
  :init
  (setq emojify-emoji-styles '(unicode github)))

(use-package dashboard
  :straight (dashboard :host github :repo "emacs-dashboard/emacs-dashboard"
                       :files ("*.el" "banners"))
  :custom
  (dashboard-center-content 1)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner "~/.emacs.d/logo.png")
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "http://github.com/coldnight")))
           (,(all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0)
            "Mail"
            "Open mu4e"
            (lambda (&rest _) (counsel-M-x "^mu4e$")))
           (,(all-the-icons-octicon "issue-opened" :height 1.1 :v-adjust 0.0)
            "Help"
            "?/h"
            (lambda (&rest _) (counsel-M-x "^help$")))
           (,(all-the-icons-octicon "x" :height 1.1 :v-adjust 0.0)
            "Quit"
            "Quit Emacs"
            (lambda (&rest _) (counsel-M-x "^kill-emacs$"))))
          ((,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
            "Update"
            "Upate straight.el Packages."
            (lambda (&rest _) (counsel-M-x "^straight-pull-all$")) nil "<" ">")
           (,(all-the-icons-octicon "pin" :height 1.1 :v-adjust 0.0)
            "Freeze"
            "Freeze straight.el packages."
            (lambda (&rest _) (counsel-M-x "^straight-freeze-versions$")) nil "<" ">")
           (,(all-the-icons-octicon "clock" :height 1.1 :v-adjust 0.0)
            "Thaw"
            "Thaw straight.el packages to latest version."
            (lambda (&rest _) (counsel-M-x "^straight-thaw-versions$")) nil "<" ">"))))
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package cnfonts
  :straight (cnfonts :host github :repo "tumashu/cnfonts")
  :after init-darwin
  :init
  (setq cnfonts--custom-set-fontnames
      '(
        ("Victor Mono")
        ("Hiragino Sans GB")
        ("HanaMinB")))

  (setq cnfonts--custom-set-fontsizes
        '(
          (14  16.5 16.5)
          ))

  (cnfonts-enable))

(use-package all-the-icons :straight t)

(use-package centaur-tabs
  :straight t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-style "wave")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-cycle-scope 'groups)
  :bind
  ("C-c t p" . centaur-tabs-backward)
  ("C-c t n" . centaur-tabs-forward)
  ("C-c t t" . centaur-tabs-counsel-switch-group))

(use-package ligature
  :straight
  (ligature :host github :repo "mickeynp/ligature.el")
  :after (s org-roam projectile)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package vterm
  :straight t
  :config
  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)))

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

;; (use-package init-helm)
(use-package init-ivy)

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
  :init
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
                  "eln-cache"
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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'hugo '("config.toml" "archetypes" "content")
                                  :project-file "config.toml"
				  :compile "hugo"
				  :test "open http://localhost:1313/"
				  :run "hugo server -D --disableFastRender --navigateToChanged"))

(use-package company
  :straight t
  :defer t
  :init
  (global-company-mode))

(use-package company-jedi
  :straight t
  :defer t
  :after company)

;; Programming Tools
(use-package autopair
  :straight (autopair :host github :repo "joaotavora/autopair")
  :config
  (autopair-global-mode))

(use-package flycheck
  :straight t
  :hook
  (after-init . global-flycheck-mode)
  :init
  ;; .rst 文件禁用 flycheck
  (setq-default flycheck-disabled-checkers '(rst)))

(use-package pos-tip
  :straight t)

(use-package flycheck-pos-tip
;;  :straight (flycheck :host github :repo "flycheck/flycheck-pos-tip")
  :straight t
  :after (flycheck pos-tip)
  :init
  (flycheck-pos-tip-mode))

(use-package posframe
  :straight t)

(use-package flycheck-swiftlint
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))

;; lsp-mode must come above of all lsp-packages
(use-package lsp-mode
  :straight (lsp-mode :host github :repo "emacs-lsp/lsp-mode")
  :init
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    "Set up before-save hooks to format buffer and add/delete imports."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  :hook
  (python-mode . lsp)
  (go-mode . lsp)
  (c++-mode . lsp)
  (rust-mode . lsp)
  (php-mode . lsp)
  (scala-mode . lsp)
  (cmake-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :custom
  (lsp-rust-server 'rust-analyzer)
  (lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  :after (company flycheck))

(use-package lsp-java
  :straight t
  :after lsp-mode
  :hook
  (java-mode . lsp))

(use-package helm-lsp :straight :commands helm-lsp-workspace-symbol)

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :straight (lsp-metals :host github :repo "emacs-lsp/lsp-metals")
  :custom (lsp-metals-treeview-show-when-views-received t))

(use-package lsp-sourcekit
  :straight t
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package dap-mode
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-go)
  (dap-go-setup)
  :custom
  ;; (dap-go-debug-program `("node" ,(expand-file-name "~/.vscode/extensions/ms-vscode.go-0.9.2/out/src/debugAdapter/goDebug.js")))
  (dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-print-io t)
  :after (lsp-mode))

(use-package lsp-ui
  :straight t
  :after lsp-mode)

(use-package whitespace
  :straight whitespace
  :hook
  (prog-mode . whitespace-mode)
  (before-save . delete-trailing-whitespace)
  :custom
  (whitespace-line-column 79)
  (whitespace-style '(face lines-tail))
  (delete-trailing-lines t))

;; Python black
(use-package blacken
  :straight (blacken :host github :repo "pythonic-emacs/blacken")
  :custom
  (blacken-fast-unsafe t)
  :bind
  ("C-c f p" . blacken-buffer))

;; Programming Language Mode
(use-package go-mode
  :straight (go-mode :host github :repo "dominikh/go-mode.el")
  :mode "\\.go\\'"
  :custom
  (gofmt-command (concat usr-bin-path "gofmt"))
  :bind
  ("C-c f g" . gofmt))

(use-package markdown-mode
  :straight t
  :mode
  ("\\.md\\'" . gfm-mode))

;; 设置缩进级别空格数
(defvar-local my/web-mode-offset 2)


(defun my/current-buffer-suffix()
  "Return suffix of current buffer."

  (nth 0 (cdr (split-string (buffer-name) "\\."))))
(use-package web-mode
  :straight t
  :hook
  (web-mode . (lambda()
                (if (string= (my/current-buffer-suffix) "vue")
                    (setq web-mode-style-padding 0
                          web-mode-script-padding 0))

                ;; 设置缩进级别
                (setq web-mode-markup-indent-offset my/web-mode-offset)
                (setq web-mode-css-indent-offset my/web-mode-offset)
                (setq web-mode-code-indent-offset my/web-mode-offset)
                (setq web-mode-attr-indent-offset my/web-mode-offset)))
  :mode
  ("\\.js\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.vue\\'" . web-mode)
  ("\\.jinja\\'" . web-mode)
  ("\\.ts\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :custom
  ;; JS2 设置缩进
  (js2-basic-offset my/web-mode-offset)
  (js-indent-level my/web-mode-offset)
  (company-tooltip-align-annotations t)

  ;; HTML/XML 缩进
  (sgml-basic-offset my/web-mode-offset))

;; TypeScript
(defun my/setup-tide-mode ()
  "Setup tide mode used in \\<keymap\\>>."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :straight t
  :hook
  (before-save . tide-format-before-save)
  (typescript-mode . setup-tide-mode)
  (web-mode .
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my/setup-tide-mode))))
  :init
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :after (web-mode flycheck company))

;; Org Mode
(use-package init-org)

;; Some useful modes
(use-package swift-mode :straight t)
(use-package indent-guide :straight t)
(use-package highlight-indentation :straight t)
(use-package json-mode :straight t)
(use-package yaml-mode :straight t)
(use-package less-css-mode :straight t)
(use-package gradle-mode :straight t)
(use-package rust-mode :straight t)
(use-package htmlize :straight t)
(use-package php-mode :straight t)
(use-package kotlin-mode :straight t)
(use-package dockerfile-mode :straight t)
(use-package cmake-mode
  :straight (:host github :flavor melpa
                   :files ("Auxiliary/*.el" "cmake-mode-pkg.el")
                   :repo "Kitware/CMake"))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :straight t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package urlview
  :straight (:host github :repo "coldnight/emacs-urlview" :branch "master"))

;; speedup
;; M-x esup
(use-package esup :straight t)

;; backup
(use-package backup-walker
  :straight t
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
  :init
  ;; 设置备份文件的路径
  (defvar-local backup-dir (concat user-emacs-directory "backups") "Backup directory.")

  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))

  (add-to-list 'backup-directory-alist `(".*" . ,backup-dir))

  (defun force-backup-of-buffer()
    "Force backup buffer."
    (setq buffer-backed-up nil))

  (add-hook 'before-save-hook 'force-backup-of-buffer)

  ;; tramp for remote edit
  (setq tramp-backup-directory-alist backup-directory-alist))

(use-package leetcode
  :straight (leetcode :host github :repo "kaiwk/leetcode.el")
  :custom
  (leetcode-prefer-language "cpp"))

(if (not (string= secret-wakatime-api-key ""))
    (use-package wakatime-mode
      :straight t
      :custom
      (wakatime-api-key secret-wakatime-api-key)
      :init
      (global-wakatime-mode)))

(use-package init-optional)
(use-package init-mu4e)

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)
;;; init.el ends here

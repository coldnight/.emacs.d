;;; init.el -- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Use use-package and straight to manage Emacs configuration
;;;
;;; Code:
;;;
;; use package
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (require 'use-package)

  (defconst secret-el-path (expand-file-name ".secret.el" user-emacs-directory))

  (if(file-exists-p secret-el-path)
      (load-file secret-el-path)))

(setq straight-vc-git-default-protocol 'ssh)

;; straight.el to manage package
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

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight (doom-modeline :host github :repo "seagle0128/doom-modeline")
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

(use-package goto-line-preview :straight t
  :bind (("M-g g" . goto-line-preview)))


(use-package centaur-tabs
  :straight t
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-style "wave")
  :bind
  ("C-c t p" . centaur-tabs-backward)
  ("C-c t n" . centaur-tabs-forward))

;;; Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package quickrun
  :straight t)

;;; Enhance Emacs
(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :straight (helm :host github :repo "emacs-helm/helm")
  :commands heml-config
  :hook
  (helm-minibuffer-set-up . spacemacs//helm-hide-minibuffer-maybe)
  :init
  (require 'helm)
  (require 'helm-config)
  (setq helm-google-suggest-use-curl-p t)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  :bind
  ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x C-f" . helm-find-files)
  (:map helm-map
        (("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-selection-action))))

(use-package helm-ag
  :straight (helm-ag :host github :repo "syohex/emacs-helm-ag")
  :bind
  ("C-c a s" . helm-do-ag-this-file)
  ("C-c a a" . helm-do-ag)
  ("C-c a p" . helm-do-ag-project-root)
  ("C-c a f" . helm-do-ag-this-file)
  :after (helm))

(use-package helm-dash
  :straight (helm-dash :host github :repo "dash-docs-el/helm-dash")
  :bind
  ("C-c d s" . helm-dash))


(use-package neotree
  :straight (neotree :host github :repo "jaypei/emacs-neotree")
  :bind
  ("C-c s n" . neotree-toggle))

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
  :init
  (projectile-mode +1)

  (setq projectile-globally-ignored-directories
        (append '(".git"
                  ".svn"
                  ".tox"
                  ".venv"
                  ".gradle"
                  ".meghanada"
                  ".clangd"
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
                  )
                projectile-globally-ignored-files))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'hugo '("config.toml" "archetypes" "content")
                                  :project-file "config.toml"
				  :compile "hugo"
				  :test "open http://localhost:1313/"
				  :run "hugo server -D --disableFastRender --navigateToChanged"))

(use-package helm-projectile
  :straight t
  :after (projectile helm)
  :custom
  (projectile-completion-system 'helm)
  :init
  (helm-projectile-on))

(use-package company
  :straight t
  :defer t
  :init
  (global-company-mode))

(use-package company-jedi
  :straight t
  :defer t
  :after company)

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :init
  (setq yas-snippet-dirs
      '("~/.emacs.d/straight/build/yasnippet-snippets/snippets/"
        "~/.emacs.d/snippets"))
  (yas-global-mode 1))

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

(use-package swift-mode
  :straight t)

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

;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Set up before-save hooks to format buffer and add/delete imports."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :straight (lsp-metals :host github :repo "emacs-lsp/lsp-metals")
  :config (setq lsp-metals-treeview-show-when-views-received t))

(use-package lsp-sourcekit
  :straight t
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

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

;; (use-package company-lsp
;;   :straight t
;;   :init
;;   (push 'company-lsp company-backends)
;;   :after (company lsp-mode))

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
  (whitespace-style '(face lines-tail)))

;; Python black
(use-package blacken
  :straight (blacken :host github :repo "pythonic-emacs/blacken")
  :custom
  (blacken-fast-unsafe t)
  :bind
  ("C-c f p" . blacken-buffer))

(use-package ein
  :straight t)

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

(use-package slime
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))


;; Org Mode
;; See also: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(defvar-local my/gtd-root "~/codes/notes/roam-research-notes-hugo/gtd/")
(defvar-local my/gtd-main (s-concat my/gtd-root "gtd.org"))
(defvar-local my/gtd-inbox (s-concat my/gtd-root "inbox.org"))
(defvar-local my/gtd-tickler (s-concat my/gtd-root "tickler.org"))
(defvar-local my/gtd-someday (s-concat my/gtd-root "someday.org"))

(defun ck/org-confirm-babel-evaluate (lang body)
  "LANG and BODY."
  (message "LANG: %s" lang)
  (not (or (string= lang "dot"))))

(use-package org
  :straight (:type git :host github :repo "bzg/org-mode")
  :after ein
  :bind
  ("C-c c" . org-capture)
  ("C-c a o" . org-agenda)
  :custom
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-odd-level-only nil)
  (org-insert-heading-respect-content nil)
  (org-M-RET-may-split-line '((item) (default . t)))
  (org-special-ctrl-a/e t)
  (org-return-follows-link nil)
  (org-use-speed-commands t)
  (org-startup-align-all-tables nil)
  (org-log-into-drawer nil)
  (org-tags-column 1)
  (org-ellipsis " \u25bc" )
  (org-speed-commands-user nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-completion-use-ido t)
  (org-indent-mode t)
  (org-startup-truncated nil)
  (org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)
  :init
  (setq org-agenda-files
        (list
         my/gtd-inbox
         my/gtd-main
         my/gtd-tickler))

  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,(s-concat my/gtd-root "inbox.org") "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline ,(s-concat my/gtd-root "tickler.org") "Tickler")
                                 "* %i%? \n %U")))

  (setq org-refile-targets `((,(s-concat my/gtd-root "gtd.org") :maxlevel . 3)
                             (,(s-concat my/gtd-root "someday.org") :level . 1)
                             (,(s-concat my/gtd-root "tickler.org") :maxlevel . 2)))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t)
     (dot . t))))

;; ;; Now we are using org-roam
;; (use-package org-wiki
;;   :straight (org-wiki :host github :repo "caiorss/org-wiki")
;;   :init
;;   (setq org-wiki-location "~/codes/incubating/notes")
;;   (setq org-wiki-template
;;         (string-trim
;; "
;; #+TITLE: %n
;; #+DESCRIPTION:
;; #+KEYWORDS:
;; #+STARTUP:  content
;; #+DATE: %d
;; #+SETUPFILE: assets/themes/theme.setup
;;
;; - [[wiki:index][Index]]
;;
;; - Related:
;;
;; * %n
;; ")))

(use-package org-journal
  :after org
  :straight t
  :bind
  ("C-c j s" . org-journal-search)
  ("C-c j f" . org-journal-open-next-entry)
  ("C-c j b" . org-journal-open-previous-entry)
  ("C-c j j" . org-journal-new-entry)
  :custom
  (org-journal-file-type 'weekly)
  (org-journal-dir "~/codes/notes/roam-research-notes-hugo/journal")
  :init
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-enable-agenda-integration t))

(use-package org-roam
  :after org
  :straight t
  :hook
  (after-init . org-roam-mode)
  :bind
  (:map org-roam-mode-map
        (("C-c n n" . org-roam-new-file)
         ("C-c n l" . org-roam)
         ("C-c n t" . org-roam-today)
         ("C-c n f" . org-roam-find-file)
         ("C-c n b" . org-roam-switch-to-buffer)
         ("C-c n g" . org-roam-graph-show))
        :map org-mode-map
        (("C-c n i" . org-roam-insert)))
  :custom
  (org-roam-directory "~/codes/notes/roam-research-notes-hugo/content-org")
  (org-roam-capture-templates `(("d" "default"
                                 plain #'org-roam-capture--get-point "%?"
                                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                 :head "#+TITLE: ${title}
#+AUTHOR: Gray King
#+DATE: %U
#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: notes
" :unnarrowed t))))

(use-package ox-hugo
  :straight t
  :after (ox org-mode))

(use-package org-roam-server
  :straight (:host github :repo "org-roam/org-roam-server"
             :files ("*.el" "*.html" "assets"))
  :after simple-httpd org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 1212
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


(use-package org-superstar
  :straight t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

;; Some useful modes
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

;; ERC to chat via IRC

;; ;; enable TLS
;; (use-package tls
;;   :straight t
;;   :custom
;;   (tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
;;                     "gnutls-cli --priority secure256"
;;                     "gnutls-cli --priority secure256 -p %p %h")))

; M-x my/start-irc
(defun my/start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))


(defun my/start-mozilla-irc ()
  "Connect to Mozilla-irc."
  (interactive)
  (erc-tls :server "irc.mozilla.org" :port 6697
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))


(use-package erc
  :straight t
  :after tls
  :commands (my/start-irc my/start-mozilla-irc))

(use-package keypression
  :straight (:host github :repo "chuntaro/emacs-keypression" :branch "master")
  :custom
  ;; (keypression-use-child-frame t)
  ;; (keypression-frame-justify 'keypression-left-justified)
  (keypression-fade-out-delay 1.0)
  (keypression-cast-command-name t)
  (keypression-cast-command-name-format "%s  %s")
  (keypression-combine-same-keystrokes t)
  (keypression-font-face-attribute '(:width normal :height 200 :weight bold)))

(use-package urlview
  :straight (:host github :repo "coldnight/emacs-urlview" :branch "master"))

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

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" default))
 '(org-agenda-files
   '("~/codes/notes/roam-research-notes-hugo/gtd/inbox.org" "~/codes/notes/roam-research-notes-hugo/gtd/gtd.org" "~/codes/notes/roam-research-notes-hugo/gtd/tickler.org")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)

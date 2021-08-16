;;; init.el -- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Use use-package and straight to manage Emacs configuration
;;;
;;; Code:
;;;

;; custom file
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

(if (file-exists-p custom-file)
    (load custom-file))

;; use package
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (require 'use-package)

  (defconst secret-el-path (expand-file-name ".secret.el" user-emacs-directory))

  (if(file-exists-p secret-el-path)
      (load-file secret-el-path)))

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

(use-package quickrun
  :straight t)

(use-package emamux
  :straight (emamux :host github :repo "coldnight/emamux" :branch "out-of-tmux")
  :init
  (advice-add 'emamux:send-command :filter-return #'my/activate-terminal)
  (advice-add 'emamux:run-command :filter-return #'my/activate-terminal)
  (advice-add 'emamux:run-last-command :filter-return #'my/activate-terminal)
  (advice-add 'emamux:run-region :filter-return #'my/activate-terminal)
  :bind
  ("C-c t s" . emamux:send-command)
  ("C-c t !" . emamux:run-command)
  ("C-c t r" . emamux:run-last-command)
  ("C-c t w" . emamux:run-region))

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
  :custom
  (projectile-enable-caching t)
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
  (not (or (string= lang "dot"))))


(use-package org
  :straight (:type git :host github :repo "bzg/org-mode")
  :after ein
  :bind
  ("C-c c" . org-capture)
  ("C-c a o" . org-agenda)
  ("C-c C-." . org-mark-ring-goto)
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
  :custom-face
  (org-headline-done ((nil (:strike-through t))))
  :init
  (require 'org-id)
  (defun my/org-id-update-id-locations-current-dir()
    "Update id locations from current dir."
    (interactive)
    (org-id-update-id-locations (directory-files "." t "\.org\$" t)))
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
  ;; (org-journal-enable-encryption t)
  ;; (org-journal-encrypt-journal t)
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
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory "~/codes/notes/roam-research-notes-hugo/content-org")
  (org-roam-capture-templates `(("d" "default" plain "%?"
                                 :unnarrowed t
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}
#+AUTHOR: Gray King
#+DATE: %U
#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: notes
")))))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package ox-hugo
  :straight t
  :after (ox org-mode))

(use-package org-superstar
  :straight t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-pomodoro
  :straight t
  :hook
  (org-pomodoro-finished . (lambda ()
                             (call-process "terminal-notifier" nil 0 nil
                                           "-message" "Take a break to eat 🍅！"
                                           "-sound" "Pebble"
                                           "-sender" "org.gnu.Emacs")))
  (org-pomodoro-short-break-finished . (lambda ()
                                         (call-process "terminal-notifier" nil 0 nil
                                           "-message" "🍅 ready to go！"
                                           "-sound" "Heroine"
                                           "-sender" "org.gnu.Emacs")))
  (org-pomodoro-long-break-finished . (lambda ()
                                        (call-process "terminal-notifier" nil 0 nil
                                                      "-message" "🍅🍅🍅 ready to go！"
                                                      "-sound" "Heroine"
                                                      "-sender" "org.gnu.Emacs"))))

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

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


(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)
;;; init.el ends here

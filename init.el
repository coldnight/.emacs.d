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
                         :files ("*.el" "themes/*.el"))
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

;; (use-package dashboard
;;   :straight (dashboard :host github :repo "emacs-dashboard/emacs-dashboard"
;;                        :files ("*.el" "banners/"))
;;   :init
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

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

;;; Shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-directories
        (append '(".git"
                  ".svn"
                  ".tox"
                  ".venv"
                  ".gradle"
                  ".meghanada"
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
                projectile-globally-ignored-files)))

(use-package helm-projectile
  :straight t
  :init
  (setq projectile-completion-system 'helm)
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

(use-package lsp-mode
  :straight (lsp-mode :host github :repo "emacs-lsp/lsp-mode")
  :hook
  (python-mode . lsp)
  (go-mode . lsp)
  (c++-mode . lsp)
  :after (company flycheck))

(use-package dap-mode
  :straight (dap-mode :host github :repo "emacs-lsp/dap-mode")
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-go)
  (setq dap-go-debug-program `("node" ,(expand-file-name "~/.vscode/extensions/ms-vscode.go-0.9.2/out/src/debugAdapter/goDebug.js")))
  :after (lsp-mode))

(use-package company-lsp
  :straight t
  :init
  (push 'company-lsp company-backends)
  :after (company lsp-mode))

(use-package lsp-ui
  :straight t
  :after lsp-mode)

(use-package whitespace
  :straight whitespace
  :hook
  (prog-mode . whitespace-mode)
  (before-save . (lambda()
                   (delete-trailing-whitespace)))
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

;; Programming Language Mode
(use-package go-mode
  :straight (go-mode :host github :repo "dominikh/go-mode.el")
  :mode "\\.go\\'"
  :custom
  (gofmt-command "/usr/local/bin/gofmt")
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

(use-package meghanada
  :straight t
  :hook
  (java-mode .
                  (lambda ()
                    ;; meghanada-mode on
                    (meghanada-mode t)
                    (flycheck-mode +1)
                    (setq c-basic-offset 2)
                    ;; use code format
                    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  :init
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))


;; Org Mode
;; See also: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(defvar-local my/gtd-root "~/codes/gtd/work/")
(defvar-local my/gtd-main (s-concat my/gtd-root "gtd.org"))
(defvar-local my/gtd-inbox (s-concat my/gtd-root "inbox.org"))
(defvar-local my/gtd-tickler (s-concat my/gtd-root "tickler.org"))
(defvar-local my/gtd-someday (s-concat my/gtd-root "someday.org"))

(use-package org
  :bind
  ("C-c c" . org-capture)
  ("C-c a o" . org-agenda)
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

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(use-package org-wiki
  :straight (org-wiki :host github :repo "caiorss/org-wiki")
  :init
  (setq org-wiki-location "~/codes/incubating/notes")
  (setq org-wiki-template
        (string-trim
"
#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  content
#+DATE: %d
#+SETUPFILE: assets/themes/theme.setup

- [[wiki:index][Index]]

- Related:

* %n
")))

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
  (org-journal-dir "~/codes/notes/journal")
  :init
  (defun org-journal-file-header-func ()
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-enable-agenda-integration t))

(use-package org-roam
  :after org
  :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
  :hook
  (org . org-roam-mode)
  (after-init . org-roam--build-cache-async)
  (after-init . org-roam-mode)
  :bind
  (:map org-roam-mode-map
        (("C-c n n" . org-roam-new-file)
         ("C-c n l" . org-roam)
         ("C-c n t" . org-roam-today)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-show-graph))
        :map org-mode-map
        (("C-c n i" . org-roam-insert)))
  :custom
  (org-roam-directory "~/codes/notes/org-roam"))


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
;; (use-package cmake-mode :straight t)

;; ERC to chat via IRC

;; enable TLS
(use-package tls
  :straight t
  :custom
  (tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --priority secure256"
                    "gnutls-cli --priority secure256 -p %p %h")))

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
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" default)))
 '(uniquify-buffer-name-style nil nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

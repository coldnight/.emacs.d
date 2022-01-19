;;; init-programming --- Configures Programming.
;;; Commentary:
;;;
;;; As I want to load all the programming plugins by their major modes,
;;; I put all the related plugins here.
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))


(defconst my/programming-major-modes
  '(python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode)
  "The major-modes that I'm using for programming.")


;; Enhance: appearance & effective
(use-package ligature
  :straight
  (ligature :host github :repo "mickeynp/ligature.el")
  :hook
  ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode prog-mode) . ligature-mode)
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
                                       "\\\\" "://")))

(use-package autopair
  :straight (autopair :host github :repo "joaotavora/autopair")
  :hook
  ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode org-mode prog-mode) . autopair-mode))

(use-package rainbow-delimiters
  :straight t
  :hook
  ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode org-mode prog-mode) . rainbow-delimiters-mode))

;; Check
(use-package flycheck
  :straight t
  :hook
  ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode org-mode php-mode web-mode) . flycheck-mode)
  :custom
  ;; .rst 文件禁用 flycheck
  (flycheck-disabled-checkers '(rst grammarly)))

(use-package pos-tip
  :straight t
  :after flycheck)

(use-package flycheck-pos-tip
  :straight t
  :after (flycheck pos-tip)
  :config
  (flycheck-pos-tip-mode))

(use-package flycheck-swiftlint
  :straight t
  :after flycheck
  :hook
  (swift-mode . flycheck-swiftlint-setup))

(use-package aggressive-indent-mode
  :straight t
  :hook
  (emacs-lisp-mode  . aggressive-indent-mode))


;; Completion
(use-package company
  :straight t
  :hook
  ((python-mode go-mode rust-mode scala-mode emacs-lisp-mode php-mode web-mode) . company-mode))

(use-package company-jedi
  :straight t
  :after company)


;;; Programming Language Specific
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
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :after (web-mode flycheck company))

;; Some useful modes
(use-package swift-mode :straight t
  :mode "\\.swift\'")
;; (use-package indent-guide :straight t)
;; (use-package highlight-indentation :straight t)
(use-package json-mode :straight t
  ;; mode to defer
  :mode "\\.json\\'")
(use-package yaml-mode :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'"))
(use-package less-css-mode :straight t
  ;; mode to defer
  :mode "\\.less\\'")
(use-package groovy-mode :straight t
  :mode "\\.gradle\\'")
(use-package gradle-mode :straight t
  :mode "\\.gradle\\'")
(use-package rust-mode :straight t
  :mode "\\.rs\\'")
(use-package htmlize :straight t
  :mode "\\.html\\'")
(use-package php-mode :straight t
  ;; mode to defer
  :mode "\\.php\\'")
(use-package kotlin-mode :straight t
  :mode "\\.kt\\'")
(use-package dockerfile-mode :straight t
  :mode "Dockerfile\\'")
(use-package cmake-mode
  :mode "CMakeLists.txt\\'"
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

;; C/C++
(use-package cc-vars
  :straight (:type built-in)
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (tab-width 4)
  (indent-tabs-mode t)
  :hook
  ((c-mode c++-mode) . (lambda() t)))

(provide 'init-programming)
;;; init-programming.el ends here

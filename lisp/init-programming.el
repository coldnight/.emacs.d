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

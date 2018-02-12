;;; init-major-modes.el -- 初始化各个主模式
;;;
;;; Commentary:
;;;
;;; 包括:
;;;
;;; - Python
;;; - JavaScript
;;; - Less CSS
;;; - Markdown
;;;
;;; Code:
;; Python
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))


;; HTML/XML 等使用 4 个空格(而非2个)
(setq sgml-basic-offset 4)

(provide 'init-major-modes)
;; init-major-modes.el ends here

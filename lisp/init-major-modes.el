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
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -i")

;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

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


;; 设置缩进级别空格数
(setq my/web-mode-offset 2)


;; JS2 设置缩进
(setq js2-basic-offset my/web-mode-offset)

;; HTML/XML 缩进
(setq sgml-basic-offset my/web-mode-offset)

(defun my/current-buffer-suffix()
  "Return suffix of current buffer."

  (nth 0 (cdr (split-string (buffer-name) "\\."))))


;; Web mode
(defun my/web-mode-hook ()
  "Hooks for Web mode."

  ;; Vue.js 下禁用 script 内 padding
  (if (string= (my/current-buffer-suffix) "vue")
      (setq web-mode-style-padding 0)
      (setq web-mode-script-padding 0))

  ;; 设置缩进级别
  (setq web-mode-markup-indent-offset my/web-mode-offset)
  (setq web-mode-css-indent-offset my/web-mode-offset)
  (setq web-mode-code-indent-offset my/web-mode-offset)
  (setq web-mode-attr-indent-offset my/web-mode-offset))

(add-hook 'web-mode-hook  'my/web-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))


(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(setq meghanada-java-path "java")
(setq meghanada-maven-path "mvn")

(provide 'init-major-modes)
;; init-major-modes.el ends here

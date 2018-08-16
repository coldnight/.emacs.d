;; init-shell.el --- 配置 shell 相关
;;; Commentary:
;;
;;
;; 配置 Shell 相关
;;; Code:

(setq shell-file-name "/usr/local/bin/zsh")

;; ansi-color
;; ansi-color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq ansi-color-for-comint-mode t)

;; with-eidtor
(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'shell-mode-hook 'with-editor-export-git-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(provide 'init-shell)
;; init-shell.el ends here

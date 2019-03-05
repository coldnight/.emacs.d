;;; init-lsp-mode.el -- LSP Mode
;;;
;;; Commentary:
;;;
;;; 配置 LSP
;;;
;;; Code:

(require 'lsp-mode)

(add-hook 'python-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)

(provide 'init-lsp-mode)
;; init-lsp-mode.el ends here

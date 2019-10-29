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
(add-hook 'c++-mode-hook #'lsp)

;; DAP(Debug Adapter Protocol)
(dap-mode 1)
(dap-ui-mode 1)
(require 'dap-go)
(setq dap-go-debug-program `("node" ,(expand-file-name "~/.vscode/extensions/ms-vscode.go-0.9.2/out/src/debugAdapter/goDebug.js")))

(provide 'init-lsp-mode)
;; init-lsp-mode.el ends here

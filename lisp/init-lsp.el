;;; init-lsp --- Initialize Language Server Mode.
;;; Commentary:
;;;
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package yasnippet
  :straight t
  :commands yas-minor-mode)

(use-package lsp-mode
  :straight (lsp-mode :host github :repo "emacs-lsp/lsp-mode")
  :hook
  ((python-mode . lsp)
   (c++-mode . lsp)
   (rust-mode . lsp)
   (rust-mode . (lambda()
				          (add-hook 'before-save-hook #'lsp-format-buffer t t)))
   (php-mode . lsp)
   (cmake-mode . lsp)
   (go-mode . lsp)
   (go-mode . (lambda ()
                "Set up before-save hooks to format buffer and add/delete imports."
                ;; Make sure you don't have other gofmt/goimports hooks enabled.
                (add-hook 'before-save-hook #'lsp-format-buffer t t)
                (add-hook 'before-save-hook #'lsp-organize-imports t t)))
   (dart-mode . lsp)
   (lsp-mode . lsp-lens-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . yas-minor-mode))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-rust-server 'rust-analyzer)
  (lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
  :after (company flycheck))

(use-package lsp-java
  :straight t
  :after lsp-mode
  :custom
  ;; We are using Java8 and the 0.57 is the latest version to support Java 8,
  ;; otherwise Java 11 is required.
  (lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
  (lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
											:path "/Users/wh/.sdkman/candidates/java/8.0.292.hs-adpt"
											:default t)])
  :hook
  (java-mode . lsp))

(use-package lsp-java-lambok
  :straight (lsp-java-lambok :host github :repo "sei40kr/lsp-java-lombok")
  :after lsp-java
  :hook
  (java-mode . lsp-java-lombok))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :straight (lsp-metals :host github :repo "emacs-lsp/lsp-metals")
  :custom (lsp-metals-treeview-show-when-views-received t)
  :after lsp-mode
  :hook
  ;; DO NOT move this to lsp-mode, that will break use-package defer
  (scala-mode . lsp))

(use-package lsp-sourcekit
  :straight t
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

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

(use-package lsp-ui :after lsp-mode :straight t  :commands lsp-ui-mode)
(use-package lsp-ivy :after lsp-mode :straight t :commands lsp-ivy-workspace-symbol)

(use-package lsp-dart
  :straight t
  :after (lsp-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here

;;; init-modeline-doom -- Emacs doom-modeline.
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package nyan-mode
  :defer 0.5
  :straight t
  :config
  (nyan-mode 1))

(use-package doom-modeline
  :straight t
  :after nyan-mode
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-all))

(use-package hide-mode-line
  :straight (hide-mode-line :host github :repo "hlissner/emacs-hide-mode-line")
  :hook
  (dashboard-mode . hide-mode-line-mode)
  (vterm-mode . hide-mode-line-mode)
  (magit-mode . hide-mode-line-mode))

(provide 'init-modeline-doom)
;;; init-modeline-doom.el ends here

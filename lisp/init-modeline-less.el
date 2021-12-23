;;; init-modeline-less --- Use awesome-tray as modeline.
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package awesome-tray
  :straight (awesome-tray :host github :repo "manateelazycat/awesome-tray")
  :config
  (setq awesome-tray-active-modules '("location" "git" "belong" "mode-name")
        awesome-tray-info-padding-right 1)
  (awesome-tray-mode 1))
(provide 'init-modeline-less)
;;; init-modeline-less.el ends here

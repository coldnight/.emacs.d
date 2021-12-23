;;; init-modeline-less --- Use awesome-tray as modeline.
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package awesome-tray
  :straight (awesome-tray :host github :repo "manateelazycat/awesome-tray")
  :config
  ;; Copy mode-line's face-attributes to the header-line,
  ;; to avoid the face-attributes are changed by awesome-tray that effect to header-line too.
  (set-face-attribute 'header-line nil
                      :foreground (face-attribute 'mode-line :foreground)
                      :background (face-attribute 'mode-line :background)
                      ;; :height of mode-line is also unspecified, so we set it directly.
                      :height 150
                      :box (face-attribute 'mode-line :box))

  (setq awesome-tray-active-modules '("buffer-read-only" "location" "git" "belong" "mode-name")
        awesome-tray-info-padding-right 1
        awesome-tray-mode-line-active-color "#727d97"
        awesome-tray-mode-line-inactive-color "#959eb1")
  (awesome-tray-mode 1))

;; (use-package taoline
;;   :straight (taoline :host github :repo "11111000000/taoline")
;;   :custom
;;   (taoline-show-git-branch      t)
;;   (taoline-show-dir             nil)
;;   (taoline-show-time            nil)
;;   (taoline-show-previous-buffer nil)
;;   :config
;;   (taoline-mode t))

(provide 'init-modeline-less)
;;; init-modeline-less.el ends here

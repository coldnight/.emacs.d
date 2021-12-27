;;; init-modeline-less --- Use awesome-tray as modeline.
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(defun my/module-buffer-state-info ()
  "Module for awesome-tray to show state of buffer."
  (if (not (eq buffer-file-name nil))
      (if (eq buffer-read-only t)
          (if (buffer-modified-p)
              "*RO"
            "RO")
        (if (buffer-modified-p)
            "*RW"
          "RW"))
    "--"))

(defface my/module-buffer-state-face
  '((((background light))
     :foreground "#cc7700" :bold t)
    (t
     :foreground "#ff9500" :bold t))
  "Buffer-state module face."
  :group 'awesome-tray)

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

  (setq awesome-tray-active-modules '("buffer-state" "buffer-name" "location" "git" "belong" "mode-name")
        awesome-tray-info-padding-right 1
        awesome-tray-mode-line-active-color "#727d97"
        awesome-tray-mode-line-inactive-color "#959eb1")
  (add-to-list 'awesome-tray-module-alist
               '("buffer-state" . (my/module-buffer-state-info my/module-buffer-state-face)))
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

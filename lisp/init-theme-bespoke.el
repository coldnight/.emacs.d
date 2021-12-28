;;; init-theme-bespoke --- Emacs look via bespoke-theme and awesome-tray
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  (load-theme 'bespoke t))

;; Vertical window divider
(use-package frame
  :straight (:type built-in)
  :hook
  ;; Make sure new frames use window-divider
  (before-make-frame . window-divider-mode)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

;; Make a clean & minimalist frame
(use-package frame
  :straight (:type built-in)
  :config
  (setq-default default-frame-alist
                (append (list
                         '(internal-border-width . 20)
                         '(left-fringe    . 0)
                         '(right-fringe   . 0)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

;; Dim inactive windows
(use-package dimmer
  :straight (:host github :repo "gonewest818/dimmer.el")
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))



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
     :foreground "#00796b" :bold t)
    (t
     :foreground "#8eb89d" :bold t))
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
  ;; colors are copied from https://github.com/mclear-tools/bespoke-themes
  (setq awesome-tray-active-modules '("location" "buffer-state" "buffer-name" "git" "belong" "mode-name")
        awesome-tray-info-padding-right 1
        awesome-tray-mode-line-active-color "#eceff1"
        awesome-tray-mode-line-inactive-color "#282b35"
        awesome-tray-buffer-name-max-length 20)
  (set-face-attribute 'awesome-tray-module-location-face nil
                      :foreground "#bc85cf")
  (set-face-attribute 'awesome-tray-module-buffer-name-face nil
                      :foreground "#bc85cf")
  (set-face-attribute 'awesome-tray-module-git-face nil
                      :foreground "#bf616a")
  (set-face-attribute 'awesome-tray-module-mode-name-face nil
                      :foreground "#8eb89d")
  (add-to-list 'awesome-tray-module-alist
               '("buffer-state" . (my/module-buffer-state-info my/module-buffer-state-face)))
  (awesome-tray-mode 1))

(provide 'init-theme-bespoke)
;;; init-theme-bespoke.el ends here

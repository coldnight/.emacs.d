;;; init-theme-modus.el -- Use modus to define the look of Emacs.
;;; Commentary:
;;;
;;;  More information to see the documentation at:
;;;    https://protesilaos.com/emacs/modus-themes
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

;;; (use-package modus-themes
;;;   :straight t
;;;   :init
;;;   ;; Add all your customizations prior to loading the themes
;;;   (setq modus-themes-italic-constructs t
;;;         modus-themes-bold-constructs nil
;;;         modus-themes-region '(bg-only no-extend))
;;;
;;;   ;; Load the theme files before enabling a theme
;;;   (modus-themes-load-themes)
;;;   :config
;;;   ;; Load the theme of your choice:
;;;   ;; (modus-themes-load-operandi)
;;;   ;; OR
;;;   (modus-themes-load-vivendi)
;;;   :bind ("<f5>" . modus-themes-toggle))
;;;

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :init
  (color-theme-sanityinc-tomorrow-bright))

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


(defun my/module-inbox-count-info()
  "Show inbox count."
  (if (stringp my/mu4e-unread-messages-count)
      my/mu4e-unread-messages-count
    "Mail: N/A"))

(defface my/module-inbox-count-face
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
  (setq awesome-tray-active-modules '("inbox-count" "location" "buffer-state" "buffer-name" "git" "belong" "mode-name")
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
  (add-to-list 'awesome-tray-module-alist
               '("inbox-count" . (my/module-inbox-count-info my/module-inbox-count-face)))
  (awesome-tray-mode 1))

(provide 'init-theme-modus)
;;; init-theme-modus.el ends here

;;; init-appearance.el -- 配置 Emacs 外观
;;;
;;; Commentary:
;;;
;;; 配置主题和 Mode Line
;;;
;;; Code:
;;;

;;; Theme
;; (load-theme 'material t)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-nord t)
(load-theme 'doom-dracula t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; (load-theme 'moe-dark t)

;;; Modeline
;; mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)

;; Not compatible with Emacs 26
;; ;;; Indent guide
;; (require 'indent-guide)
;; (indent-guide-global-mode)

;; Git Gutter
;; (global-git-gutter-mode +1)

;; Font
(cond ((string-equal system-type "darwin")
  (progn
    (set-face-attribute 'default nil
                        :family "Source Code Pro Light" :height 145 :weight 'normal)))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (set-face-attribute 'default nil
                        :family "Source Code Pro" :height 100 :weight 'normal))))


;; Transparent
;;
;;   (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;   (set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))


;; Emoji
(add-hook 'after-init-hook #'global-emojify-mode)

(provide 'init-appearance)
;; init-appearance.el ends here

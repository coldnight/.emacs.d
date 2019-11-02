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
;; (require 'smart-mode-line)
;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)
;; (setq sml/theme 'dark)
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Not compatible with Emacs 26
;; ;;; Indent guide
;; (require 'indent-guide)
;; (indent-guide-global-mode)

;; Git Gutter
;; (global-git-gutter-mode +1)

;; Emoji
(setq emojify-emoji-styles '(unicode github))
(add-hook 'after-init-hook #'global-emojify-mode)

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(provide 'init-appearance)
;; init-appearance.el ends here

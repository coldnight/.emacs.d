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
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;;; Modeline
;; mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)

;;; Indent guide
(require 'indent-guide)
(indent-guide-global-mode)

;; Git Gutter
(global-git-gutter-mode +1)

;; ansi-color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq ansi-color-for-comint-mode t)

(provide 'init-appearance)
;; init-appearance.el ends here

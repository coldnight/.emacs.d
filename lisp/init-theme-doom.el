;;; init-theme-doom -- Emacs look via doom-emacs.
;;;
;;; Commentary:
;;;
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package doom-themes
  :ensure t
  :straight (doom-themes :host github :repo "hlissner/emacs-doom-themes"
                         :files ("*.el" "themes"))
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-nord t)
  (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package centaur-tabs
  :straight t
  :demand
  :defer 2
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-style "wave")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-cycle-scope 'groups)
  :bind
  ("C-c t p" . centaur-tabs-backward)
  ("C-c t n" . centaur-tabs-forward)
  ("C-c t t" . centaur-tabs-counsel-switch-group))

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

(provide 'init-theme-doom)
;;; init-theme-doom.el ends here

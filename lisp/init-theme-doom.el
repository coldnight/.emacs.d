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

;; (use-package nyan-mode
;;   :defer 0.5
;;   :straight t
;;   :config
;;   (nyan-mode 1))
;;
;; (use-package doom-modeline
;;   :straight t
;;   :after nyan-mode
;;   :config
;;   (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-buffer-file-name-style 'truncate-all))
;;
;; (use-package hide-mode-line
;;   :straight (hide-mode-line :host github :repo "hlissner/emacs-hide-mode-line")
;;   :hook
;;   (dashboard-mode . hide-mode-line-mode)
;;   (vterm-mode . hide-mode-line-mode)
;;   (magit-mode . hide-mode-line-mode))

(use-package awesome-tray
  :straight (awesome-tray :host github :repo "manateelazycat/awesome-tray")
  :config
  (setq awesome-tray-active-modules '("location" "git" "belong" "file-path" "mode-name")
        awesome-tray-info-padding-right 1)
  (awesome-tray-mode 1))

(use-package dashboard
  :straight (dashboard :host github :repo "emacs-dashboard/emacs-dashboard"
                       :files ("*.el" "banners"))
  :after all-the-icons
  :custom
  (dashboard-center-content 1)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner "~/.emacs.d/logo.png")
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (registers . 5)))
  (dashboard-set-navigator t)
  ;; Format: "(icon title help action face prefix suffix)"
  (dashboard-navigator-buttons
   `(;; line1
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "http://github.com/coldnight")))
      (,(all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0)
       "Mail"
       "Open mu4e"
       (lambda (&rest _) (counsel-M-x "^mu4e$")))
      (,(all-the-icons-octicon "issue-opened" :height 1.1 :v-adjust 0.0)
       "Help"
       "?/h"
       (lambda (&rest _) (counsel-M-x "^help$")))
      (,(all-the-icons-octicon "x" :height 1.1 :v-adjust 0.0)
       "Quit"
       "Quit Emacs"
       (lambda (&rest _) (counsel-M-x "^kill-emacs$"))))
     ((,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
       "Update"
       "Upate straight.el Packages."
       (lambda (&rest _) (counsel-M-x "^straight-pull-all$")) nil "<" ">")
      (,(all-the-icons-octicon "pin" :height 1.1 :v-adjust 0.0)
       "Freeze"
       "Freeze straight.el packages."
       (lambda (&rest _) (counsel-M-x "^straight-freeze-versions$")) nil "<" ">")
      (,(all-the-icons-octicon "clock" :height 1.1 :v-adjust 0.0)
       "Thaw"
       "Thaw straight.el packages to latest version."
       (lambda (&rest _) (counsel-M-x "^straight-thaw-versions$")) nil "<" ">"))))
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  :hook
  (dashboard-after-initialize . (lambda () (dashboard-mode)))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(provide 'init-theme-doom)
;;; init-theme-doom.el ends here

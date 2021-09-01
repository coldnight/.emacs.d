;;; init-ivy -- Enhance Emacs via ivy & swiper & counsel
;;;
;;; Commentary:
;;;
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package counsel
  :straight t
  :custom
  (counsel-find-file-at-point t)
  :config
  (counsel-mode +1)
  :bind
  ("C-x b" . counsel-switch-buffer)
  ("C-c a p" . counsel-ag)
  ("M-y" . counsel-yank-pop)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> o" . counsel-describe-symbol)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ;; ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  (:map minibuffer-local-map
        (("C-r" . counsel-minibuffer-history))))

(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-wrap t)
  :bind
  ("\C-s" . swiper)
  ("\C-r" . swiper-backward)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume))

(use-package ivy-posframe
  :straight t
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper          . ivy-posframe-display-at-point)
     (complete-symbol . ivy-posframe-display-at-point)
     (t . ivy-posframe-display)))
  (ivy-posframe-parameters '((left-fringe . 8)
                             (right-fringe . 8)))
  :config
  (ivy-posframe-mode 1))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :bind
  (:map projectile-mode-map
        (("C-c p" . projectile-command-map)))
  :config
  (counsel-projectile-mode +1))

(use-package ivy-rich
  :straight t
  :after (ivy)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package all-the-icons-ivy-rich
  :straight t
  :after (ivy-rich)
  :config (all-the-icons-ivy-rich-mode 1))

(provide 'init-ivy)

;;; init-ivy.el ends here

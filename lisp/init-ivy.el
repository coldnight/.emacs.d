;;; init-ivy -- Enhance Emacs via ivy & swiper & counsel
;;;
;;; Commentary:
;;;
;;; Code:
(use-package counsel
  :straight t
  :init
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
  :init
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
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  :init
  (ivy-posframe-mode 1))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :bind
  (:map projectile-mode-map
        (("C-c p" . projectile-command-map)))
  :init
  (counsel-projectile-mode +1))

(use-package ivy-rich
  :straight t
  :after (ivy)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1))

(use-package all-the-icons-ivy-rich
  :straight t
  :after (ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))

(provide 'init-ivy)

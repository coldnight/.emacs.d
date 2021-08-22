;;; init-helm -- Use helm to enhance Emacs
;;; Commentary:
;;;
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(defun my//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :straight (helm :host github :repo "emacs-helm/helm")
  :commands heml-config
  :hook
  (helm-minibuffer-set-up . my//helm-hide-minibuffer-maybe)
  :init
  (require 'helm)
  (require 'helm-config)
  (setq helm-google-suggest-use-curl-p t)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  :bind
  ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x C-f" . helm-find-files)
  (:map helm-map
        (("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-selection-action))))

(use-package helm-ag
  :straight (helm-ag :host github :repo "syohex/emacs-helm-ag")
  :bind
  ("C-c a s" . helm-do-ag-this-file)
  ("C-c a a" . helm-do-ag)
  ("C-c a p" . helm-do-ag-project-root)
  ("C-c a f" . helm-do-ag-this-file)
  :after (helm))

(use-package helm-dash
  :straight (helm-dash :host github :repo "dash-docs-el/helm-dash")
  :bind
  ("C-c d s" . helm-dash))

(use-package helm-icons
  :straight t
  :after helm
  :init
  (helm-icons-enable))

(use-package helm-projectile
  :straight t
  :after (projectile helm)
  :custom
  (projectile-completion-system 'helm)
  :init
  (helm-projectile-on))

(use-package helm-lsp :straight t :commands helm-lsp-workspace-symbol :after lsp-mode)

(provide 'init-helm)

;;; init-helm.el ends here

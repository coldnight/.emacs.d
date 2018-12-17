;;; init-key-bind.el -- 按键绑定
;;;
;;; Commentary:
;;;
;;; 配置按键绑定
;;;
;;;
;;; Code:
;;;

;; windmove
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w l") 'windmove-right)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w j") 'windmove-down)

;; smex
;; (require 'smex)
;; (global-set-key (kbd "M-x") 'smex)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; neotree
(require 'neotree)
(global-set-key (kbd "C-c s n") 'neotree-toggle)

(require 'mu4e)
(global-set-key (kbd "C-c s m") 'mu4e)

;; helm-ag
(global-set-key (kbd "C-c a s") 'helm-do-ag-this-file)
(global-set-key (kbd "C-c a a") 'helm-do-ag)
(global-set-key (kbd "C-c a p") 'helm-do-ag-project-root)
(global-set-key (kbd "C-c a f") 'helm-do-ag-this-file)

;; helm-dash
(global-set-key (kbd "C-c s d") 'helm-dash)

(defun my/open-remark-org() (interactive)
  "Open my own remark in org-mode"
  (find-file "~/Documents/org-modes/remark.org"))

(global-set-key (kbd "C-c s r") 'my/open-remark-org)

;; Shell

(global-set-key (kbd "C-c s s") 'shell)
(global-set-key (kbd "C-c s r") 'rename-buffer)


;; Magit
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m p") 'magit-push-current)
(global-set-key (kbd "C-c m c") 'magit-branch-checkout)
(global-set-key (kbd "C-c m b") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c m f") 'magit-fetch)
(global-set-key (kbd "C-c m m") 'magit-merge)
(global-set-key (kbd "C-c m r") 'magit-rebase)


;; make a temp buffer
(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(global-set-key (kbd "C-c g b") 'generate-buffer)

;; multi-mode
(global-set-key (kbd "C-c t m") 'multi-term)


;; hg reject

(defun switch-hg-reject ()
  (interactive)
  (let ((other-file
     (if (string= (substring (buffer-file-name) -4 nil) ".rej")
         (substring (buffer-file-name) 0 -4)
       (concat (buffer-file-name) ".rej"))))
    (if (file-exists-p other-file)
      (switch-to-buffer (find-file-noselect other-file))
      (message (format "No alternate reject file found" other-file)))))

(global-set-key (kbd "C-c r") 'switch-hg-reject)

(provide 'init-key-bind)
;; init-key-bind.el ends here

;;; init-taking-notes -- 配置使用 Emacs 记录笔记
;;;
;;; Commentary:
;;;
;;; - org-journal 用于日记，以周为单位记录
;;; - org-roam 用于通过 Roam Research 的方式记录笔记
;;;
;;; Code:
;;;


(customize-set-variable 'org-journal-file-type 'weekly)
(customize-set-variable 'org-journal-dir "~/codes/notes/journal")

(require 'org-journal)

(defun org-journal-file-header-func ()
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
(setq org-journal-file-header 'org-journal-file-header-func)

;; rebinding
(global-unset-key (kbd "C-c C-s"))
(global-set-key (kbd "C-c j s") 'org-journal-search)
(global-unset-key (kbd "C-c C-f"))
(global-set-key (kbd "C-c j f") 'org-journal-open-next-entry)
(global-unset-key (kbd "C-c C-b"))
(global-set-key (kbd "C-c j b") 'org-journal-open-previous-entry)
(global-unset-key (kbd "C-c C-j"))
(global-set-key (kbd "C-c j j") 'org-journal-new-entry)

(require 'org-roam)

(add-hook 'org-mode 'org-roam-mode)
(add-hook 'after-init-hook 'org-roam--build-cache-async)

(setq org-roam-directory "~/codes/notes/org-roam")

(global-set-key (kbd "C-c n l") 'org-roam)
(global-set-key (kbd "C-c n t") 'org-roam-today)
(global-set-key (kbd "C-c n f") 'org-roam-find-file)
(global-set-key (kbd "C-c n i") 'org-roam-insert)
(global-set-key (kbd "C-c n g") 'org-roam-show-graph)

(provide 'init-taking-notes)
;;; init-taking-notes.el ends here

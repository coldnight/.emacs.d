;;; init-org-mode.el --- 配置 org-mode
;;; Commentary:
;; org-mode 配置 GTD, 参考:
;;
;;   https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

;;; Code:
(setq-local my/gtd-root "~/codes/gtd/work/")
(setq-local my/gtd-main (s-concat my/gtd-root "gtd.org"))
(setq-local my/gtd-inbox (s-concat my/gtd-root "inbox.org"))
(setq-local my/gtd-tickler (s-concat my/gtd-root "tickler.org"))
(setq-local my/gtd-someday (s-concat my/gtd-root "someday.org"))

(setq org-agenda-files
      (list
       my/gtd-inbox
       my/gtd-main
       my/gtd-tickler))

(setq org-capture-templates `(("t" "Todo [inbox]" entry
                               (file+headline ,(s-concat my/gtd-root "inbox.org") "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline ,(s-concat my/gtd-root "tickler.org") "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets `((,(s-concat my/gtd-root "gtd.org") :maxlevel . 3)
                           (,(s-concat my/gtd-root "someday.org") :level . 1)
                           (,(s-concat my/gtd-root "tickler.org") :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; 禁用 htmlize
;; (setq org-html-htmlize-output-type nil)

;; 使用 pygments 做代码高亮
;; (define-advice org-html-src-block (:override (src-block _contents info) pygmentize)
;;   "Highlight src-block via Pygmentize."
;;   (let ((lang (org-element-property :language src-block))
;;         (code (org-html-format-code src-block info)))
;;     (with-temp-buffer
;;       (call-process-region code nil "pygmentize" nil t nil "-l" lang "-f" "html")
;;       (buffer-string))))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a o") 'org-agenda)
(provide 'init-org-mode)
;; init-org-mode.el ends here

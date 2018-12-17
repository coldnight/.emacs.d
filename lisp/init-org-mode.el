;;; init-org-mode.el --- 配置 org-mode
;;; Commentary:
;; org-mode 配置 GTD, 参考:
;;
;;   https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

;;; Code:

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))
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

(provide 'init-org-mode)
;; init-org-mode.el ends here

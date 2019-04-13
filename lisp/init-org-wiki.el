;;; init-org-wiki.el
;;;
;;; Commentary:
;;;
;;;   配置 org-wiki 用于笔记记录
;;;
;;; Code:


(require 'org-wiki)

(setq org-wiki-location "~/codes/incubating/notes")
(setq org-wiki-template
      (string-trim
"
#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  content
#+DATE: %d
#+SETUPFILE: assets/themes/theme.setup

- [[wiki:index][Index]]

- Related:

* %n
"))
(provide 'init-org-wiki)
;; ends init-org-wiki.el

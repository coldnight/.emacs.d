;;; init-org -- Use Orgmode to organize my notes.
;;;
;;; Commentary:
;;;
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(defun ck/org-confirm-babel-evaluate (lang body)
  "LANG and BODY."
  (not (or (string= lang "dot"))))

;; Emacs IPython Notebook
(use-package ein
  :straight t
  :defer
  :after org)

(use-package org
  :straight (:type git :host github :repo "coldnight/org-mode" :branch "branch_9.5.2")
  :bind
  ("C-c c" . org-capture)
  ("C-c a o" . org-agenda)
  ("C-c C-." . org-mark-ring-goto)
  :custom
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-odd-level-only nil)
  (org-insert-heading-respect-content nil)
  (org-M-RET-may-split-line '((item) (default . t)))
  (org-special-ctrl-a/e t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t) ;; 隐藏标记
  (org-return-follows-link nil)
  (org-catch-invisible-edits 'show-and-error)
  (org-use-speed-commands t)
  (org-startup-align-all-tables nil)
  (org-log-into-drawer nil)
  (org-tags-column 1)
  (org-ellipsis " \u25bc" )
  (org-speed-commands-user nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-completion-use-ido t)
  (org-indent-mode t)
  (org-startup-truncated nil)
  (org-confirm-babel-evaluate 'ck/org-confirm-babel-evaluate)
  ;; Agenda styling
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  :custom-face
  (org-headline-done ((nil (:strike-through t))))
  :config
  ;; 中文行内格式不用加空格
  ;; https://emacs-china.org/t/orgmode/9740/18?u=grayking
  (require 'org)
  (setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)

  (require 'org-id)
  (defun my/org-id-update-id-locations-current-dir()
    "Update id locations from current dir."
    (interactive)
    (org-id-update-id-locations (directory-files "." t "\.org\$" t)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t)
     (dot . t))))

;; ;; Now we are using org-roam
;; (use-package org-wiki
;;   :straight (org-wiki :host github :repo "caiorss/org-wiki")
;;   :init
;;   (setq org-wiki-location "~/codes/incubating/notes")
;;   (setq org-wiki-template
;;         (string-trim
;; "
;; #+TITLE: %n
;; #+DESCRIPTION:
;; #+KEYWORDS:
;; #+STARTUP:  content
;; #+DATE: %d
;; #+SETUPFILE: assets/themes/theme.setup
;;
;; - [[wiki:index][Index]]
;;
;; - Related:
;;
;; * %n
;; ")))

(use-package org-journal
  :straight t
  :bind
  ("C-c j s" . org-journal-search)
  ("C-c j f" . org-journal-open-next-entry)
  ("C-c j b" . org-journal-open-previous-entry)
  ("C-c j j" . org-journal-new-entry)
  :custom
  (org-journal-file-type 'weekly)
  (org-journal-dir (expand-file-name "codes/notes/roam-research-notes-hugo/journal" "~"))
  ;; (org-journal-enable-encryption t)
  ;; (org-journal-encrypt-journal t)
  (org-journal-enable-agenda-integration t)
  (org-journal-file-header
   (lambda (time)
     (concat
      (pcase org-journal-file-type
        (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
        (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
        (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U")
        (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded\n#+HUGO_BASE_DIR: ../\n#+HUGO_SECTION: journal\n#+DATE: %U"))))))

(use-package org-roam
  :straight t
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name "codes/notes/roam-research-notes-hugo/content-org" "~"))
  (org-roam-capture-templates `(("d" "default" plain "%?"
                                 :unnarrowed t
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}
#+AUTHOR: Gray King
#+DATE: %U
#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: notes
")))))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :commands org-roam-ui-mode
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package ox-hugo
  :straight t
  :after (ox org-mode))

;;; (use-package org-superstar
;;;   :straight t
;;;   :after org
;;;   :hook
;;;   (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-modern
  :straight
  (:host github :repo "minad/org-modern")
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-mode))

(use-package org-pomodoro
  :straight t
  :commands org-pomodoro
  :hook
  (org-pomodoro-finished . (lambda ()
                             (call-process "terminal-notifier" nil 0 nil
                                           "-message" "Take a break to eat 🍅！"
                                           "-sound" "Pebble"
                                           "-sender" "org.gnu.Emacs")))
  (org-pomodoro-short-break-finished . (lambda ()
                                         (call-process "terminal-notifier" nil 0 nil
													   "-message" "🍅 ready to go！"
													   "-sound" "Heroine"
													   "-sender" "org.gnu.Emacs")))
  (org-pomodoro-long-break-finished . (lambda ()
                                        (call-process "terminal-notifier" nil 0 nil
                                                      "-message" "🍅🍅🍅 ready to go！"
                                                      "-sound" "Heroine"
                                                      "-sender" "org.gnu.Emacs"))))

(use-package org-fragtog
  :straight t
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-download
  :straight t
  :after org-roam
  :custom
  (org-download-image-dir "images")
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package org-krita
  :straight
  (org-krita :host github :repo "lepisma/org-krita" :files ("*.el" "resources"))
  :custom
  (org-krita-executable "/Applications/krita.app/Contents/MacOS/krita")
  :hook
  (org-mode . org-krita-mode))

(use-package org-fc
  :straight (:host github :repo "l3kn/org-fc" :files ("*.el" "*.org" "awk"))
  :commands
  (org-fc-review org-fc-type-normal-init org-fc-review org-fc-review-buffer)
  :custom
  (org-fc-directories (expand-file-name "codes/notes/roam-research-notes-hugo/flashcards" "~"))
  (org-fc-review-history-file (expand-file-name "codes/notes/roam-research-notes-hugo/flashcards/org-fc-reviews.tsv" "~")))


(use-package org-zotxt
  :straight (:host github :repo "egh/zotxt-emacs")
  :bind
  (:map org-mode-map
        ("C-c z i" . org-zotxt-insert-reference-link)
        ("C-c z a" . org-zotxt-open-attachment))
  :hook
  (org-mode . org-zotxt-mode))

(provide 'init-org)
;;; init-org.el ends here

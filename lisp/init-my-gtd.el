;;; init-my-gtd -- GTD based on org-mode.
;;;
;;; Commentary:
;;;
;;; https://github.com/rougier/emacs-gtd
;;;
;;; Code:
(require 'org)
(require 'org-capture)
(require 'org-agenda)

(setq org-directory (expand-file-name "codes/notes/roam-research-notes-hugo/mind-monkey" "~"))

(defun my/expand-agenda-files (x)
  "Expand agenda file path X to absoluted."
  (expand-file-name x org-directory))

(setq org-agenda-files (cl-map 'list #'my/expand-agenda-files
                               (list "inbox.org" "agenda.org"
                                     "notes.org" "projects.org")))

(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
         ,(concat "* TODO Process \"%a\" %?\n"
                  "/Entered on/ %U"))
        ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
         ,(concat "* %? :meeting:\n"
                  "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "notes.org")
         ,(concat "* Note (%a)\n"
                  "/Entered on/ %U\n" "\n" "%?"))))

(setq org-agenda-hide-tags-regexp ".")
;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s")
;;         (todo   . " ")
;;         (tags   . " %i %-12:c")
;;         (search . " %i %-12:c")))
;;

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my/toggle-agenda-weekly-daily-view()
  "Toggle view of agenda."
  (interactive)
  (if (equal org-agenda-span 'week)
      (setq org-agenda-span 'day)
    (setq org-agenda-span 'week)))

(global-set-key (kbd "C-c a t") 'my/toggle-agenda-weekly-daily-view)

;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      `((,(expand-file-name "projects.org" org-directory) :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED', IGNORE rest."
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(setq org-log-done 'time)
(provide 'init-my-gtd)
;;; init-my-gtd.el ends here.

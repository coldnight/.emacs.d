;;; init-mu4e --- initialize mu4e to receive and send email.
;;;
;;; Commentary:
;;;
;;; Code:
(defvar-local my/mu4e-sys-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")

(if (file-exists-p my/mu4e-sys-path)
    (progn
      (eval-when-compile
        (add-to-list 'load-path my/mu4e-sys-path)
        (require 'mu4e))
      ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
      (setq mu4e-change-filenames-when-moving t)
      (setq mu4e-get-mail-command "mbsync gmail")
      (defun my-make-mu4e-context (name address signature)
        "Return a mu4e context named NAME with :match-func matching
  its ADDRESS in From or CC fields of the parent message. The
  context's `user-mail-address' is set to ADDRESS and its
  `mu4e-compose-signature' to SIGNATURE."
        (lexical-let ((addr-lex address))
          (make-mu4e-context :name name
                             :vars `((user-mail-address . ,address)
                                     (mu4e-compose-signature . ,signature)
                                     (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                                     (mu4e-drafts-folder . "/gmail/Drafts")
                                     (mu4e-trash-folder . "/gmail/Trash")
                                     (mu4e-refile-folder . "/gmail/[Gmail]/All Mail"))
                             :match-func
                             (lambda (msg)
                               (when msg
                                 (or (mu4e-message-contact-field-matches msg :to addr-lex)
                                     (mu4e-message-contact-field-matches msg :cc addr-lex)))))))
      (setq mu4e-contexts
            `( ,(my-make-mu4e-context "main" "grayking.w@gmail.com" nil)))
      ;; Most of the time, I merely want mu4e to re-index my local maildir (because
      ;; I'm running mbsync as a cron job). However, sometimes I want to fetch mails
      ;; immediately. Do this by changing the meaning of a prefix for
      ;; mu4e-update-mail-and-index (bound to "U").
      ;;
      ;; A prefix usually means run in the background, but I don't think I ever want
      ;; that. Change things so a prefix means to call mbsync.
      (defun rjs/mu4e-update-mail-and-index (orig-fun prefix &rest args)
        (interactive "P")
        (if prefix (funcall orig-fun nil) (mu4e-update-index)))
      (advice-add 'mu4e-update-mail-and-index
                  :around #'rjs/mu4e-update-mail-and-index)))

;; SMTP setup
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      starttls-use-gnutls t)

;; Personal info
(setq user-full-name "Gray King")
(setq user-mail-address "grayking.w@gmail.com")
;; gmail setup
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-smtp-user "grayking.w@gmail.com")

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package org-msg
  :straight t
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-fmt "\nHi *%s*,\n\n")
  (org-msg-recipient-names '(("grayking.w@gmail.com" . "Gray King")))
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives '((new		. (text html)))
				(reply-to-html	. (text html))
				(reply-to-text	. (text)))
  (org-msg-convert-citation t)
  (org-msg-signature "

 Regards,

 #+begin_signature
 --
 *Gray*
 /One Emacs to rule them all/
 #+end_signature")
  :init
  (org-msg-mode))

(use-package mu4e-dashboard
  :straight (mu4e-dashboard :host github :repo "rougier/mu4e-dashboard")
  :init
  (defun my/mu4e-dashboard()
    "Open mu4e dashboard."
    (interactive)
    (find-file (expand-file-name "mu4e.org" user-emacs-directory))
    (org-mode)
    (mu4e-dashboard-mode))
  :bind
  ("C-c m e" . my/mu4e-dashboard))

(provide 'init-mu4e)
;;; init-mu4e.el ends here

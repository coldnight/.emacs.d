;;; init-mu4e --- initialize mu4e to receive and send email.
;;;
;;; Commentary:
;;;
;;; Code:
(defvar-local my/mu4e-sys-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")

(if (file-exists-p my/mu4e-sys-path)
    (progn
      (eval-when-compile
        (add-to-list 'load-path my/mu4e-sys-path))

      ;; happy flycheck
      (eval-when-compile
        (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
        (require 'use-package))

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
      (use-package mu4e
        :straight (:type built-in)
        :bind
        ("C-c c" . mu4e-org-store-and-capture)  ;; org-agenda
        ("C-c c" . mu4e-org-store-and-capture)  ;; org-agenda
        :commands mu4e
        :custom
        ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
        (mu4e-change-filenames-when-moving t)
        ;; Update mails every 2 minutes.
        (mu4e-update-interval 120)
        ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
        ;; Override in context switching for other type of mailboxes
        (mu4e-sent-messages-behavior 'delete)
        (message-kill-buffer-on-exit t)
        (mu4e-get-mail-command "/usr/local/bin/mbsync gmail")
        (mu4e-contexts
         `( ,(my-make-mu4e-context "main" "grayking.w@gmail.com" nil)))
        (message-send-mail-function 'smtpmail-send-it)
        (smtpmail-stream-type 'starttls)
        (starttls-use-gnutls t)
        ;; Personal info
        (user-full-name "Gray King")
        ( user-mail-address "grayking.w@gmail.com")
        ;; gmail setup
        (smtpmail-smtp-server "smtp.gmail.com")
        (smtpmail-smtp-service 587)
        (smtpmail-smtp-user "grayking.w@gmail.com"))

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

      (defun my/mu4e-dashboard()
        "Open mu4e dashboard."
        (interactive)
        (find-file (expand-file-name "mu4e.org" user-emacs-directory))
        (org-mode)
        (mu4e-dashboard-mode))

      (use-package mu4e-dashboard
        :straight (mu4e-dashboard :host github :repo "rougier/mu4e-dashboard")
        :bind
        ("C-c m e" . my/mu4e-dashboard))

      (use-package mu4e-alert
        :straight t
        :after mu4e
        :hook
        (after-init mu4e-alert-enable-notifications)
        :config
        (mu4e-alert-set-default-style 'notifier))))

(provide 'init-mu4e)
;;; init-mu4e.el ends here

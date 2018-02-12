;;; init-mu4e.el -- mu4e 配置
;;; Commentary:
;;;
;;;   config fo mu4e
;;;
;;; Code:
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)
(setq mu4e-index-update-in-backgroud t)

(setq mu4e-contexts
      `(
        ,(make-mu4e-context
           :name "Gray"
           :enter-func (lambda () (mu4e-message "Switch to the Gray context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Gray" (mu4e-message-field msg :maildir))))
           :vars '(( user-mail-address . secret-gmail-gray-account)
                   ( user-full-name . "Gray King")
                   ( smtpmail-auth-credentials .
                     (("smtp.gmail.com" 587 secret-gmail-cold-account nil)))
                   ( mu4e-drafts-folder . "/Gray/[Gmail].草稿")
                   ( mu4e-sent-folder  . "/Gray/[Gmail].已发邮件")
                   ( mu4e-trash-folder . "/Gray/[Gmail].垃圾邮件")
                   ( mu4e-maildir-shortcuts .
                     ( ("/Gray/INBOX"                . ?i)
                        ("/Gray/[Gmail].已发邮件"    . ?s)
                        ("/Gray/[Gmail].垃圾邮件"    . ?t)
                        ("/Gray/[Gmail].所有邮件"    . ?a)))
                   ( mu4e-compose-signature .
                     (concat
                      "Gray King\n"
                      "https://www.linuxzen.com.com\n")
                     )))

         ,(make-mu4e-context
           :name "Cold"
           :enter-func (lambda () (mu4e-message "Entering Cold context"))
           :leave-func (lambda () (mu4e-message "Leaving Private context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Cold" (mu4e-message-field msg :maildir))))
           :vars '(( user-mail-address . secret-gmail-cold-account)
                   ( user-full-name . "Gray King")
                   ( smtpmail-auth-credentials .
                     (("smtp.gmail.com" 587 secret-gmail-cold-account nil)))
                   ( mu4e-drafts-folder . "/Cold/[Gmail].草稿")
                   ( mu4e-sent-folder  . "/Cold/[Gmail].已发邮件")
                   ( mu4e-trash-folder . "/Cold/[Gmail].垃圾邮件")
                   ( mu4e-maildir-shortcuts .
                     (
                       ("/Cold/INBOX"              . ?i)
                       ("/Cold/[Gmail].已发邮件"    . ?s)
                       ("/Cold/[Gmail].垃圾邮件"    . ?t)
                       ("/Cold/[Gmail].所有邮件"    . ?a)
                       ("/Cold/CPyUG"      . ?c)
                       ))
                   ( mu4e-compose-signature .
                     (concat
                      "Gray King\n"
                      "https://www.linuxzen.com.com\n")
                     )))

         )
      )

(setq mu4e-maildir "~/Maildir")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/Gray/INBOX"               . ?i)
       ("/Gray/[Gmail].已发邮件"    . ?s)
       ("/Gray/[Gmail].垃圾邮件"    . ?t)
       ("/Gray/[Gmail].所有邮件"    . ?a)
       ))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

(require 'smtpmail)
;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(setq
   message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-auth-credentials "~/.authinfo"
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


;; Speeding up indexing
(setq
  mu4e-index-cleanup nil      ;; don't do a full cleanup check
  mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs


;; Alert
(mu4e-alert-set-default-style 'notifier)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(provide 'init-mu4e)
;; init-mu4e.el ends here

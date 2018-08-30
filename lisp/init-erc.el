;;; init-erc.el -- IRC
;;;
;;; Commentary:
;;;
;;; ERC 配置 IRC
;;;
;;; Code:
;; ERC
(require 'erc)
;; ERC SSL
(require 'tls)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                    "gnutls-cli --priority secure256"
                    "gnutls-cli --priority secure256 -p %p %h"))

; M-x start-irc
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))


(defun start-mozilla-irc ()
  "Connect to Mozilla-irc."
  (interactive)
  (erc-tls :server "irc.mozilla.org" :port 6697
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))

(provide 'init-erc)
;; init-erc.el ends here

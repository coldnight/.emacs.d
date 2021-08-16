;;; init-optional --- Some cool packages that I don't use every day.
;;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package")
  (require 'use-package))


;; ERC to chat via IRC

;; ;; enable TLS
;; (use-package tls
;;   :straight t
;;   :custom
;;   (tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
;;                     "gnutls-cli --priority secure256"
;;                     "gnutls-cli --priority secure256 -p %p %h")))

; M-x my/start-irc
(defun my/start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))


(defun my/start-mozilla-irc ()
  "Connect to Mozilla-irc."
  (interactive)
  (erc-tls :server "irc.mozilla.org" :port 6697
           :full-name secret-freenode-full-name
           :nick secret-freenode-nick
           :password secret-freenode-password))


(use-package erc
  :straight t
  :after tls
  :commands (my/start-irc my/start-mozilla-irc))

(use-package keypression
  :straight (:host github :repo "chuntaro/emacs-keypression" :branch "master")
  :custom
  ;; (keypression-use-child-frame t)
  ;; (keypression-frame-justify 'keypression-left-justified)
  (keypression-fade-out-delay 1.0)
  (keypression-cast-command-name t)
  (keypression-cast-command-name-format "%s  %s")
  (keypression-combine-same-keystrokes t)
  (keypression-font-face-attribute '(:width normal :height 200 :weight bold)))

(use-package quickrun
  :straight t)

(use-package neotree
  :straight (neotree :host github :repo "jaypei/emacs-neotree")
  :bind
  ("C-c s n" . neotree-toggle))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :init
  (setq yas-snippet-dirs
      '("~/.emacs.d/straight/build/yasnippet-snippets/snippets/"
        "~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package slime
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))

(provide 'init-optional)
;;; init-optional.el ends here

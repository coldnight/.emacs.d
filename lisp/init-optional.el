;;; init-optional --- Some cool packages that I don't use every day.
;;;
;;; Commentary:
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
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
  :commands keypression-mode
  :custom
  ;; (keypression-use-child-frame t)
  ;; (keypression-frame-justify 'keypression-left-justified)
  (keypression-fade-out-delay 1.0)
  (keypression-cast-command-name t)
  (keypression-cast-command-name-format "%s  %s")
  (keypression-combine-same-keystrokes t)
  (keypression-font-face-attribute '(:width normal :height 200 :weight bold)))

(use-package quickrun
  :straight t
  :commands quickrun)

(use-package neotree
  :straight (neotree :host github :repo "jaypei/emacs-neotree")
  :bind
  ("C-c s n" . neotree-toggle))

(use-package slime
  :straight t
  :commands slime
  :custom
  (inferior-lisp-program "sbcl"))

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :if (memq window-system '(x))
  :custom
  ;; posframe 中顯示更加友好，但是無法在 posframe 中查看候選
  (rime-show-candidate 'minibuffer)
  (rime-share-data-dir "/usr/share/rime-data")
  (rime-user-data-dir "~/.rime")
  (default-input-method "rime"))

(provide 'init-optional)
;;; init-optional.el ends here

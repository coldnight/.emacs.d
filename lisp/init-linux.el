;;; init-linux.el -- init for Linux
;;;
;;; Commentary:
;;;
;;; Configure for Linux only
;;;
;;; Code:
;; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 100 :weight 'normal)

;; Go
(setenv "GOPATH" "/home/wh/codes/go")

(when window-system
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "www-browser"))

(defun my/activate-terminal()
  (error "TODO"))
(provide 'init-linux)
;; init-darwin.el ends here

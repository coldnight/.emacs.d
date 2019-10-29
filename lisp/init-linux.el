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

;; Shell
(setq shell-file-name "/usr/bin/fish")

;; Go
(setenv "GOPATH" "/home/wh/codes/go")
(provide 'init-linux)
;; init-darwin.el ends here

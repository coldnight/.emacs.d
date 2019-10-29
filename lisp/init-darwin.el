;;; init-darwin.el -- init for macOS
;;;
;;; Commentary:
;;;
;;; Configure for macOS only
;;;
;;; Code:

;; Go
(setenv "GOPATH" "/Users/wh/codes/go")

;; Shell
(setq shell-file-name "/usr/local/bin/fish")

;; Font
(set-face-attribute 'default nil
                    :family "Victor Mono" :height 145 :weight 'normal)

;; Transparent
;;
;;   (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;   (set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 70))
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))

;; share clipboard between os
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Ligatures
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(provide 'init-darwin)
;; init-darwin.el ends here

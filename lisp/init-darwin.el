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
(setq usr-bin-path "/usr/local/bin/")

;; Font
(set-face-attribute 'default nil
                    :family "Victor Mono" :height 145 :weight 'normal)

;; Transparent
;;
;;   (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;   (set-frame-parameter (selected-frame) 'alpha <both>)
(defvar my-transparent-enabled t "Indicates if transparent enabled.")
(defun toggle-frame-transparent()
  "Toggle frame transparent when lose focus."
  (interactive)
  (cond (my-transparent-enabled
         (progn
           (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
           (add-to-list 'default-frame-alist '(alpha . (100 . 100)))))
        ((not my-transparent-enabled)
         (progn
           (set-frame-parameter (selected-frame) 'alpha '(100 . 90))
           (add-to-list 'default-frame-alist '(alpha . (100 . 90))))))
  (setq my-transparent-enabled (not my-transparent-enabled)))

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

(when window-system
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 208 48))

(provide 'init-darwin)
;; init-darwin.el ends here

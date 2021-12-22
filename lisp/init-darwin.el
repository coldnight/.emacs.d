;;; init-darwin.el -- init for macOS
;;;
;;; Commentary:
;;;
;;; Configure for macOS only
;;;
;;; Code:

;; Go
(setenv "GOPATH" (expand-file-name "~/codes/go"))

;; Shell
(setq usr-bin-path "/usr/local/bin/")

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

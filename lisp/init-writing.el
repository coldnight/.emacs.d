;;; init-writing --- The packages that help to writing correct and writing good.
;;;
;;; Commentary:
;;; Code:

;; happy flycheck
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package flycheck-grammarly
  :straight (:host github :repo "emacs-grammarly/flycheck-grammarly")
  :custom
  (flycheck-grammarly-check-time 0.8)
  :after (flycheck))

(provide 'init-writing)
;;; init-writing.el ends here

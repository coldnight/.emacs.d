;;; init-theme-nano -- Emacs look via nano-emacs.
;;;
;;; Commentary:
;;;
;;; Code:
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

(use-package nano-base-colors
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :after all-the-icons)

(use-package nano-faces
  :after nano-base-colors
  :init
  (require 'nano-faces)
  (nano-faces))

(use-package nano-modeline
  :after nano-faces)

(use-package nano-theme
  :after nano-faces
  :init
  (require 'nano-theme)
  (nano-theme))

(provide 'init-theme-nano)


;;; init-theme-nano.el ends here

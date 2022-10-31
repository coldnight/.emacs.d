;;; init-local-early --- A place to write local configurations.
;;;
;;; Commentary:
;;;
;;; Which means this file is ignored by .gitignore, any changes won't
;;; commit to the repository.
;;; Code:

(defgroup lo nil
  "Configurations of local."
  :group 'convenience)

(defcustom lo-frame-width
  204
  "Local frame width."
  :type '(integer))

(defcustom lo-frame-height
  58
  "Local frame height."
  :type '(integer))

(provide 'init-local-early)
;;; init-local-early.el ends here.

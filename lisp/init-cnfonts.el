;;; init-cnfonts.el -- cnfonts configuration
;;;
;;; Commentary:
;;;
;;;cnfonts configuration
;;;
;;; Code:
;;;
(require 'cnfonts)
(setq cnfonts--custom-set-fontnames
      '(
        ("Victor Mono")
        ("Hiragino Sans GB")
        ("HanaMinB")))

(setq cnfonts--custom-set-fontsizes
      '(
        (14  16.5 16.5)
        ))

(cnfonts-enable)


(provide 'init-cnfonts)

;;; init-cnfonts.el ends here

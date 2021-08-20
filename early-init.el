;;; early-init.el -- Emacs early configuration
;;;
;;; Commentary:
;;;
;;;   Emacs Early configuration.
;;;
;;; Code:
(setq package-enable-at-startup nil)

;; Optmization
;; Sources:
;;
;; - https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(defvar old-file-name-handler file-name-handler-alist)
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda nil
                  (setq gc-cons-threshold     16777216
                        gc-cons-percentage   0.1
                        file-name-handler-alist old-file-name-handler)))

;; Disable native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;; early-init.el ends here

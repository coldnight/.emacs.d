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
(setq gc-cons-threshold 32000000) ;; 32mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

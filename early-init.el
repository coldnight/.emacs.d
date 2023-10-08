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

;; Fix libgccjit according to: https://github.com/d12frosted/homebrew-emacs-plus/issues/323
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc@12/lib:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc@12/lib/gcc/12/gcc/x86_64-apple-darwin22/12")

;; ACK org-roam-v2
(setq org-roam-v2-ack t)

;; Fix "Too Many Files Open" on macOS.
(defun my/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; early-init.el ends here

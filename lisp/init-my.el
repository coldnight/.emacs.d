;;; init-my.el -- 自定义函数
;;;
;;; Commentary:
;;;
;;;    一些自定义函数。
;;;
;;; Code:
;;;

(defgroup my nil
  "Configurations of myself."
  :group 'convenience)

(defcustom my-url-http-proxy-service
  "127.0.0.1:1087"
  "HTTP proxy that apply to 'url-retrieve'."
  :type '(string))

(defun my/url-current-public-ip()
  "Return current public IPv4 address with url."
  (let ((r (url-retrieve-synchronously "https://api-ipv4.ip.sb/ip" t nil 10)))
    (with-current-buffer r
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (string-trim (buffer-string)))))

(defun my/url-proxy-on()
  "Enable HTTP proxy for url."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-url-http-proxy-service)
          ("https" . ,my-url-http-proxy-service)))
  (message "Proxy services set and public IP is: %s" (my/url-current-public-ip)))

(defun my/url-proxy-off()
  "Disable HTTP proxy for url."
  (interactive)
  (setq url-proxy-services nil)
  (message "Porxy services removed and public IP is: %s." (my/url-current-public-ip)))

(defun my/url-proxy-toggle()
  "Toggle HTTP proxy for url."
  (interactive)
  (if (eq url-proxy-services nil)
      (my/url-proxy-on)
    (my/url-proxy-off)))

;; make a temp buffer
(defun my/generate-buffer ()
  "Generate a tmp buffer."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(defun my/open-remark-org()
  "Open my own remark in 'org-mode'."
  (interactive)
  (find-file "~/Documents/org-modes/remark.org"))

(defun my/activate-terminal(args)
  (call-process-shell-command "open -a \"iTerm\"" nil nil nil)
  args)

;; Transparent only works on emacs-mac
;;
;;   (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;   (set-frame-parameter (selected-frame) 'alpha <both>)
(defvar my/transparent-enabled t "Indicates if transparent enabled.")
(defun my/toggle-frame-transparent()
  "Toggle frame transparent when lose focus."
  (interactive)
  (cond (my/transparent-enabled
         (progn
           (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
           (add-to-list 'default-frame-alist '(alpha . (100 . 100)))))
        ((not my/transparent-enabled)
         (progn
           (set-frame-parameter (selected-frame) 'alpha '(100 . 90))
           (add-to-list 'default-frame-alist '(alpha . (100 . 90))))))
  (setq my/transparent-enabled (not my/transparent-enabled)))

(provide 'init-my)
;;; init-my.el ends here

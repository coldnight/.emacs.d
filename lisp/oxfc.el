;;; oxfc.el --- create org-fc cards for English words by API of oxford.
;;; Commentary:
;;; Code:
(require 'url-vars)
(require 's)
(require 'json)

(defgroup oxfc nil
  "Configurations of oxfc.el."
  :group 'convenience)

(defcustom oxfc-oxford-appid
  ""
  "Application ID of Oxford Dictionaries."
  :type '(string))

(defcustom oxfc-oxford-appkey
  ""
  "Application Key of Oxford Dictionaries."
  :type '(string))

(defcustom oxfc-oxford-api-base-url
  "https://od-api.oxforddictionaries.com/api/v2"
  "Consistent part of API requests of Oxford Dictionaries."
  :type '(string))

(defcustom oxfc-oxford-language
  "en-gb"
  "Language that used for Oxford Dictionaries."
  :type '(string))

(defun oxfc--fetch-word (word &optional callback)
  "Fetch WORD's defination in Oxford Dictionaries over its API.
Apply CALLBACK if it was provided."

  (let ((url (s-concat oxfc-oxford-api-base-url "/entries/" oxfc-oxford-language "/" word))
        (url-request-extra-headers `(
                                     ("app_id" . ,oxfc-oxford-appid)
                                     ("app_key" . ,oxfc-oxford-appkey)))
        (buffer (current-buffer)))
    (url-retrieve url (lambda (status l-buffer l-callback)
                        (oxfc--on-fetch-word status l-buffer l-callback)) `(,buffer ,callback))))


(defun oxfc--on-fetch-word(status buffer &optional callback)
  "Callback of fetch Oxford Dictionaries API with STATUS.
Then write result to BUFFER.  Apply CALLBACK when write is done."
  (goto-char (point-min))
  (re-search-forward "^$")
  (let* ((regionp (buffer-substring (point) (point-max)))
         (json (json-parse-string regionp
                                  :object-type 'hash-table
                                  :array-type 'list))
         (id (gethash "id" json))
         (results (gethash "results" json)))

    (with-current-buffer buffer
      (insert (format "** %s\n" id))
      (insert "*** Back\n")
      (oxfc--write-results results)
      (if callback
          (funcall callback)))))

(defun oxfc--write-results (results)
  "Write RESULTS to the current buffer."
  (dolist (ret results)
    (dolist (lex-entry (gethash "lexicalEntries" ret))
      (oxfc--write-lexical-entry lex-entry))))


(defun oxfc--write-lexical-entry(lex-entry)
  "Write LEX-ENTRY to the current buffer."
  (insert (format "=%s=" (gethash "text" (gethash "lexicalCategory" lex-entry))))
  (dolist (entry (gethash "entries" lex-entry))
    (oxfc--write-entry entry)))


(defun oxfc--write-entry(entry)
  "Write ENTRY to the current buffer."
  (let ((pronuns (gethash "pronunciations" entry))
        (senses (gethash "senses" entry)))
    (if (listp pronuns)
        (progn
          (dolist (p pronuns)
            (oxfc--write-entry-pronun p)
            (insert "\n"))))
    (if (listp senses)
        (progn
          (dolist (s senses)
            (oxfc--write-entry-sense s))))))


(defun oxfc--write-entry-pronun(p)
  "Write P to the current buffer."
  (insert (format " [[%s][/%s/]] "
                  (gethash "audioFile" p)
                  (gethash "phoneticSpelling" p))))

(defun oxfc--write-entry-sense (sense)
  "Write SENSE to the current buffer."
  (let ((defs (gethash "definitions" sense))
        (synonyms (gethash "synonyms" sense))
        (examples (gethash "examples" sense)))
    (insert (format "- %s\n" (apply 's-concat defs)))
    (if (length> synonyms 0)
        (insert (format "\n  Synonyms: %s"
                        (s-join "/" (mapcar (lambda (x) (gethash "text" x))
                                            synonyms)))))
    (if (length> examples 0)
        (progn
          (insert "\n\n  Examples: \n")
          (dolist (example examples)
            (insert (format "  + %s\n" (gethash "text" example))))))))


(defun oxfc-lookup-word(word)
  "Lookup defination of WORD by Oxford Dicttionaries."
  (interactive "sEnter word: ")
  (oxfc--fetch-word word))

(defun oxfc-lookup-word-fc(word)
  "Lookup defination of WORD by Oxford Dicttionaries and make it to a org-fc card."
  (interactive "sEnter word: ")
  (oxfc--fetch-word word (lambda ()
                           (re-search-backward "^\\*\\* ")
                           (org-fc-type-normal-init))))

(provide 'oxfc)
;;; oxfc.el ends here

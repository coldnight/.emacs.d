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

(defun oxfc--fetch-word (word)
  "Fetch WORD's defination in Oxford Dictionaries over its API."

  (let (
        (url (s-concat oxfc-oxford-api-base-url "/entries/" oxfc-oxford-language "/" word))
        (url-request-extra-headers `(
                                     ("app_id" . ,oxfc-oxford-appid)
                                     ("app_key" . ,oxfc-oxford-appkey))))
    (url-retrieve url (lambda (status)
                        (oxfc--on-fetch-word status)))))


(defun oxfc--on-fetch-word(status)
  "Callback of fetch Oxford Dictionaries API with STATUS."
  (goto-char (point-min))
  (re-search-forward "^$")
  (let* ((regionp (buffer-substring (point) (point-max)))
         (json (json-parse-string regionp
                                  :object-type 'hash-table
                                  :array-type 'list))
         (id (gethash "id" json))
         (results (gethash "results" json)))

    (with-temp-buffer
      (insert (format "** %s\n" id))
      (oxfc--write-results results)
      (kill-append (buffer-string) nil))))


(defun oxfc--write-results (results)
  "Write RESULTS to the current buffer."
  (dolist (ret results)
    (dolist (lex-entry (gethash "lexicalEntries" ret))
      (oxfc--write-lexical-entry lex-entry))))


(defun oxfc--write-lexical-entry(lex-entry)
  "Write LEX-ENTRY to the current buffer."
  (insert (format "*** %s\n" (gethash "text" (gethash "lexicalCategory" lex-entry))))
  (dolist (entry (gethash "entries" lex-entry))
    (oxfc--write-entry entry)))


(defun oxfc--write-entry(entry)
  "Write ENTRY to the current buffer."
  (let ((pronuns (gethash "pronunciations" entry))
        (senses (gethash "senses" entry)))
    (if (listp pronuns)
        (progn
          (insert "Pronunciations ")
          (dolist (p pronuns)
            (oxfc--write-entry-pronun p)
            (insert "\n"))))
    (if (listp senses)
        (progn
          (dolist (s senses)
            (oxfc--write-entry-sense s))))))


(defun oxfc--write-entry-pronun(p)
  "Write P to the current buffer."
  (insert (format " [[%s][%s /%s/]] "
                  (gethash "audioFile" p)
                  (gethash "phoneticNotation" p)
                  (gethash "phoneticSpelling" p))))

(defun oxfc--write-entry-sense (sense)
  "Write SENSE to the current buffer."
  (let ((defs (gethash "definitions" sense))
        (synonyms (gethash "synonyms" sense))
        (examples (gethash "examples" sense)))
    (insert (format "- %s\n" (apply 's-concat defs)))
    (insert (format "  Synonyms: %s"
                    (s-join "/" (mapcar (lambda (x) (gethash "text" x)) synonyms))))
    (insert "\n  Examples: \n")
    (dolist (example examples)
      (insert (format "  + %s\n" (gethash "text" example))))))


(defun oxfc-lookup-word(word)
  "Lookup defination of WORD by Oxford Dicttionaries."
  (interactive "sEnter word: ")
  (oxfc--fetch-word word))

(oxfc-lookup-word "word")
;;; oxfc.el ends here

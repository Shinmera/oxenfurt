#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defparameter *api* "https://od-api.oxforddictionaries.com/api/v1")
(defvar *app-id*)
(defvar *app-key*)

(define-condition api-call-failed (error)
  ((url :initarg :url :reader url)
   (result :initarg :result :reader result)
   (body :initarg :body :reader body))
  (:report (lambda (c s) (format s "Oxford API replied with code ~a on ~a:~%  ~a"
                                 (result c) (url c) (body c)))))

(defun %request (url parameters key id)
  (declare (ignore url parameters key id))
  (error "No HTTP client backend is loaded."))

(defun request (base parts &key (key *app-key*) (id *app-id*) parameters)
  (let ((url (format NIL "~a~a~{/~a~}" *api* base
                     (loop for part in parts
                           when part
                           collect (url-encode part)))))
    (multiple-value-bind (stream result)
        (%request url (remove NIL parameters :key #'cdr) key id)
      (unwind-protect
           (cond ((/= 200 result)
                  (error 'api-call-failed :url url :result result
                                          :body (alexandria:read-stream-content-into-string stream)))
                 (T
                  (let* ((data (yason:parse stream))
                         (result (gethash "result" data)))
                    (or (gethash "results" data)
                        (when result (list result))))))
        (close stream)))))

(defun serialize-filters (filters)
  (when filters
    (format NIL "~{~/ORG.SHIRAKUMO.OXENFURT::%FORMAT-PARAM/=~{~/ORG.SHIRAKUMO.OXENFURT::%FORMAT-SPECIAL/~^,~}~^;~}"
            filters)))

(defun serialize-features (features)
  (when features
    (format NIL "~{~/ORG.SHIRAKUMO.OXENFURT::%FORMAT-PARAM/=~/ORG.SHIRAKUMO.OXENFURT::%FORMAT-SPECIAL/~^;~}"
            features)))

(defun serialize-sort (sort)
  (when sort
    (format NIL "~{~a~/ORG.SHIRAKUMO.OXENFURT::%FORMAT-PARAM/~^,~}"
            (loop for sort in (if (consp sort) sort (list sort))
                  nconc (destructuring-bind (sort . dir) (if (consp sort) sort (cons sort :asc))
                          (list (ecase dir (:asc "") ((:dsc :desc) "-")) sort))))))

(defun normalize-frequency (frequency)
  (etypecase frequency
    ((or null (eql T)) (cons NIL NIL))
    (integer (cons (princ-to-string frequency)
                   (princ-to-string frequency)))
    (cons (cons (when (car frequency) (princ-to-string (car frequency)))
                (when (cdr frequency) (princ-to-string (cdr frequency)))))))

(defun list-languages (&key source-lang target-lang)
  (into 'language-dataset (request "/languages" (list source-lang target-lang))))

(defun list-filters (&optional endpoint)
  (if endpoint
      (let ((endpoint (string-downcase endpoint)))
        (mapcar #'param->key (-> (request "/filters" (list endpoint)) endpoint)))
      (let ((result (request "/filters" ())))
        (list :entries (mapcar #'param->key (-> result "entries"))
              :inflections (mapcar #'param->key (-> result "inflections"))
              :translations (mapcar #'param->key (-> result "translations"))
              :wordlist (mapcar #'param->key (-> result "wordlist"))))))

(defun list-lexical-categories (&key (language "en"))
  (loop for key being the hash-keys of (request "/lexicalcategories" (list language))
        collect (special->key key)))

(defun list-registers (&key (source-lang "en") target-lang)
  (loop for key being the hash-keys of (request "/registers" (list source-lang target-lang))
        collect (special->key (url-decode key))))

(defun list-domains (&key (source-lang "en") target-lang)
  (loop for key being the hash-keys of (request "/domains" (list source-lang target-lang))
        collect (special->key (url-decode key))))

(defun list-regions (&key (source-lang "en"))
  (let ((table (request "/regions" (list source-lang))))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          collect (param->key key) collect val)))

(defun list-grammatical-features (&key (source-lang "en"))
  (let ((table (request "/grammaticalFeatures" (list source-lang))))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          collect (special->key key)
          collect (loop for key being the hash-keys of val collect (special->key key)))))

(defun inflections (word &key (source-lang "en") filters)
  (into 'word (request "/inflections" (list source-lang word (serialize-filters filters)))))

(defun find-word (word &key (source-lang "en") filters synonyms antonyms target-lang sentences)
  (when (and filters (or synonyms antonyms target-lang sentences))
    (error "Filters cannot be applied in combination with synonyms, antonyms, translations, or sentences."))
  (when (and (or synonyms antonyms) (or filters target-lang sentences))
    (error "Synonyms or antonyms cannot be fetched in combination with filters, translations, or sentences."))
  (when (and target-lang (or filters synonyms antonyms sentences))
    (error "Translations cannot be fetched in combination with synonyms, antonyms, filters, or sentences."))
  (when (and sentences (or filters synonyms antonyms target-lang))
    (error "Sentences cannot be fetched in combination with synonyms, antonyms, filters, or translations."))
  (into 'word (first (request "/entries" (list source-lang word
                                               (serialize-filters filters)
                                               (cond ((and synonyms antonyms) "synonyms;antonyms")
                                                     (synonyms "synonyms")
                                                     (antonyms "antonyms"))
                                               (when target-lang (format NIL "translations=~a" target-lang))
                                               (when sentences "sentences"))))))

(defun search-words (query &key (source-lang "en") prefix regions target-lang (offset 0) (limit 5000))
  (into 'match (request "/search" (list source-lang (when target-lang (format NIL "translations=~a" target-lang)))
                        :parameters `(("q" . ,query)
                                      ("prefix" . ,(bool->string prefix))
                                      ("regions" . ,(when regions (format NIL "~{~a~^~}" regions)))
                                      ("offset" . ,(princ-to-string offset))
                                      ("limit" . ,(princ-to-string limit))))))

(defun list-words (&key (source-lang "en") filters exclude exclude-senses exclude-prime-senses word-length prefix exact (offset 0) (limit 5000))
  (into 'word (request "/wordlist" (list source-lang (serialize-filters filters))
                       :parameters `(("exclude" . ,(serialize-filters exclude))
                                     ("exclude_senses" . ,(serialize-filters exclude-senses))
                                     ("exclude_prime_senses" . ,(serialize-filters exclude-prime-senses))
                                     ("word_length" . ,(etypecase word-length
                                                         ((or null (eql T)))
                                                         (integer (princ-to-string word-length))
                                                         (cons (format NIL "~@[>~a~]~:[~;,~]~@[<~a~]"
                                                                       (car word-length)
                                                                       (and (car word-length) (cdr word-length))
                                                                       (cdr word-length)))))
                                     ("prefix" . ,prefix)
                                     ("exact" . ,(bool->string exact))
                                     ("offset" . ,(princ-to-string offset))
                                     ("limit" . ,(princ-to-string limit))))))

(defun word-frequency (lemma &key (source-lang "en") (corpus "nmc") wordform true-case lexical-category grammatical-features sort frequency normalized-frequency offset limit)
  (destructuring-bind (min-frequency . max-frequency) (normalize-frequency frequency)
    (destructuring-bind (min-norm-frequency . max-norm-frequency) (normalize-frequency normalized-frequency)
      (into 'frequency (request (if (listp lemma) "/stats/frequency/words" "/stats/frequency/word")
                                (list source-lang "")
                                :parameters `(("corpus" . ,corpus)
                                              ("wordform" . ,wordform)
                                              ("trueCase" . ,true-case)
                                              ("lemma" . ,(if (listp lemma) (format NIL "~{~a~^,~}" lemma) lemma))
                                              ("lexicalCategory" . ,(when lexical-category (key->special lexical-category)))
                                              ("grammaticalFeatures" . ,(serialize-features grammatical-features))
                                              ("sort" . ,(serialize-sort sort))
                                              ("minFrequency" . ,min-frequency)
                                              ("maxFrequency" . ,max-frequency)
                                              ("minNormalizedFrequency" . ,min-norm-frequency)
                                              ("maxNormalizedFrequency" . ,max-norm-frequency)
                                              ("offset" . ,(when offset (princ-to-string offset)))
                                              ("limit" . ,(when limit (princ-to-string limit)))))))))

(defun ngram-frequency (tokens &key (source-lang "en") (corpus "nmc") contains punctuation (format :oup) frequency document-frequency (offset 0) (limit 100))
  (destructuring-bind (min-frequency . max-frequency) (normalize-frequency frequency)
    (destructuring-bind (min-doc-frequency . max-doc-frequency) (normalize-frequency document-frequency)
      (into 'frequency (request "/stats/frequency/ngrams" (list source-lang corpus
                                                                (princ-to-string
                                                                 (if (consp (first tokens))
                                                                     (length (first tokens))
                                                                     (length tokens)))
                                                                "")
                                :parameters `(("tokens" . ,(if (consp (first tokens))
                                                               (format NIL "~{~{~a~^ ~}~^,~}" tokens)
                                                               (format NIL "~{~a~^ ~}" tokens)))
                                              ("contains" . ,(if (listp contains)
                                                                 (format NIL "~{~a~^,~}" contains)
                                                                 contains))
                                              ("punctuation" . ,(bool->string punctuation))
                                              ("format" . ,(key->param format))
                                              ("minFrequency" . ,min-frequency)
                                              ("maxFrequency" . ,max-frequency)
                                              ("minDocumentFrequency" . ,min-doc-frequency)
                                              ("maxDocumentFrequency" . ,max-doc-frequency)
                                              ("offset" . ,(princ-to-string offset))
                                              ("limit" . ,(princ-to-string limit))))))))

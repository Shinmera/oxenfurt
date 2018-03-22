#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defvar *api* "https://od-api.oxforddictionaries.com/api/v1")
(defvar *app-id*)
(defvar *app-key*)

(define-condition api-call-failed (error)
  ((url :initarg :url :reader url)
   (result :initarg :result :reader result)
   (body :initarg :body :reader body))
  (:report (lambda (c s) (format s "Oxford API replied with code ~a on ~a:~%  ~a"
                                 (result c) (url c) (body c)))))

(defun request (base parts &key (key *app-key*) (id *app-id*))
  (let ((url (format NIL "~a~a~{/~a~}" *api* base
                     (loop for part in parts
                           when part
                           collect (url-encode part)))))
    (multiple-value-bind (stream result)
        (drakma:http-request url
                             :accept "application/json"
                             :additional-headers `(("app_key" . ,key) ("app_id" . ,id))
                             :external-format-in :utf-8
                             :external-format-out :utf-8
                             :want-stream T)
      (unwind-protect
           (cond ((/= 200 result)
                  (error 'api-call-failed :url url :result result
                                          :body (alexandria:read-stream-content-into-string stream)))
                 (T
                  (gethash "results" (yason:parse stream))))
        (close stream)))))

(defun -> (result &rest tree)
  (if (and result tree)
      (let ((key (car tree)))
        (apply #'->
               (etypecase key
                 (integer (nth key result))
                 (string (gethash key result)))
               (cdr tree)))
      result))

(defun param->key (param)
  (intern (with-output-to-string (out)
            (loop for c across param
                  do (when (upper-case-p c)
                       (write-char #\- out))
                     (write-char (char-upcase c) out)))
          "KEYWORD"))

(defun key->param (key)
  (let ((key (string key)))
    (with-output-to-string (out)
      (loop for i from 0 below (length key)
            for c = (aref key i)
            do (cond ((char= c #\-)
                      (incf i)
                      (write-char (aref key i) out))
                     (T
                      (write-char (char-downcase c) out)))))))

(defun url-encode (thing &optional (external-format :utf-8))
  (with-output-to-string (out)
    (loop for octet across (babel:string-to-octets thing :encoding external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (T (format out "%~2,'0x" (char-code char)))))))

(defun url-decode (string &optional (external-format :utf-8))
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for char = (aref string i)
          do (case char
               (#\% (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) out)
                (incf i 2))
               (#\+ (vector-push (char-code #\Space) out))
               (T (vector-push (char-code char) out)))
          finally (return (babel:octets-to-string out :encoding external-format)))))

(defclass language-dataset ()
  ((region :initarg :region :accessor region)
   (source :initarg :source :accessor source)
   (language :initarg :language :accessor language)
   (target :initarg :target :accessor target)))

(defmethod print-object ((dataset language-dataset) stream)
  (print-unreadable-object (dataset stream :type T)
    (format stream "~s ~a~@[ (~a)~]~@[ <=> ~a~]"
            (source dataset) (language dataset) (region dataset) (target dataset))))

(defmethod bilingual-p ((dataset language-dataset))
  (not (null (target dataset))))

(defun languages (&key source-lang target-lang)
  (loop for result in (request "/languages" (list source-lang target-lang))
        collect (make-instance 'language-dataset
                               :region (-> result "region")
                               :source (-> result "source")
                               :language (-> result "sourceLanguage" "id")
                               :target (-> result "targetLanguage" "id"))))

(defun filters (&key endpoint)
  (if endpoint
      (mapcar #'param->key (-> (request "/filters" (list endpoint)) endpoint))
      (let ((result (request "/filters" (list endpoint))))
        (list :entries (mapcar #'param->key (-> result "entries"))
              :inflections (mapcar #'param->key (-> result "inflections"))
              :translations (mapcar #'param->key (-> result "translations"))
              :wordlist (mapcar #'param->key (-> result "wordlist"))))))

;; FIXME: Translating the categories, registers, etc into keywords
;;        requires a map as they cannot be bijectively mapped without
;;        making the keyword look unnatural.
;;
;;        We should keep the verbatim strings or even tables ourselves
;;        to bypass this. The tables should be automatically loaded
;;        using these API functions when first needed.
(defun lexical-categories (&key (language "en"))
  (loop for key being the hash-keys of (request "/lexicalcategories" (list language))
        collect key))

(defun registers (&key (source-lang "en") target-lang)
  (loop for key being the hash-keys of (request "/registers" (list source-lang target-lang))
        collect (url-decode key)))

(defun domains (&key (source-lang "en") target-lang)
  (loop for key being the hash-keys of (request "/domains" (list source-lang target-lang))
        collect (url-decode key)))

(defun regions (&key (source-lang "en"))
  (let ((table (request "/regions" (list source-lang))))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          collect (param->key key) collect val)))

(defun grammatical-features (&key (source-lang "en"))
  (let ((table (request "/grammaticalFeatures" (list source-lang))))
    (loop for key being the hash-keys of table
          for val being the hash-values of table
          collect key collect (loop for key being the hash-keys of val collect key))))

(defun inflections (word &key (source-lang "en") filters))

(defun entries (word &key (source-lang "en") filters synonyms antonyms))

(defun translations (word target-lang &key (source-lang "en")))

(defun search-entries (query &key (source-lang "en") prefix regions translations (offset 0) (limit 5000)))

(defun wordlist (&key filters exclude exclude-senses exclude-prime-senses word-length prefix exact (offset 0) (limit 5000)))

(defun sentences (word &key (source-lang "en")))

(defun frequency (lemma &key (source-lang "en") (corpus "nmc") wordform true-case lexical-category grammatical-features sort frequency normalized-frequency (offset 0) (limit 100)))

(defun ngram-frequency (tokens &key (source-lang "en") (corpus "nmc") contains punctuation (format "oup") frequency document-frequency (offset 0) (limit 100)))

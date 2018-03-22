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
                           collect (drakma:url-encode part :utf-8)))))
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

(defun languages (&key source-lang target-lang)
  (request "/languages" (list source-lang target-lang)))

(defun filters (&key endpoint))

(defun lexical-categories (&key (language "en")))

(defun registers (&key (source-lang "en") target-lang))

(defun domains (&key (source-lang "en") target-lang))

(defun regions (&key (source-lang "en")))

(defun grammatical-features (&key (source-lang "en")))

(defun inflections (word &key (source-lang "en") filters))

(defun entries (word &key (source-lang "en") filters synonyms antonyms))

(defun translations (word target-lang &key (source-lang "en")))

(defun search-entries (query &key (source-lang "en") prefix regions translations (offset 0) (limit 5000)))

(defun wordlist (&key filters exclude exclude-senses exclude-prime-senses word-length prefix exact (offset 0) (limit 5000)))

(defun sentences (word &key (source-lang "en")))

(defun frequency (lemma &key (source-lang "en") (corpus "nmc") wordform true-case lexical-category grammatical-features sort frequency normalized-frequency (offset 0) (limit 100)))

(defun ngram-frequency (tokens &key (source-lang "en") (corpus "nmc") contains punctuation (format "oup") frequency document-frequency (offset 0) (limit 100)))

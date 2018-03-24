#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defun parameters->string (params)
  (with-output-to-string (out)
    (loop for cons on params
          for param = (car cons)
          do (format out "~a=~a" (url-encode (car param)) (url-encode (cdr param)))
             (when (cdr cons) (format out "&")))))

(defun %request (url parameters key id)
  (let ((url (quri:merge-uris (quri:make-uri :query (parameters->string parameters)) (quri:uri url))))
    (dex:get url :headers `(("Accept" . "application/json")
                            ("Accept-Charset" . "UTF-8")
                            ("app_key" . ,key)
                            ("app_id" . ,id))
                 :want-stream T)))

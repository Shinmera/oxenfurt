#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defvar *keyword-table* (make-hash-table :test 'eq))

(defun -> (result &rest tree)
  (if (and result tree)
      (let ((key (car tree)))
        (apply #'->
               (etypecase key
                 (integer (nth key result))
                 (string (gethash key result)))
               (cdr tree)))
      result))

(defun bool->string (bool)
  (if bool "true" "false"))

(defun param->key (param)
  (when param
    (intern (with-output-to-string (out)
              (loop for c across param
                    do (when (upper-case-p c)
                         (write-char #\- out))
                       (write-char (char-upcase c) out)))
            "KEYWORD")))

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

(defun special->key (special)
  (when special
    (let ((key (intern (with-output-to-string (out)
                         (loop for c across special
                               do (case c
                                    ((#\_ #\Space) (write-char #\- out))
                                    ((#\'))
                                    (T (write-char (char-upcase c) out)))))
                       "KEYWORD")))
      (setf (gethash key *keyword-table*) special)
      key)))

(defun key->special (key)
  (or (gethash key *keyword-table*)
      (with-output-to-string (out)
        (loop with upcase = T
              for c across (string key)
              do (cond ((eql c #\-)
                        (setf upcase T)
                        (write-char #\_ out))
                       (upcase
                        (setf upcase NIL)
                        (write-char (char-upcase c) out))
                       (T
                        (write-char (char-downcase c) out)))))))

(defun %format-special (s a cp at)
  (declare (ignore cp at))
  (write-string (etypecase a
                  (keyword (key->special a))
                  (string a))
                s))

(defun %format-param (s a cp at)
  (declare (ignore cp at))
  (write-string (etypecase a
                  (keyword (key->param a))
                  (string a))
                s))

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

(defmacro define-unprintable-printer (class format-string &rest format-args)
  `(defmethod print-object ((,class ,class) stream)
     (print-unreadable-object (,class stream :type T)
       (format stream ,format-string ,@format-args))))

(defmacro with-result-instance (class results &body initargs)
  (let ((result (gensym "RESULT"))
        (name (gensym "NAME")))
    `(let ((,result ,results))
       (flet ((=> (&rest ,name)
                (apply #'-> ,result ,name)))
         (make-instance ,class ,@initargs)))))

(defgeneric into (type object))

(defmacro define-converter (class &body body)
  (let ((object (gensym "OBJECT")))
    `(defmethod into ((,(gensym "TYPE") (eql ',class)) (,object hash-table))
       (with-result-instance ',class ,object
         ,@body))))

(defmacro define-oxenfurt-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses
     ,(loop for slot in slots
            collect (if (listp slot)
                        slot
                        (list slot :initarg (intern (string slot) "KEYWORD") :reader slot)))
     ,@options))

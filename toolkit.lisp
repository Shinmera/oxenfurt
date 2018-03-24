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

(defun class-direct-slots (class)
  ()
  #+abcl      (mop:class-direct-slots class)
  #+allegro   (mop:class-direct-slots class)
  #+clisp     (clos:class-direct-slots class)
  #+clozure   (ccl:class-direct-slots class)
  #+cmu       (clos-mop:class-direct-slots class)
  #+ecl       (clos:class-direct-slots class)
  #+lispworks (clos:class-direct-slots class)
  #+mcl       (ccl:class-direct-slots class)
  #+sbcl      (sb-mop:class-direct-slots class)
  #+scl       (clos:class-direct-slots class))

(defun slot-definition-name (slot)
  ()
  #+abcl      (mop:slot-definition-name slot)
  #+allegro   (mop:slot-definition-name slot)
  #+clisp     (clos:slot-definition-name slot)
  #+clozure   (ccl:slot-definition-name slot)
  #+cmu       (clos-mop:slot-definition-name slot)
  #+ecl       (clos:slot-definition-name slot)
  #+lispworks (clos:slot-definition-name slot)
  #+mcl       (ccl:slot-definition-name slot)
  #+sbcl      (sb-mop:slot-definition-name slot)
  #+scl       (clos:slot-definition-name slot))

(defmethod describe-tree ((object standard-object) &optional (indent 0))
  (loop for slot in (class-direct-slots (class-of object))
        for name = (slot-definition-name slot)
        for value = (slot-value object name)
        do (when value
             (format T "~&~v{ ~}~a~vt" indent 0 name (+ indent 20))
             (typecase value
               (standard-object
                (format T "[~a]" (type-of object))
                (describe-tree value (+ indent 1)))
               (cons
                (typecase (first value)
                  (keyword
                   (format T "([KEYWORD] ~a)" (length value))
                   (dolist (item value)
                     (format T "~&~v{ ~}~s" (+ indent 2) 0 item)))
                  (string
                   (format T "([STRING] ~a)" (length value))
                   (dolist (item value)
                     (format T "~&~v{ ~}~s" (+ indent 2) 0 item)))
                  (T
                   (format T "([~a] ~a)" (type-of (first value)) (length value))
                   (dolist (item value)
                     (describe-tree item (+ indent 2))
                     (format T "~&~v{ ~}--" (+ indent 2) 0)))))
               (T (format T "~s" value)))))
  object)

#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:oxenfurt
  (:nicknames #:org.shirakumo.oxenfurt)
  (:use #:cl)
  ;; api.lisp
  (:export
   #:*api*
   #:*app-id*
   #:*app-key*
   #:languages
   #:filters
   #:lexical-categories
   #:registers
   #:domains
   #:regions
   #:grammatical-features
   #:inflections
   #:entries
   #:translations
   #:search-entries
   #:wordlist
   #:sentences
   #:frequency
   #:ngram-frequency))

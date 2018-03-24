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
   #:api-call-failed
   #:url
   #:result
   #:body
   #:list-languages
   #:list-filters
   #:list-lexical-categories
   #:list-registers
   #:list-domains
   #:list-regions
   #:list-grammatical-features
   #:inflections
   #:find-word
   #:search-words
   #:list-words
   #:word-frequency
   #:ngram-frequency)
  ;; objects.lisp
  (:export
   #:antonym
   #:cross-reference
   #:derivative
   #:entry
   #:example
   #:frequency
   #:language-dataset
   #:lexical-entry
   #:match
   #:note
   #:pronunciation
   #:sense
   #:sentence
   #:synonym
   #:translation
   #:variant-form
   #:word
   ;; Slots
   #:antonyms
   #:audio-file
   #:bilingual-p
   #:components
   #:cross-reference-markers
   #:cross-references
   #:definitions
   #:derivative-of
   #:derivatives
   #:dialects
   #:domains
   #:entries
   #:etymologies
   #:examples
   #:first-mention
   #:frequency
   #:grammatical-features
   #:homograph-number
   #:id
   #:inflection-id
   #:inflection-of
   #:kind
   #:language
   #:lemma
   #:lexical-category
   #:lexical-entries
   #:match-string
   #:match-type
   #:normalized-frequency
   #:normalized-lemma
   #:notes
   #:phonetic-notation
   #:phonetic-spelling
   #:pronunciations
   #:region
   #:regions
   #:registers
   #:score
   #:sense-ids
   #:senses
   #:sentences
   #:source
   #:subsenses
   #:synonyms
   #:target-lang
   #:text
   #:tokens
   #:translations
   #:true-case
   #:variant-forms
   #:word
   #:wordform)
  ;; toolkit.lisp
  (:export
   #:describe-tree))

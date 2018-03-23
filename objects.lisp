#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

(defmethod into (class (list list))
  (loop for entry in list collect (into class entry)))

(defmethod into ((class (eql 'grammatical-feature)) (list list))
  (loop for feature in list
        collect (special->key (-> feature "type"))
        collect (special->key (-> feature "text"))))

(defmethod into ((class (eql 'grammatical-feature)) (object hash-table))
  (loop for key being the hash-keys of object
        for value being the hash-values of object
        collect (special->key key)
        collect (special->key value)))

(define-oxenfurt-class language-dataset ()
  (region
   source
   language
   target-lang))

(defmethod bilingual-p ((dataset language-dataset))
  (not (null (target-lang dataset))))

(define-unprintable-printer language-dataset
  "~s ~a~@[ (~a)~]~@[ <=> ~a~]"
  (source language-dataset) (language language-dataset) (region language-dataset) (target-lang language-dataset))

(define-converter 'language-dataset
  :region (param->key (=> "region"))
  :source (=> "source")
  :language (param->key (=> "sourceLanguage" "id"))
  :target-lang (param->key (=> "targetLanguage" "id")))

(define-oxenfurt-class lexical-entry ()
  (grammatical-features
   inflection-of
   language
   lexical-category
   text
   derivative-of
   derivatives
   entries
   notes
   pronunciations
   variant-forms
   sentences))

(define-unprintable-printer lexical-entry
  "~s ~a" (text lexical-entry) (language lexical-entry))

(define-converter lexical-entry
  :grammatical-features (into 'grammatical-feature (=> "grammaticalFeatures"))
  :inflection-of (loop for inflection in (=> "inflectionOf")
                       collect (-> inflection "id"))
  :language (param->key (=> "language"))
  :lexical-category (special->key (=> "lexicalCategory"))
  :text (=> "text")
  :derivative-of (into 'derivative (=> "derivativeOf"))
  :derivatives (into 'derivative (=> "derivatives"))
  :entries (into 'entry (=> "entries"))
  :notes (into 'note (=> "notes"))
  :pronunciations (into 'pronunciation (=> "pronunciations"))
  :variant-forms (into 'variant-form (=> "variantForms"))
  :sentences (into 'sentence (=> "sentences")))

(define-oxenfurt-class word ()
  (id
   language
   lexical-entries
   pronunciations
   word))

(define-unprintable-printer word
  "~s ~a" (id word) (language word))

(define-converter word
  :id (=> "id")
  :language (param->key (=> "language"))
  :word (=> "word")
  :pronunciations (into 'pronunciation (=> "pronunciations"))
  :lexical-entries (into 'lexical-entry (=> "lexicalEntries")))

(define-oxenfurt-class entry ()
  (etymologies
   grammatical-features
   homograph-number
   notes
   pronunciations
   senses
   variant-forms))

(define-converter entry
  :etymologies (=> "etymologies")
  :grammatical-features (into 'grammatical-feature (=> "grammaticalFeatures"))
  :homograph-number (parse-integer (=> "homographNumber"))
  :notes (into 'note (=> "notes"))
  :pronunciations (into 'pronunciation (=> "pronunciations"))
  :senses (into 'sense (=> "senses"))
  :variant-forms (into 'variant-form (=> "variantForms")))

(define-oxenfurt-class pronunciation ()
  (audio-file
   dialects
   regions
   phonetic-notation
   phonetic-spelling))

(define-unprintable-printer pronunciation
  "~s (~a)~{ ~a~}" (phonetic-spelling pronunciation) (phonetic-notation pronunciation) (dialects pronunciation))

(define-converter pronunciation
  :audio-file (=> "audio-file")
  :dialects (=> "dialects")
  :regions (mapcar #'param->key (=> "regions"))
  :dialects (=> "dialects")
  :phonetic-notation (=> "phoneticNotation")
  :phonetic-spelling (=> "phoneticSpelling"))

(define-oxenfurt-class note ()
  (id
   text
   kind))

(define-unprintable-printer note
  "~a ~s" (kind note) (id note))

(define-converter note
  :id (=> "id")
  :text (=> "text")
  :kind (=> "type"))

(define-oxenfurt-class derivative ()
  (id
   language
   regions
   domains
   registers
   text))

(define-unprintable-printer derivative
  "~s ~a" (id derivative) (language derivative))

(define-converter derivative
  :id (=> "id")
  :language (param->key (=> "language"))
  :regions (=> "regions")
  :domains (mapcar #'special->key (=> "domains"))
  :registers (mapcar #'special->key (=> "registers"))
  :text (=> "text"))

(define-oxenfurt-class variant-form ()
  (regions
   text))

(define-unprintable-printer variant-form
  "~s" (text variant-form))

(define-converter variant-form
  :regions (=> "regions")
  :text (=> "text"))

(define-oxenfurt-class sense ()
  (cross-reference-markers
   cross-references
   definitions
   domains
   examples
   id
   notes
   pronunciations
   regions
   registers
   subsenses
   translations
   variant-forms))

(define-unprintable-printer sense
  "~s" (id sense))

(define-converter sense
  :cross-reference-markers (=> "crossReferenceMarkers")
  :cross-references (into 'cross-reference (=> "crossReference"))
  :definitions (=> "definitions")
  :domains (mapcar #'special->key (=> "domains"))
  :examples (into 'example (=> "examples"))
  :id (=> "id")
  :notes (into 'note (=> "notes"))
  :pronunciations (into 'pronunciation (=> "pronunciations"))
  :regions (=> "regions")
  :registers (mapcar #'special->key (=> "registers"))
  :subsenses (into 'sense (=> "subsenses"))
  :translations (into 'translation (=> "translations"))
  :variant-forms (into 'variant-form (=> "variantForms")))

(define-oxenfurt-class cross-reference ()
  (id
   text
   kind))

(define-unprintable-printer cross-reference
  "~a ~s" (kind cross-reference) (id cross-reference))

(define-converter cross-reference
  :id (=> "id")
  :text (=> "text")
  :kind (special->key (=> "kind")))

(define-oxenfurt-class example ()
  (definitions
   domains
   notes
   regions
   registers
   sense-ids
   text
   translations))

(define-unprintable-printer example
  "~s" (text example))

(define-converter example
  :definitions (=> "definitions")
  :domains (mapcar #'special->key (=> "domains"))
  :notes (into 'note (=> "notes"))
  :regions (=> "regions")
  :registers (mapcar #'special->key (=> "registers"))
  :sense-ids (=> "senseIds")
  :text (=> "text")
  :translations (into 'translation (=> "translations")))

(define-oxenfurt-class translation ()
  (domains
   grammatical-features
   language
   notes
   regions
   registers
   text))

(define-unprintable-printer translation
  "~s ~a" (text translation) (language translation))

(define-converter translation
  :domains (mapcar #'special->key (=> "domains"))
  :grammatical-features (into 'grammatical-feature (=> "grammaticalFeatures"))
  :language (param->key (=> "language"))
  :notes (into 'note (=> "notes"))
  :regions (=> "regions")
  :registers (mapcar #'special->key (=> "registers"))
  :text (=> "text"))

(define-oxenfurt-class match ()
  (id
   inflection-id
   match-string
   match-type
   region
   word
   score))

(define-unprintable-printer match
  "~s ~f" (id match) (score match))

(define-converter match
  :id (=> "id")
  :inflection-id (=> "inflection_id")
  :match-string (=> "matchString")
  :match-type (=> "matchType")
  :region (param->key (=> "region"))
  :word (=> "word")
  :score (=> "score"))

(define-oxenfurt-class sentence ()
  (definitions
   domains
   notes
   regions
   registers
   sense-ids
   text
   translations))

(define-unprintable-printer sentence
  "~s" (text sentence))

(define-converter sentence
  :definitions (=> "definitions")
  :domains (mapcar #'special->key (=> "domains"))
  :notes (into 'note (=> "notes"))
  :regions (=> "regions")
  :registers (mapcar #'special->key (=> "registers"))
  :sense-ids (=> "senseIds")
  :text (=> "text")
  :translations (into 'translation (=> "translations")))

(define-oxenfurt-class frequency ()
  (frequency
   normalized-frequency
   lemma
   normalized-lemma
   tokens
   lexical-category
   grammatical-features
   components
   first-mention
   true-case
   wordform))

(define-unprintable-printer frequency
  "~s ~a" (or (lemma frequency) (tokens frequency)) (frequency frequency))

(define-converter frequency
  :frequency (=> "frequency")
  :normalized-frequency (=> "normalizedFrequency")
  :lemma (=> "lemma")
  :normalized-lemma (=> "normalizedLemma")
  :tokens (=> "tokens")
  :lexical-category (special->key (=> "lexicalCategory"))
  :grammatical-features (into 'grammatical-feature (=> "grammaticalFeatures"))
  :components (=> "components")
  :first-mention (=> "firstMention")
  :true-case (=> "trueCase")
  :wordform (=> "wordform"))

#|
 This file is a part of Oxenfurt
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.oxenfurt)

;;; api.lisp
(docs:define-docs
  (variable *api*
    "The base endpoint URL of the Oxford dictionary API.")
  
  (variable *app-id*
    "Your Oxford dictionary API app ID.

See https://developer.oxforddictionaries.com/admin/applications")
  
  (variable *app-key*
    "Your Oxford dictionary API app key.

See https://developer.oxforddictionaries.com/admin/applications")
  
  (type api-call-failed
    "Error signalled whenever an API call fails for whatever reason.

See URL
See RESULT
See BODY
See REQUEST")
  
  (function url
    "Returns the URL that the request tried to access.

See API-CALL-FAILED")
  
  (function result
    "Returns the HTTP return code for the failed access.

See API-CALL-FAILED")
  
  (function body
    "Returns the HTTP body as a string.

See API-CALL-FAILED")
  
  (function list-languages
    "Returns a list of LANGUAGE-DATASETs that the API supports.

If source-lang is given, only that language is returned.
If source- and target-lang are given, the API only succeeds if a
translation from source to target is possible.

See LANGUAGE-DATASET")
  
  (function list-filters
    "Returns a list of possible filters, either for each endpoint, or for a particular one.")
  
  (function list-lexical-categories
    "Returns a list of possible lexical categories for the given language or translation.")
  
  (function list-registers
    "Returns a list of possible registers for the given language or translation.")
  
  (function list-domains
    "Returns a list of possible domains for the given language or translation.")
  
  (function list-regions
    "Returns a plist of known regions to their alternate names for the given language.")
  
  (function list-grammatical-features
    "Returns a plist of grammatical features to their possible values for the given language.")
  
  (function inflections
    "Check whether a given word exists in the dictionary and retrieve its root form.

See INFLECTION")
  
  (function words
    "Retrieve dictionary information for a given word.

The parameters FILTERS, SYNONYMS/ANTONYMS, TARGET-LANG, and SENTENCES
can only be used one at a time. The API does not support, for instance,
sentences for translations into a target language.

SYNONYMS, ANTONYMS, and SENTENCES are booleans about whether to return
information from the thesaurus or sentence lexicon for the given word.

TARGET-LANG specifies a language for which translations from the given
SOURCE-LANG of the word should be returned.

FILTERS can be a plist of filter keys to lists of filter values.
See LIST-FILTERS for the possible combinations. One additional, special
filter is possible that cannot be combined with other filters: :REGION.
Its value should be a list of available regions to which to constrain
word results. See LIST-REGIONS for possible region names.

See WORD")
  
  (function search-words
    "Search the dictionary for matching words.

The query is matched using headwords, lemmatization, and fuzzy matching.

If PREFIX is true, only words starting with the query are considered.

REGIONS can be a list of regions that the words must match.

When TARGET-LANG is given, the translation dictionaries are searched instead.

See MATCH")
  
  (function list-words
    "List words matching certain criteria.

FILTERS can be a plist of filter keys to lists of filter values.
See LIST-FILTERS for the possible combinations.

EXCLUDE is the same as FILTERS, but excludes headwords that contain
senses that match the exclude pattern.

EXCLUDE-SENSES is the same as EXCLUDE but excludes matching senses
for headwords.

EXCLUDE-PRIME-SENSES is the same as EXCLUDE but only excludes a
headword if the primary sense matches the pattern.

WORD-LENGTH can either be an integer or a cons of (LOWER . UPPER)
to specify the limits of the word lengths in characters. Both LOWER
and UPPER can be NIL if the limit should only go one way.

PREFIX filters to words that start with the given string.
If EXACT is true, only exact matches are returned.

See WORD")
  
  (function word-frequency
    "Returns word usage frequency information.

The LEMMA can either be a single word, or a list of words. In the
latter case, the results are split into the smallest possible units.

CORPUS should always by NMC. Other corpora will require special
support from the Oxford API.

WORDFROM can be the written form of the word to look up, preserving
the capitalisation.

TRUE-CASE can be the written form of the word to look up with
normalised case.

LEXICAL-CATEGORY can be the lexical category to match for the word
to look up. See LIST-LEXICAL-CATEGORIES for available categories.

GRAMMATICAL-FEATURES should be a plist of constraints on the
grammatical features of the specific word to look up.
See LIST-GRAMMATICAL-FEATURES for possible options.

SORT should be one or more fields to sort by. Each field can
either be the name of the field directly, or a cons where the
cdr is one of :ASC or :DSC, denoting the sorting order. The
available fields are :WORDFORM :TRUE-CASE :LEMMA :LEXICAL-CATEGORY
:FREQUENCY :NORMALIZED-FREQUENCY.

FREQUENCY can be a constraint on the allowed frequency. Can be
either a single number for an exact match, or a cons for a lower
and upper bound. Both bounds can be NIL.

NORMALIZED-FREQUENCY can be a constraint on the allowed normalized
frequency. Can be either a single number for an exact match, or a
cons for a lower and upper bound. Both bounds can be NIL.

See FREQUENCY")
  
  (function ngram-frequency
    "Returns ngram (multiple word) frequency information.

TOKENS should be either a single list of tokens for one ngram, or
a list of lists. Each sub-list must be the same length.

CORPUS should always by NMC. Other corpora will require special
support from the Oxford API.

CONTAINS can be a list of tokens. If given, only ngrams which
match all of the given tokens. The order of the tokens is
irrelevant.

If PUNCTUATION is true, ngrams that include punctuation are also
included in the results.

FORMAT denotes whether the tokens are returned as a list or as
a single string. For the list, choose :OUP, for a single string,
choose :GOOGLE.

FREQUENCY can be a constraint on the allowed frequency. Can be
either a single number for an exact match, or a cons for a lower
and upper bound. Both bounds can be NIL.

DOCUMENT-FREQUENCY can be a constraint on the required frequency
of the ngram within documents. Can be either a single number for
an exact match, or a cons for a lower and upper bound. Both bounds
can be NIL.

See FREQUENCY"))

;;; objects.lisp
(docs:define-docs
  (type cross-reference
    "")
  
  (type derivative
    "")
  
  (type entry
    "")
  
  (type example
    "")
  
  (type frequency
    "")
  
  (type inflection
    "")
  
  (type language-dataset
    "")
  
  (type lexical-entry
    "")
  
  (type match
    "")
  
  (type note
    "")
  
  (type pronunciation
    "")
  
  (type sense
    "")
  
  (type sentence
    "")
  
  (type translation
    "")
  
  (type variant-form
    "")
  
  (type word
    "")
  
  ;; Slots
  (function audio-file
    "")
  
  (function components
    "")
  
  (function cross-reference-markers
    "")
  
  (function cross-references
    "")
  
  (function definitions
    "")
  
  (function derivative-of
    "")
  
  (function derivatives
    "")
  
  (function dialects
    "")
  
  (function domains
    "")
  
  (function entries
    "")
  
  (function etymologies
    "")
  
  (function examples
    "")
  
  (function first-mention
    "")
  
  (function frequency
    "")
  
  (function grammatical-features
    "")
  
  (function homograph-number
    "")
  
  (function id
    "")
  
  (function inflection-id
    "")
  
  (function inflection-of
    "")
  
  (function kind
    "")
  
  (function language
    "")
  
  (function lemma
    "")
  
  (function lexical-category
    "")
  
  (function lexical-entries
    "")
  
  (function match-string
    "")
  
  (function match-type
    "")
  
  (function normalized-frequency
    "")
  
  (function normalized-lemma
    "")
  
  (function notes
    "")
  
  (function phonetic-notation
    "")
  
  (function phonetic-spelling
    "")
  
  (function pronunciations
    "")
  
  (function region
    "")
  
  (function regions
    "")
  
  (function registers
    "")
  
  (function score
    "")
  
  (function sense-ids
    "")
  
  (function senses
    "")
  
  (function sentences
    "")
  
  (function source
    "")
  
  (function subsenses
    "")
  
  (function target
    "")
  
  (function text
    "")
  
  (function tokens
    "")
  
  (function translations
    "")
  
  (function true-case
    "")
  
  (function variant-forms
    "")
  
  (function word
    "")
  
  (function wordform
    ""))

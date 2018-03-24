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

See WORD")
  
  (function find-word
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
  (type antonym
    "Represents an antonym of a sense.

See DOMAINS
See ID
See LANGUAGE
See REGIONS
See REGISTERS
See TEXT")
  
  (type cross-reference
    "Represents a cross reference between senses.

The KIND shows how the words are related. Can be either
:close-match :related :see-also :variant-spelling :abbreviation
:pre :post.

See ID
See TEXT
See KIND")
  
  (type derivative
    "Represents a related entry of a sense.

See DOMAINS
See ID
See LANGUAGE
See REGIONS
See REGISTERS
See TEXT")
  
  (type entry
    "A word entry in a lexical entry.

See ETYMOLOGIES
See GRAMMATICAL-FEATURES
See HOMOGRAPH-NUMBER
See NOTES
See PRONUNCIATIONS
See SENSES
See VARIANT-FORMS")
  
  (type example
    "Represents an example usage of a word.

See DEFINITIONS
See DOMAINS
See NOTES
See REGIONS
See REGISTERS
See SENSE-IDS
See TEXT
See TRANSLATIONS")
  
  (type frequency
    "Container for frequency information of a word or ngram.

In the case of an ngram frequency, only the FREQUENCY and
the TOKENS fields are guaranteed to be set.

See FREQUENCY
See NORMALIZED-FREQUENCY
See LEMMA
See NORMALIZED-LEMMA
See TOKENS
See LEXICAL-CATEGORY
See GRAMMATICAL-FEATURES
See COMPONENTS
See FIRST-MENTION
See TRUE-CASE
See WORDFORM")
  
  (type language-dataset
    "Container for information about a language in the API.

See REGION
See SOURCE
See LANGUAGE
See TARGET-LANG
See BILINGUAL-P")
  
  (type lexical-entry
    "An entry of lexical information about a word.

This is the primary information container for a word.

See GRAMMATICAL-FEATURES
See INFLECTION-OF
See LANGUAGE
See LEXICAL-CATEGORY
See TEXT
See DERIVATIVE-OF
See DERIVATIVES
See ENTRIES
See NOTES
See PRONUNCIATIONS
See VARIANT-FORMS
See SENTENCES")
  
  (type match
    "Represents a match for a word search query.

See ID
See INFLECTION-ID
See MATCH-STRING
See MATCH-TYPE
See REGION
See WORD
See SCORE")
  
  (type note
    "A human-readable note about an object.

This usually contains associated, additional information for the
human reader. The KIND is a free-form string that describes what
this note might contain.

See ID
See TEXT
See KIND")
  
  (type pronunciation
    "Represents information about the pronunciation of a lexical entry.

See AUDIO-FILE
See DIALECTS
See REGIONS
See PHONETIC-NOTATION
See PHONETIC-SPELLING")
  
  (type sense
    "Represents information about a specific sense or meaning of a word.

See CROSS-REFERENCE-MARKERS
See CROSS-REFERENCES
See DEFINITIONS
See DOMAINS
See EXAMPLES
See ID
See NOTES
See PRONUNCIATIONS
See REGIONS
See REGISTERS
See SUBSENSES
See TRANSLATIONS
See VARIANT-FORMS")
  
  (type sentence
    "Represents information about an example usage sentence for a word.

See DEFINITIONS
See DOMAINS
See NOTES
See REGIONS
See REGISTERS
See SENSE-IDS
See TEXT
See TRANSLATIONS")

  (type synonym
    "Represents a synonym of a sense.

See DOMAINS
See ID
See LANGUAGE
See REGIONS
See REGISTERS
See TEXT")
  
  (type translation
    "Represents information about a translation of a word into a different language.

See DOMAINS
See GRAMMATICAL-FEATURES
See LANGUAGE
See NOTES
See REGIONS
See REGISTERS
See TEXT")
  
  (type variant-form
    "Represents information about an alternate form of a word that can be used interchangeably.

See REGIONS
See TEXT")
  
  (type word
    "Represents a specific word in the dictionary.

See ID
See LANGUAGE
See LEXICAL-ENTRIES
See PRONUNCIATIONS
See WORD")
  
  ;; Slots
  (function audio-file
    "Returns the URL of a sound file for the pronunciation.

See PRONUNCIATION")

  (function bilingual-p
    "Returns whether the language is bilingual (can be translated).

See LANGUAGE-DATASET")
  
  (function components
    "Returns the components of a frequency result.

See FREQUENCY")
  
  (function cross-reference-markers
    "Returns a grouping of cross reference notes.

See SENSE")
  
  (function cross-references
    "Returns a list of cross-references.

See SENSE")
  
  (function definitions
    "Returns a list of definitions for the exact meaning of the object.

See SENSE
See EXAMPLE
See SENTENCE")
  
  (function derivative-of
    "Returns a list of other words from which this one derives.

See LEXICAL-ENTRY")
  
  (function derivatives
    "Returns a list of other words which derive from this one.

See LEXICAL-ENTRY")
  
  (function dialects
    "Returns a list of dialects in which this pronunciation is used.

See PRONUNCIATION")
  
  (function domains
    "Returns the name of a subject, discipline, or branch of knowledge related to this object.

See ANTONYM
See SYNONYM
See DERIVATIVE
See SENSE
See EXAMPLE
See TRANSLATION
See SENTENCE")
  
  (function entries
    "Returns the list of word entries for this lexical entry.

See LEXICAL-ENTRY")
  
  (function etymologies
    "Returns a list of strings describing the etymology of the word.

See ENTRY")
  
  (function examples
    "Returns a list of example usages of the word.

See SENSE")
  
  (function first-mention
    "Returns a date formatted as a string representing the time this word was first observed.

See FREQUENCY")
  
  (function frequency
    "Returns a number representing the usage frequency of the word or ngram.

See FREQUENCY")
  
  (function grammatical-features
    "Returns a plist of applicable grammatical features and their values.

See LEXICAL-ENTRY
See FREQUENCY
See TRANSLATION
See ENTRY")
  
  (function homograph-number
    "Identifies the homograph grouping.

The last two digits identify different entries of the same homograph.
The first one/two digits identify the homograph number.

See ENTRY")
  
  (function id
    "The identifier of the word this object is about.

See ANTONYM
See SYNONYM
See NOTE
See DERIVATIVE
See SENSE
See CROSS-REFERENCE
See MATCH
See WORD")
  
  (function inflection-id
    "The identifier for the specific inflection of the word that was matched.

See MATCH")
  
  (function inflection-of
    "Returns a list of word IDs for which this lexical entry is an inflection.

See LEXICAL-ENTRY")
  
  (function kind
    "Returns an identifier for the kind of data this object carries.

See NOTE
See CROSS-REFERENCE")
  
  (function language
    "Returns a language identifier for which this object is meant.

See ANTONYM
See SYNONYM
See DERIVATIVE
See TRANSLATION
See LANGUAGE-DATASET
See LEXICAL-ENTRY
See WORD")
  
  (function lemma
    "Returns the lemma for which this frequency applies.

See FREQUENCY")
  
  (function lexical-category
    "Returns the linguistic category for which this word qualifies.

Generally the category is defined by the syntactic or morphological
behaviour of the lexical item in question, such as noun or verb.

See FREQUENCY
See LEXICAL-ENTRY")
  
  (function lexical-entries
    "Returns a list of lexical entries that this word represents.

See WORD")
  
  (function match-string
    "Returns the string that was used to match.

See MATCH")
  
  (function match-type
    "Returns the kind of match that occurred.

See MATCH")
  
  (function normalized-frequency
    "Returns the normalized frequency number.

This is the frequency per million based on a corpus.

See FREQUENCY")
  
  (function normalized-lemma
    "Returns the normalised form of the lemma.

See FREQUENCY")
  
  (function notes
    "Returns a list of notes about this object.

See LEXICAL-ENTRY
See ENTRY
See SENSE
See EXAMPLE
See TRANSLATION
See SENTENCE")
  
  (function phonetic-notation
    "Returns an identifier for the phonetic notation used to describe the pronunciation.

See PRONUNCIATION")
  
  (function phonetic-spelling
    "Returns a string describing how this word should be pronounced in some notation.

See PRONUNCIATION")
  
  (function pronunciations
    "Returns a list of possible pronunciations for this object.

See LEXICAL-ENTRY
See ENTRY
See WORD
See SENSE")
  
  (function region
    "Returns the name of a region for which this match applies.

See MATCH")
  
  (function regions
    "Returns a list of region names to which this object applies.

See ANTONYM
See SYNONYM
See PRONUNCIATION
See DERIVATIVE
See VARIANT-FORM
See SENSE
See EXAMPLE
See TRANSLATION
See SENTENCE")
  
  (function registers
    "Returns a list describing the level of language usage, typically with respect to formality.

See ANTONYM
See SYNONYM
See DERIVATIVE
See SENSE
See EXAMPLE
See TRANSLATION
See SENTENCE")
  
  (function score
    "Returns a number representing how highly this match scored in the search.

See MATCH")
  
  (function sense-ids
    "Returns a list of IDs for senses related to this object.

See EXAMPLE
See SENTENCE")
  
  (function senses
    "Returns a list of senses for this entry.

See ENTRY")
  
  (function sentences
    "Returns a list of example usage sentences for this lexical entry.

See LEXICAL-ENTRY")
  
  (function source
    "Returns a string description of the source of the dictionary.

See LANGUAGE-DATASET")
  
  (function subsenses
    "Returns an ordered list of subsenses of a sense.

See SENSE")
  
  (function target-lang
    "Returns a descriptor for the target language of a translation dataset.

See LANGUAGE-DATASET")
  
  (function text
    "Returns a full text representation of the object.

See ANTONYM
See SYNONYM
See LEXICAL-ENTRY
See NOTE
See DERIVATIVE
See VARIANT-FORM
See CROSS-REFERENCE
See EXAMPLE
See TRANSLATION
See SENTENCE")
  
  (function tokens
    "Returns the ngram tokens for the frequency result.

See FREQUENCY")
  
  (function translations
    "Returns a list of translations for the given object.

See SENSE
See EXAMPLE
See SENTENCE")
  
  (function true-case
    "Returns a given written realisation of an entry, typically in lower case.

See FREQUENCY")
  
  (function variant-forms
    "Returns a list of variant forms for the given object.

See LEXICAL-ENTRY
See ENTRY
See SENSE")
  
  (function word
    "Returns a given written or spoken realisation of a an entry, lowercased.

See WORD
See MATCH")
  
  (function wordform
    "Returns a given written realisation of an entry, preserving case.

See FREQUENCY"))

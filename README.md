## About Oxenfurt
This library implements a client for the [Oxford dictionary API](A given written or spoken realisation of a an entry, lowercased.). It gives you full access to the available REST endpoints and transforms all results into native Lisp instances for easy handling.

## How To
First you'll need to sign up for their API and get the App keys. Once you have registered, you can find the keys [here](https://developer.oxforddictionaries.com/admin/applications). Then, set the library up with your keys:

    (setf oxenfurt:*app-id* "dadwadwwada"
          oxenfurt:*app-key* "2131831273189addhadwhauidhalidhwa")

Once that's done, you should be able to query the API to your heart's content... within the restrictions imposed by your account's plan of course.

Oxford's API is fairly extensive in what it returns. For instance, if we look at the result for "lisp":

    (oxenfurt:lexical-entries (oxenfurt:find-word "lisp"))
    ; => (#<LEXICAL-ENTRY "lisp" EN> #<LEXICAL-ENTRY "lisp" EN> #<LEXICAL-ENTRY "Lisp" EN>)
    
    (oxenfurt:describe-tree (first *))
    ; LANGUAGE            :EN
    ; LEXICAL-CATEGORY    :NOUN
    ; TEXT                "lisp"
    ; DERIVATIVES         ([DERIVATIVE])
    ;   ID                  "lisper"
    ;   TEXT                "lisper"
    ; ENTRIES             ([ENTRY])
    ;   ETYMOLOGIES         ([STRING])
    ;     "Old English wlispian (recorded in Äwlyspian), from wlisp (adjective) âlispingâ, of imitative origin; compare with Dutch lispen and German lispeln"
    ;   GRAMMATICAL-FEATURES [PLIST]
    ;     :NUMBER           :SINGULAR
    ;   HOMOGRAPH-NUMBER    0
    ;   SENSES              ([SENSE])
    ;     DEFINITIONS         ([STRING])
    ;       "a speech defect in which s is pronounced like th in thick and z is pronounced like th in this"
    ;     DOMAINS             [PLIST]
    ;       :PHONETICS        NIL
    ;     EXAMPLES            ([EXAMPLE])
    ;       TEXT                "he spoke with a slight lisp"
    ;     ID                  "m_en_gbus0586220.005"
    ; PRONUNCIATIONS      ([PRONUNCIATION])
    ;   DIALECTS            ([STRING])
    ;     "British English"
    ;   PHONETIC-NOTATION   "IPA"
    ;   PHONETIC-SPELLING   "lÉªsp"

While Oxford's API documentation does cover everything, it is a bit obtuse to handle. The documentation of this project includes descriptions for every object type and field that is returned, perhaps that will be easier to navigate. If you prefer to explore, the above `describe-tree` function should also provide a good tool to figure out where relevant information is stored.

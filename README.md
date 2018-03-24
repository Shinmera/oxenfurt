## About Oxenfurt
This library implements a client for the [Oxford dictionary API](https://developer.oxforddictionaries.com/). It gives you full access to the available REST endpoints and transforms all results into native Lisp instances for easy handling.

## How To
First you'll need to sign up for their API and get the App keys. Once you have registered, you can find the keys [here](https://developer.oxforddictionaries.com/admin/applications). Then, set the library up with your keys:

    (ql:quickload :oxenfurt)
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
    ; DERIVATIVES         ([DERIVATIVE] 1)
    ;   ID                  "lisper"
    ;   TEXT                "lisper"
    ;   --
    ; ENTRIES             ([ENTRY] 1)
    ;   ETYMOLOGIES         ([STRING] 1)
    ;     "Old English wlispian (recorded in āwlyspian), from wlisp (adjective) ‘lisping’, of imitative origin; compare with Dutch lispen and German lispeln"
    ;   GRAMMATICAL-FEATURES ([KEYWORD] 2)
    ;     :NUMBER
    ;     :SINGULAR
    ;   HOMOGRAPH-NUMBER    0
    ;   SENSES              ([SENSE] 1)
    ;     DEFINITIONS         ([STRING] 1)
    ;       "a speech defect in which s is pronounced like th in thick and z is pronounced like th in this"
    ;     DOMAINS             ([KEYWORD] 1)
    ;       :PHONETICS
    ;     EXAMPLES            ([EXAMPLE] 1)
    ;       TEXT                "he spoke with a slight lisp"
    ;       --
    ;     ID                  "m_en_gbus0586220.005"
    ;     --
    ;   --
    ; PRONUNCIATIONS      ([PRONUNCIATION] 1)
    ;   DIALECTS            ([STRING] 1)
    ;     "British English"
    ;   PHONETIC-NOTATION   "IPA"
    ;   PHONETIC-SPELLING   "lɪsp"
    ;   --

While Oxford's API documentation does cover everything, it is a bit obtuse to handle. The documentation of this project includes descriptions for every object type and field that is returned, perhaps that will be easier to navigate. If you prefer to explore, the above `describe-tree` function should also provide a good tool to figure out where relevant information is stored.

Oxenfurt supports two HTTP clients as backends: Dexador and Drakma. By default, Dexador is used. If you prefer Drakma, load `oxenfurt-drakma` instead.

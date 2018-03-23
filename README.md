## About Oxenfurt
This library implements a client for the [Oxford dictionary API](A given written or spoken realisation of a an entry, lowercased.). It gives you full access to the available REST endpoints and transforms all results into native Lisp instances for easy handling.

## How To
First you'll need to sign up for their API and get the App keys. Once you have registered, you can find the keys [here](https://developer.oxforddictionaries.com/admin/applications). Then, set the library up with your keys:

    (setf oxenfurt:*app-id* "dadwadwwada"
          oxenfurt:*app-key* "2131831273189addhadwhauidhalidhwa")

Once that's done, you should be able to query the API to your heart's content... within the restrictions imposed by your account's plan of course.

Oxford's API is fairly extensive in what it returns. For instance, if we look at the result for "lisp":

    (lexical-entries (find-word "lisp"))
    ; => (#<LEXICAL-ENTRY "lisp" EN> #<LEXICAL-ENTRY "lisp" EN> #<LEXICAL-ENTRY "Lisp" EN>)
    
    (describe (first *))
    ; #<LEXICAL-ENTRY "lisp" EN>
    ;   [standard-object]
    ; 
    ; Slots with :INSTANCE allocation:
    ;   GRAMMATICAL-FEATURES           = NIL
    ;   INFLECTION-OF                  = NIL
    ;   LANGUAGE                       = :EN
    ;   LEXICAL-CATEGORY               = :NOUN
    ;   TEXT                           = "lisp"
    ;   DERIVATIVE-OF                  = NIL
    ;   DERIVATIVES                    = (#<DERIVATIVE "lisper" NIL>)
    ;   ENTRIES                        = (#<ENTRY {1006578ED3}>)
    ;   NOTES                          = NIL
    ;   PRONUNCIATIONS                 = (#<PRONUNCIATION "lÉªsp" (IPA) British English>)
    ;   VARIANT-FORMS                  = NIL
    ;   SENTENCES                      = NIL

While Oxford's API documentation does cover everything, it is a bit obtuse to handle. The documentation of this project includes descriptions for every object type and field that is returned, perhaps that will be easier to navigate.

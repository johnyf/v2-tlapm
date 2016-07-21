------------------------------ MODULE instance ------------------------------
VARIABLE z

I == INSTANCE instanceA WITH x <- z, y <- z 

LEMMA I!T # ENABLED(z' # z') BY DEF I!T, I!A

LEMMA I!T2 # ENABLED(z' # z') BY DEF I!T2

LEMMA I!TH # ENABLED(z' # z') BY DEF I!TH


=============================================================================
\* Modification History
\* Last modified Fri Feb 27 11:35:27 CET 2015 by marty
\* Created Tue Feb 24 11:51:43 CET 2015 by marty

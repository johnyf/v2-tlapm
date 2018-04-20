----------------------- MODULE extends_use_def_outer -----------------------

EXTENDS extends_use_def_inner

LEMMA \A y : (D(y) = y') OBVIOUS  \* cannot be proved

LEMMA \A y : (D(y) = y') BY DEF D 

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 14:40:20 CEST 2018 by merz
\* Last modified Tue Jan 26 15:17:26 CET 2016 by marty
\* Created Tue Jan 26 15:16:36 CET 2016 by marty

-------------------------- MODULE extends_use_def --------------------------

------- MODULE inner ------

D(x) == x'

USE DEF D

====

------- MODULE outer -------
 EXTENDS inner

 LEMMA \A y : (D(y) = y') OBVIOUS 

 LEMMA \A y : (D(y) = y') BY DEF D 

====


=============================================================================
\* Modification History
\* Last modified Tue Jan 26 17:21:30 CET 2016 by marty
\* Created Tue Jan 26 14:51:48 CET 2016 by marty

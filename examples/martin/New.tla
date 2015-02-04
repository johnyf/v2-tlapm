-------------------------------- MODULE New --------------------------------

LEMMA CONTRADICTION == ASSUME NEW A PROVE ~A

LEMMA ASSUME NEW P PROVE ~P BY CONTRADICTION  
LEMMA ASSUME VARIABLE P PROVE ~P BY CONTRADICTION

LEMMA ASSUME
        ASSUME NEW A PROVE ~A,
        NEW P
      PROVE ~P
OBVIOUS

=============================================================================
\* Modification History
\* Last modified Tue Jan 20 14:38:43 CET 2015 by marty
\* Created Tue Jan 20 14:38:25 CET 2015 by marty

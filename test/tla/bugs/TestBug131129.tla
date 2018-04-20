--------------------------- MODULE TestBug131129 ---------------------------

THEOREM TRUE
PROOF
     <2>2. TRUE     
     <2>4. QED
       BY <2>2, <2>4  \* sm 2018-04-20: correctly raises a warning about <2>4 being used in its proof despite not having assumptions

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 16:12:33 CEST 2018 by merz
\* Last modified Mon Jan 19 14:24:00 CET 2015 by marty
\* Created Mon Jan 19 14:23:19 CET 2015 by marty

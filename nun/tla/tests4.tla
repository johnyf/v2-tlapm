------------------------------- MODULE tests4 -------------------------------

THEOREM 
    ASSUME 
        NEW a, 
        NEW b, 
        a#b, 
        NEW P,
        \A x, y : x # y => P[x] # P[y]
    PROVE P[P[a]] # P[P[b]] 
OBVIOUS


THEOREM 
    ASSUME 
        NEW a, 
        NEW b, 
        a#b, 
        NEW P,
        \A x, y : x # x => P[x] # P[y]
    PROVE P[P[a]] # P[P[b]] 
OBVIOUS

=============================================================================
\* Modification History
\* Last modified Wed May 11 01:50:13 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
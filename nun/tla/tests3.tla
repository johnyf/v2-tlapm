------------------------------- MODULE tests3 -------------------------------

THEOREM 
    ASSUME 
        NEW a, 
        NEW b, 
        a#b, 
        NEW P,
        \A x, y : x # y => P[x] # P[y]
    PROVE P[P[a]] # P[P[b]] OBVIOUS

=============================================================================
\* Modification History
\* Last modified Wed May 11 01:22:47 CEST 2016 by Matthieu
\* Created Tue May 10 17:43:48 CEST 2016 by Matthieu

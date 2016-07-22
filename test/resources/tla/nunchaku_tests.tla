------------------------------- MODULE nunchaku_tests -------------------------------

THEOREM ASSUME NEW a, NEW b, a#b, NEW S, S = {a,b} PROVE S = b OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW S, S = {a,b} PROVE S = b OBVIOUS

THEOREM ASSUME NEW a, NEW b PROVE a=b OBVIOUS

THEOREM ASSUME NEW a, NEW b PROVE a # b OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a /\ c=b OBVIOUS

(*

THEOREM ASSUME NEW a, NEW b, NEW c, a # b, b # c PROVE a # c OBVIOUS

THEOREM ASSUME NEW a, NEW b, a#b, NEW P PROVE P[a] # P[a] OBVIOUS
 
THEOREM ASSUME NEW a, NEW b, a#b, NEW P PROVE P[a] # P[b] OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a \/ c=b OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a /\ c=b OBVIOUS

CONSTANT a,b,c,d,S

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x \in {a,b} : \E x0 \in {a,b} : x # x0 OBVIOUS

LEMMA \A x \in {a,b} : \E x0 \in {b,a} : x = x0 OBVIOUS

LEMMA \A x \in {a,b} : \E x0 \in {a} : x = x0 OBVIOUS

*)
    
=============================================================================
\* Modification History
\* Last modified Fri Jul 22 13:53:14 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu

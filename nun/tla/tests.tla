------------------------------- MODULE tests -------------------------------

THEOREM ASSUME NEW a, NEW b PROVE a=b OBVIOUS

THEOREM ASSUME NEW a, NEW b PROVE a # b OBVIOUS

THEOREM ASSUME NEW a, NEW b, a#b, NEW P PROVE P[a] # P[a] OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c, a # b, b # c PROVE a # c OBVIOUS

\*FIXME THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a \/ c=b OBVIOUS

CONSTANT a,b,c,d,S

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x1 \in {a,b} : \E x0 \in {a,b} : x1 # x0 OBVIOUS

LEMMA \A x1 \in {a,b} : \E x0 \in {b,a} : x1 = x0 OBVIOUS

LEMMA \A x1 \in {a,b} : \E x0 \in {a} : x1 = x0 OBVIOUS

S2 == {a,b,c}

LEMMA \A x \in S2 : \E x0 \in S2 : x # x0 BY DEF S2

THEOREM 
    ASSUME 
        NEW v, 
        NEW w, 
        v#w, 
        NEW P,
        \A x, y : x # y => P[x] # P[y]
    PROVE P[P[v]] # P[P[w]] OBVIOUS

=============================================================================
\* Modification History
\* Last modified Thu Jun 16 16:48:26 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
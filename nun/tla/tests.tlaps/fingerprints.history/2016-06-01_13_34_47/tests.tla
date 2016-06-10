------------------------------- MODULE tests -------------------------------

CONSTANT a,b,c

LEMMA \A x \in {a,b,c} : \E x0 \in {a,b,c} : x # x0 OBVIOUS

S == {a,b,c}

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x \in S : \E x0 \in S : x # x0 BY DEF S

S2 == {a,b,c}

LEMMA \A x \in {a,b,c} : \E x0 \in {b,c,a} : x # x0 BY DEF S2

LEMMA {a, a, b} = {a, b} OBVIOUS

=============================================================================
\* Modification History
\* Last modified Wed Jun 01 13:34:47 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
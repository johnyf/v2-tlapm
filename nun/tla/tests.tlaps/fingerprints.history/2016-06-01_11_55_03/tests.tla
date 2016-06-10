------------------------------- MODULE tests -------------------------------

CONSTANT a,b,c

LEMMA \A x \in {a,b,c} : \E x0 \in {a,b,c} : x # x0 OBVIOUS

S == {a,b,c}

LEMMA \A x \in S : \E x0 \in S : x # x0 BY DEF S

LEMMA \A x \in {a,b,c} : \E x0 \in {b,c,a} : x # x0 BY DEF S

LEMMA {a, a, b} = {a, b} OBVIOUS

=============================================================================
\* Modification History
\* Last modified Wed Jun 01 11:54:53 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
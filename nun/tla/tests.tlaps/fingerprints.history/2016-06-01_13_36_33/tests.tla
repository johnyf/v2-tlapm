------------------------------- MODULE tests -------------------------------

CONSTANT a,b,c

LEMMA \A x \in {a,b,c} : \E x0 \in {a,b,c} : x # x0 OBVIOUS

S1 == {a,b,c}

LEMMA \A x \in S1 : \E x0 \in S1 : x # x0 OBVIOUS

S2 == {a,b,c}

LEMMA \A x \in S2 : \E x0 \in S2 : x # x0 BY DEF S2

S3 == {a,b,c}

LEMMA \A x \in {a,b,c} : \E x0 \in {b,c,a} : x # x0 BY DEF S3

LEMMA {a, a, b} = {a, b} OBVIOUS

=============================================================================
\* Modification History
\* Last modified Wed Jun 01 13:36:33 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
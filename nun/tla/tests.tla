------------------------------- MODULE tests -------------------------------

CONSTANT a,b,c,d,S

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x1 \in {a,b,c} : \E x0 \in {a,b,c} : x1 # x0 OBVIOUS

S2 == {a,b,c}

LEMMA \A x \in S2 : \E x0 \in S2 : x # x0 BY DEF S2

=============================================================================
\* Modification History
\* Last modified Mon Jun 13 15:02:00 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
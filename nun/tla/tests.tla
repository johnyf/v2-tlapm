------------------------------- MODULE tests -------------------------------

CONSTANT a,b,c,d,S

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x \in {a,b,c} : \E x0 \in {a,b,c,d} : x # x0 OBVIOUS

\* S == {a,b,c}

\* LEMMA \A x \in S : \E x0 \in S : x # x0 BY DEF S

=============================================================================
\* Modification History
\* Last modified Fri Jun 10 00:56:41 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
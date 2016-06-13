---- MODULE obligation_bug1 ----
\* Tests issue 2: https://github.com/tlaplus/v2-tlapm/issues/2
\* the lemmas should fail but the bug introduces additional assumption.
\* For this reason there is no additional BY ShouldFail . 

EXTENDS Testing

CONSTANT a,b,c

S == {a,b,c}

LEMMA \A x \in {a,b,c} : \E x0 \in {a,b,c} : x # x0 OBVIOUS

LEMMA \A x \in S : \E x0 \in S : x # x0 OBVIOUS

LEMMA \A x \in S : \E x0 \in S : x # x0 BY DEF S

LEMMA \A x \in {a,b,c} : \E x0 \in {b,c,a} : x # x0 BY DEF S

====
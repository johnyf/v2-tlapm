----------------------------- MODULE minilambda -----------------------------

EXTENDS Testing

F(x,Op(_)) == Op(Op(x))

LEMMA \A x : F(x, LAMBDA y : y) = x BY ShouldFail

LEMMA \A x : F(x, LAMBDA y : x) = x BY ShouldFail

LEMMA \A x : F(x, LAMBDA y : y) = x BY DEF F

LEMMA \A x : F(x, LAMBDA y : x) = x BY DEF F


=============================================================================
\* Modification History
\* Last modified Wed Oct 07 16:54:23 CEST 2015 by marty
\* Created Tue Jun 09 10:06:40 CEST 2015 by marty

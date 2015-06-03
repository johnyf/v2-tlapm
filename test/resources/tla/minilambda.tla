----------------------------- MODULE minilambda -----------------------------

F(x,Op(_)) == Op(Op(x))

LEMMA \A x : F(x, LAMBDA y : y) = x BY DEF F

LEMMA \A x : F(x, LAMBDA y : x) = x BY DEF F

=============================================================================
\* Modification History
\* Last modified Wed Jun 10 14:45:42 CEST 2015 by marty
\* Created Tue Jun 09 10:06:40 CEST 2015 by marty

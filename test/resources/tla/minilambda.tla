----------------------------- MODULE minilambda -----------------------------
EXTENDS TLAPS

F(x,Op(_)) == Op(Op(x))

LEMMA \A x : F(x, LAMBDA y : y) = x BY DEF F

LEMMA \A x : F(x, LAMBDA y : x) = x BY DEF F

VARIABLE x,y

LEMMA ENABLED(x'=0 /\ y'=1) BY PTL, IsaT(60)

=============================================================================
\* Modification History
\* Last modified Tue Jun 16 15:16:38 CEST 2015 by marty
\* Created Tue Jun 09 10:06:40 CEST 2015 by marty

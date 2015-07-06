----------------------------- MODULE minilambda -----------------------------
EXTENDS TLAPS

F(x,Op(_)) == Op(Op(x))

LEMMA \A x : F(x, LAMBDA y : y) = x BY DEF F

LEMMA \A x : F(x, LAMBDA y : x) = x BY DEF F

VARIABLE x,y

LEMMA ENABLED(x'=0 /\ y'=1) BY PTL, IsaT(60)

CONSTANT s

Dummy(Op(_),u) == Op(u)

LEMMA Dummy(LAMBDA v: v = v', s) BY Isa DEF Dummy \* this should be provable

=============================================================================
\* Modification History
\* Last modified Tue Jun 30 11:54:40 CEST 2015 by marty
\* Created Tue Jun 09 10:06:40 CEST 2015 by marty

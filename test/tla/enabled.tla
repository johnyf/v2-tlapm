------------------------------ MODULE enabled ------------------------------
EXTENDS TLAPS
VARIABLE x,y

LEMMA ENABLED(x'=0 /\ y'=1) BY PTL, IsaT(60)

CONSTANT s

Dummy(Op(_),u) == Op(u)

LEMMA Dummy(LAMBDA v: v = v', s) BY Isa DEF Dummy \* this should be provable

NextEQ(u) == ENABLED( u # u')
LEMMA \neg NextEQ(0) BY DEF NextEQ

=============================================================================
\* Modification History
\* Last modified Wed Jul 08 10:16:32 CEST 2015 by marty
\* Created Tue Jul 07 13:59:12 CEST 2015 by marty

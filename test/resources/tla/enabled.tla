------------------------------ MODULE enabled ------------------------------
EXTENDS TLAPS
VARIABLE x,y

LEMMA ENABLED(x'=0 /\ y'=1) BY PTL, IsaT(60)

CONSTANT s

Dummy(Op(_),u) == Op(u)

LEMMA Dummy(LAMBDA v: v = v', s) BY Isa DEF Dummy \* this should be provable


=============================================================================
\* Modification History
\* Last modified Tue Jul 07 14:00:00 CEST 2015 by marty
\* Created Tue Jul 07 13:59:12 CEST 2015 by marty

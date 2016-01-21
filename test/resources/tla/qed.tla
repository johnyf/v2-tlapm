-------------------------------- MODULE qed --------------------------------
\* tests the qed proof step
EXTENDS Testing, Naturals


LEMMA \A x : x = x
<1> QED OBVIOUS

AXIOM AX == \A x : (x # x+1)

LEMMA \A y : 2*y # 2*y +1
<1> QED BY AX

=============================================================================
\* Modification History
\* Last modified Thu Jan 21 10:29:35 CET 2016 by marty
\* Created Thu Jan 21 10:26:24 CET 2016 by marty

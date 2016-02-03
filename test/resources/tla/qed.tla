-------------------------------- MODULE qed --------------------------------
\* tests the qed proof step
EXTENDS Testing, Naturals

\* test by obvious
LEMMA \A x : x = x
<1> QED OBVIOUS


\* test by axiom
AXIOM AX == \A x : (x # x+1)

LEMMA \A y : 2*y # 2*y +1
<1> QED BY AX

\* test nested qed
LEMMA 1=2
<1> QED
  <2> QED BY 3=3, ShouldFail

\* test qed by def
A == 1
LEMMA 1=A BY DEF A


LEMMA \A x \in Nat : (x * (x+1)) >= x OBVIOUS
=============================================================================
\* Modification History
\* Last modified Fri Jan 22 16:45:33 CET 2016 by marty
\* Created Thu Jan 21 10:26:24 CET 2016 by marty

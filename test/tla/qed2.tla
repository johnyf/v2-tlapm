-------------------------------- MODULE qed2 --------------------------------
\* this is the same as qed, but has everything in one large proof

\* tests the qed proof step
EXTENDS Testing, Naturals

AXIOM AX == \A x : (x # x+1)

A == 1

LEMMA TRUE
  \* test by obvious
  <1> \A x : x = x
    <2> QED OBVIOUS
  \* test by axiom

  <1> \A y : 2*y # 2*y +1
    <2> QED BY AX

  \* test nested qed
  <1> 1=2
    <2> QED
      <3> TRUE
      <3> QED BY 3=3, ShouldFail

  \* test qed by def
  <1> 1=A BY DEF A

  <1> \A x \in Nat : (x * (x+1)) >= x OBVIOUS
  <1> QED OBVIOUS

=============================================================================
\* Modification History
\* Last modified Thu Feb 11 17:05:46 CET 2016 by marty
\* Created Thu Feb 11 16:54:15 CET 2016 by marty

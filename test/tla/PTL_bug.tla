---- MODULE PTL_bug ----
EXTENDS Naturals, TLAPS
VARIABLE xyz

Init == xyz = 0

Inc(x) == xyz' = xyz+x
Next == \E x \in Nat : Inc(x)
Spec == Init /\ [][Next]_xyz

LEMMA Spec => []Init
<1>1. Init /\ [Next]_xyz => Init'
  <2>. SUFFICES ASSUME Init, Next PROVE Init'  BY DEF Init
  (* the following step illustrates the error and should not be provable *)
  <2>1. ASSUME NEW x \in Nat, Inc(x) PROVE Init'  BY SMT
  <2>. QED BY <2>1 DEF Next
<1>. QED  BY <1>1, PTL DEF Spec
===============================

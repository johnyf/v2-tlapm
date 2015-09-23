----------------------------- MODULE proofsteps -----------------------------
EXTENDS TLAPS, Naturals
\* Tests proof steps and the use statement

D(P,Q) == P /\ Q

THEOREM T == ASSUME NEW P, NEW Q PROVE D(P,Q) => P \/ Q
<1>. USE DEF D
<1>1. P => P \/ Q OBVIOUS 
<1>2. Q => P \/ Q OBVIOUS
<1>3. Q => P \/ Q OBVIOUS
<1> QED BY <1>1, <1>2


\* tests take / witness
CONSTANT P(_, _)
EAP == (\E y : \A x : P(x,y))
AEP == (\A x : \E y : P(x,y))

THEOREM (EAP => AEP)  \* BY Isa DEF EAP, AEP
\* <1> USE DEF EAP, AEP
<1>1 EAP => EAP OBVIOUS
<1> PICK d : (\A x : P(x,d)) => AEP BY DEF EAP
<1>b PICK c : AEP => (\E y : P(c,y)) OBVIOUS
<1> QED BY <1>b, PTL

THEOREM EAP => AEP
<1> SUFFICES ASSUME NEW c, NEW d PROVE (EAP => P(c,d)) /\ (P(c,d) => AEP) OBVIOUS
<1> QED BY SMT


THEOREM \E x \in Nat : x = x*x - 90
<1>1 WITNESS 10 \in Nat
\*<1>2 WITNESS 11 \in Nat
<1> QED OBVIOUS \*BY <1>2

THEOREM \A x \in Nat : \E y \in Nat : (x + y) * (x - y) = 0
\* <1>1 TAKE x \in Nat
<1> QED BY SMT

LEMMA 100 + 200' = 300 BY SMT

LEMMA ASSUME NEW X, X PROVE X
<1>1 ASSUME NEW Y, Y PROVE Y BY <1>1
<1> QED OBVIOUS


=============================================================================
\* Modification History
\* Last modified Tue Sep 22 19:12:05 CEST 2015 by marty
\* Created Mon May 11 11:34:19 CEST 2015 by marty

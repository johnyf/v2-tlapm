----------------------------- MODULE proofsteps -----------------------------
EXTENDS TLAPS, Testing, Naturals
\* Tests proof steps and the use statement

D(P,Q) == P /\ Q

THEOREM T == ASSUME NEW P, NEW Q PROVE D(P,Q) => P \/ Q
<1> USE DEF D
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
<1> PICK d : (\A x : P(x,d)) => AEP BY DEF EAP, AEP
<1>b PICK c : AEP => (\E y : P(c,y)) OBVIOUS
<1> QED BY <1>b, Zenon


Ind(Q(_)) == (Q(0) /\ (\A x \in Nat: Q(x) => Q(x+1))) => (\A x \in Nat : Q(x)) 

LEMMA CancelR == \A x,y \in Nat : x+1 >= y+1 <=> x >= y OBVIOUS  

THEOREM ASSUME Ind(LAMBDA x:(x+1)*(x+1) - (x+1) +1 > x*x - x +1) 
        PROVE ~\E x \in Nat : x*x - x +1 = 0
<1> \A x \in Nat : x*x - x +1 > 0
  <2> 0+0-0+1 > 0 OBVIOUS
  <2> ASSUME NEW c \in Nat PROVE (c+1)*(c+1) - (c+1) +1 >=  c*c - c +1
    <3> SUFFICES c*c + c + 1 >= c*c - c +1 OBVIOUS
    <3> SUFFICES c*c + c  >= c*c - c + 3*c OBVIOUS
    <3> 2*c >= 0 OBVIOUS
    <3> QED BY DEF Ind
  <2> QED BY DEF Ind
<1> QED OBVIOUS

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


\* test scope of use statement
T1 == TRUE
T2 == TRUE

THEOREM T1 /\ T2
<1> USE T1
<1>1 T1
   <2> USE DEF T2
   <2> QED OBVIOUS
<1>2 T2 BY ShouldFail
<1> QED BY <1>1, <1>2



\* test references to different parts of the proof

THEOREM FALSE
<1>1 SUFFICES ASSUME TRUE PROVE FALSE OBVIOUS
<1>2 FALSE BY <1>1, ShouldFail
<1> QED BY <1>2

VARIABLE x
Invariant == TRUE
Next == TRUE
vars == <<x>>

THEOREM ASSUME Invariant, [Next]_vars PROVE Invariant'
<1> QED BY PTL

=============================================================================
\* Modification History
\* Last modified Thu Apr 13 17:14:37 CEST 2017 by marty
\* Created Mon May 11 11:34:19 CEST 2015 by marty

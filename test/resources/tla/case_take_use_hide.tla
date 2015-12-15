------------------------- MODULE case_take_use_hide -------------------------

EXTENDS Naturals, TLAPS

Even(x) == x % 2 = 0
Odd(x) == x % 2 = 1

MODAxiom(x,y) == (x \in Nat /\ y \in Nat) => ((x % 2) + (y % 2)) % 2 = (x+y)%2

THEOREM Mod == \A x,y : MODAxiom(x,y) OMITTED

THEOREM T1 == \A x \in Nat: Even(x+x)
<1>a TAKE x \in Nat
<1>1 CASE Even(x)
  <2> USE <1>1, Mod
  <2> ((x % 2) + (x % 2)) % 2 = (x+x)%2 BY DEF MODAxiom
  <2> DEFINE A == x%2
  <2> HIDE DEF A
  <2> A = 0 => (A + A) % 2 = (0+0) %2 BY SMT DEF Even
  <2> QED BY DEF Even, A
<1>2 CASE Odd(x)
  <2> USE <1>2, Mod
  <2> ((x % 2) + (x % 2)) % 2 = (x+x)%2 BY DEF MODAxiom
  <2> DEFINE A == x%2
  <2> HIDE DEF A
  <2> A = 1 => (A + A) % 2 = (1+1) %2 BY SMT DEF Even
  <2> QED BY DEF Even, Odd, A
<1>3 Even(x) \/ Odd(x) BY DEF Even, Odd 
<1> QED BY <1>1, <1>2, <1>3



=============================================================================
\* Modification History
\* Last modified Tue Dec 15 16:02:23 CET 2015 by marty
\* Created Mon Oct 26 15:01:26 CET 2015 by marty

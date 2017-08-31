------------------------- MODULE case_take_use_hide -------------------------

EXTENDS Naturals, TLAPS, Testing

Even(x) == x % 2 = 0
Odd(x) == x % 2 = 1

THEOREM \A x \in Nat : Even(x+x)
BY Z3 DEF Even

MODAxiom(x,y) == (x \in Nat /\ y \in Nat) => ((x % 2) + (y % 2)) % 2 = (x+y)%2

THEOREM Mod == \A x,y : MODAxiom(x,y) OMITTED

THEOREM T1 == \A x \in Nat: Even(x+x)
<1>a TAKE x \in Nat
<1>1 CASE Even(x)
  <2> USE <1>1, Mod
  <2> ((x % 2) + (x % 2)) % 2 = (x+x)%2  BY DEF MODAxiom
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

\* tests using proof steps as formulas
CONSTANT a,b
THEOREM T2 == TRUE
<1>1 a = b BY ShouldFail
<1>2 <1>1 => (a=b) OBVIOUS
<1> QED OBVIOUS


\* tests USE of an unprovable theorem which then is easily provable
THEOREM FALSE
<1> USE \A x : x = 0, ShouldFail \* the resulting obligation must fail
<1> QED OBVIOUS

THEOREM FALSE
<1>1 1=2 BY ShouldFail
<1> QED
  <2> USE <1>1
  <2> QED OBVIOUS

=============================================================================
\* Modification History
\* Last modified Mon Apr 10 17:45:22 CEST 2017 by marty
\* Created Mon Oct 26 15:01:26 CET 2015 by marty

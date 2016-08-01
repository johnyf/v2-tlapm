----------------------------- MODULE smt2test2 -----------------------------

EXTENDS Naturals, TLAPS

Even(x) == x % 2 = 0

THEOREM \A x \in Nat : Even(x+x)
BY Z3 DEF Even

THEOREM \A x \in Nat : Even(x+x)
BY SMT DEF Even

=============================================================================
\* Modification History
\* Last modified Tue Jun 21 16:31:22 CEST 2016 by marty
\* Created Tue Jun 21 16:30:56 CEST 2016 by marty

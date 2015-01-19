--------------------------- MODULE TestBug121005B ---------------------------

EXTENDS Naturals, TLAPS

P(n) == INSTANCE TestBug121005A 

THEOREM DoubleOne == P(1)!TwoN
BY SMT

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 16:15:42 CET 2015 by marty
\* Created Mon Jan 19 16:15:18 CET 2015 by marty

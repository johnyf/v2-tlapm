--------------------------- MODULE TestBug121005A ---------------------------

EXTENDS Naturals, TLAPS

CONSTANT n
ASSUME InNat == n \in Nat

THEOREM TwoN == 2*n \in Nat
BY InNat

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 16:14:37 CET 2015 by marty
\* Created Mon Jan 19 16:14:27 CET 2015 by marty

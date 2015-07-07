----------------------------- MODULE instanceA -----------------------------

EXTENDS TLAPS

VARIABLES x,y

A == x' # y'

\* THEOREM T == ENABLED A BY DEF A \* not provable at the moment  
T == ENABLED A

T2 == ENABLED (x' # y')

THEOREM TH == ENABLED (x' # y')

F == ENABLED (x' = 0 /\ y' = 1)

=============================================================================
\* Modification History
\* Last modified Fri Feb 27 11:35:09 CET 2015 by marty
\* Created Tue Feb 24 11:52:12 CET 2015 by marty

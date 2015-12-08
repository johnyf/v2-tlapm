--------------------------- MODULE basic_equality ---------------------------

\* theorems T1-3 are provable, because TLA has the constants TRUE and FALSE
\* and an axiom stating TRUE # FALSE. Therefore forcing the domain to one
\* element is inconsistent. 
THEOREM T1 == (\A x, y : x = y) => (\E u,v : u # v) OBVIOUS

CONSTANT a,b
THEOREM T2 == (\A x, y : x = y) => a # b OBVIOUS

THEOREM T3 == (\A x, y : x = y) => FALSE OBVIOUS

=============================================================================
\* Modification History
\* Last modified Fri Nov 20 11:57:26 CET 2015 by marty
\* Created Fri Nov 20 11:01:26 CET 2015 by marty

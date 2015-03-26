---------------------------- MODULE recursiveop ----------------------------

\* This tests recursive operator definitions and proofs with them.
\* The factorial definition is an example from the TLA2 document.

EXTENDS Naturals

RECURSIVE fact(_)
fact(n) == IF n = 0 THEN 1 ELSE n*fact(n-1)

THEOREM fact(3) = 6 
<1>1 fact(3) = 3 * fact(2) BY DEF fact
<1>2 fact(2) = 2 * fact(1) BY DEF fact
<1>3 fact(1) = 1 * fact(0) BY DEF fact
<1>4 fact(0) = 1 BY DEF fact
<1> QED BY <1>1, <1>2, <1>3, <1>4
 
=============================================================================
\* Modification History
\* Last modified Wed Mar 25 15:39:00 CET 2015 by marty
\* Created Wed Mar 25 10:07:40 CET 2015 by marty

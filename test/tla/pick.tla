-------------------------------- MODULE pick --------------------------------
(* Test cases for the pick statement *)

EXTENDS Testing

CONSTANT P(_,_), f(_)
CONSTANT A(_), B(_)

ASSUME Witnesses == A(0) /\ B(0)

\* both T1 and T2 must be provable
THEOREM T1 == \E x:A(x) /\ B(x)
<1>1 PICK y:B(y) /\ A(y) BY Witnesses
<1> QED BY <1>1

THEOREM T2 == \E x:A(x) /\ B(x)
<1>1a \E y:B(y) /\ A(y) BY Witnesses
<1>1b SUFFICES ASSUME NEW c, B(c) /\ A(c) PROVE \E x:A(x) /\ B(x) BY <1>1a
<1> QED BY <1>1b

Goal == FALSE

THEOREM Goal
<1> PICK x : f(x) BY ShouldFail
<1> QED OBVIOUS

THEOREM Goal
<1>1 \E x : f(x) BY ShouldFail
<1>2 SUFFICES ASSUME NEW c, f(c) PROVE Goal  BY <1>1
<1> QED BY <1>2, ShouldFail

THEOREM Goal
<1> ASSUME \E x : f(x), 
           ASSUME NEW c, f(c) 
           PROVE Goal
    PROVE Goal OBVIOUS
<1> QED BY ShouldFail


=============================================================================
\* Modification History
\* Last modified Mon Dec 07 09:21:45 CET 2015 by marty
\* Created Fri Nov 20 10:50:36 CET 2015 by marty

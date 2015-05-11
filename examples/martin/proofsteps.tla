----------------------------- MODULE proofsteps -----------------------------

\* Tests proof steps and the use statement

D(P,Q) == P /\ Q

THEOREM ASSUME NEW P, NEW Q PROVE D(P,Q) => P \/ Q
<1>. USE DEF D
<1>1. P => P \/ Q OBVIOUS
<1>2. Q => P \/ Q OBVIOUS
<1> QED BY <1>1, <1>2


=============================================================================
\* Modification History
\* Last modified Mon May 11 14:10:30 CEST 2015 by marty
\* Created Mon May 11 11:34:19 CEST 2015 by marty

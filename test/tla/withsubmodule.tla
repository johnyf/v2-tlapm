----------------------------- MODULE withsubmodule -----------------------------

---- MODULE sub ----

VARIABLE x

\* D(y) == IF TRUE THEN x' ELSE y \* works fine, but we don't need it at the moment

THEOREM T == x'=x

THEOREM ASSUME T PROVE FALSE

THEOREM A == ASSUME TRUE PROVE TRUE

====================

I == INSTANCE sub WITH x <- 0

IbT == 0' = 0

THEOREM T2 == ASSUME NEW CONSTANT x PROVE x'=x

THEOREM ASSUME IbT PROVE TRUE \* this is not accepted by the toolbox

THEOREM ASSUME I!T PROVE TRUE \* should not be accepted by the toolbox too, but it is. leads to malformed xml export. 

\* THEOREM ASSUME I!T!1 = I!T!1  PROVE TRUE \* should not be true \* not exportable at the moment 


\*THEOREM ASSUME I!D(100) # 0' PROVE TRUE \* exportable

=============================================================================
\* Modification History
\* Last modified Thu Mar 30 15:09:20 CEST 2017 by marty
\* Created Tue Mar 03 10:57:30 CET 2015 by marty

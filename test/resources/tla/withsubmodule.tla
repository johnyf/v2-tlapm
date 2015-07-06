----------------------------- MODULE withsubmodule -----------------------------

---- MODULE sub ----

VARIABLE x

\* D(y) == IF TRUE THEN x' ELSE y \* works fine, but we don't need it at the moment

THEOREM T == x'=x

====================

I == INSTANCE sub WITH x <- 0

IbT == 0' = 0

THEOREM ASSUME I!T PROVE TRUE \* should not be true \* not exportable at the moment 

\* THEOREM ASSUME I!T!1 = I!T!1  PROVE TRUE \* should not be true \* not exportable at the moment 


\*THEOREM ASSUME I!D(100) # 0' PROVE TRUE \* exportable

=============================================================================
\* Modification History
\* Last modified Mon Jun 29 16:24:34 CEST 2015 by marty
\* Created Tue Mar 03 10:57:30 CET 2015 by marty

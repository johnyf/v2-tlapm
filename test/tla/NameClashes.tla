---------------------------- MODULE NameClashes ----------------------------

VARIABLE z \* we cannot declare x here

---- MODULE sub1 ----
 VARIABLE x
 
 COMPARE(u) == u = x 
====

---- MODULE sub2 ----
 VARIABLE x,y
 
 CAP == ENABLED (x' # y') 
====

S == INSTANCE sub1 WITH x <- z
T == INSTANCE sub2 WITH x <- 0, y <- 0

THEOREM T!CAP BY DEF T!CAP

THEOREM T!CAP BY DEF T

=============================================================================
\* Modification History
\* Last modified Wed Apr 01 15:50:43 CEST 2015 by marty
\* Created Tue Mar 31 16:24:25 CEST 2015 by marty

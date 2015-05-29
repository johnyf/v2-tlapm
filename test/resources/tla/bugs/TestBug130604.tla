--------------------------- MODULE TestBug130604 ---------------------------

EXTENDS TLC

THEOREM Print("abc", TRUE)
BY DEF Print


=============================================================================
\* Modification History
\* Last modified Mon Jan 19 10:14:07 CET 2015 by marty
\* Created Mon Jan 19 10:13:29 CET 2015 by marty

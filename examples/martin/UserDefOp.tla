----------------------------- MODULE UserDefOp -----------------------------

ConstantOp == TRUE
Operator(x,y) == x = y

LEMMA Operator(1,1) BY DEF Operator

=============================================================================
\* Modification History
\* Last modified Mon Feb 09 13:49:29 CET 2015 by marty
\* Created Mon Feb 09 10:58:26 CET 2015 by marty

---------------------------- MODULE Quantifiers ----------------------------
EXTENDS TLAPS

CONSTANT P(_,_)

UQ(y) == \A x : P(x,y) 
UQB == \A x:P(x,x)

VARIABLE x

THEOREM UQ(x) = UQB BY Zenon DEF UQ, UQB \* must _not_ be provable

=============================================================================
\* Modification History
\* Last modified Wed Jun 03 14:38:56 CEST 2015 by marty
\* Created Wed Jun 03 14:31:09 CEST 2015 by marty

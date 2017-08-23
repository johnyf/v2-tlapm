------------------------------- MODULE lambda -------------------------------

\* Here we test examples of lambda expressions similar to those from the
\* TLA2 document.

F(Op(_,_)) == Op(1,2)

Id(a,b) == a=b

THEOREM F(Id) = F(LAMBDA x,y : x = y) BY DEF F, Id


------ MODULE M ------

CONSTANT O(_,_)

APPLYO(x,y) == O(x,y)

======================

I == INSTANCE M WITH O <- LAMBDA x,y : x \* clash from importing APPLYO in case of unnamed import
INSTANCE M WITH O <- LAMBDA x,y : y

THEOREM APPLYO(23,42) = 42 BY DEF APPLYO
THEOREM I!APPLYO(23,42) = 23 BY DEF I!APPLYO

=============================================================================
\* Modification History
\* Last modified Wed Aug 23 15:07:42 CEST 2017 by marty
\* Created Wed Mar 25 15:39:39 CET 2015 by marty

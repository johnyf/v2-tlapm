--------------------------- MODULE TestBug130813 ---------------------------

EXTENDS Naturals, TLAPS
 
A == [i \in {0} |-> 0]
B == [A EXCEPT ![0] = @+1, ![0] = @+2] (* recursive application of @: B[0]=(A[0]+1)+2 = 3 *)

Good == B[0] = 3
Bad == B[0] # 3 
 
THEOREM Good BY Isa DEF Good, A, B (* This theorem should be provable *)
THEOREM Bad BY Isa DEF Bad, A, B   (* This theorem should not be provable *)

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 11:38:10 CET 2015 by marty
\* Created Mon Jan 19 11:35:20 CET 2015 by marty

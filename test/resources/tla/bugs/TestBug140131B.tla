--------------------------- MODULE TestBug140131B ---------------------------

(* This produces an error: Module "TestBug140131A" unknown *)
THEOREM FALSE
<1> M == INSTANCE TestBug140131A
<1> QED 

(* This produces a tlapm parsing error
THEOREM FALSE
<1> INSTANCE TestBug140131A
<1> QED 
*)

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 15:21:35 CET 2015 by marty
\* Created Mon Jan 19 15:15:57 CET 2015 by marty

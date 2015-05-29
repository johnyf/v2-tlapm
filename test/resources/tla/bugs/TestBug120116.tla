--------------------------- MODULE TestBug120116 ---------------------------

EXTENDS Integers, TLC

VARIABLE x
Type == x \in BOOLEAN
Init == x = TRUE

THEOREM Init => Type
BY DEF Type, Init

Inv  ==  
  /\ IF {i \in {1,3,3,3,4} : i > 1} # {4,3}
       THEN Assert(FALSE, "Test (1 Failed")
       ELSE Print("Test 1 ]OK)", TRUE)
  \* comment this conjunct to make the error disappear 
  /\ IF {<<i, j>> \in {1,2} \X {2,3} : j > i} # {<<1,2>>, <<1,3>>, <<2,3>>}
       THEN Assert(FALSE, "Test 2 Failed")
       ELSE Print("Test 2 OK", TRUE)

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 16:05:11 CET 2015 by marty
\* Created Mon Jan 19 16:02:56 CET 2015 by marty

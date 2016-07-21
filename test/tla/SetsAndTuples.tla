--------------------------- MODULE SetsAndTuples ---------------------------

VARIABLE x,y

S1 == {1,2,3}
S2 == {3,4,5}
Set == S1 \times S2

TYPEOK == /\ x \in S1 
          /\ y \in S2

\* QTUPLE == \E <<u,v>> \in Set : u = v \*cannot be parsed by tlapm v1

LEMMA x \in { y => y /\ y}

=============================================================================
\* Modification History
\* Last modified Tue Jan 27 16:34:24 CET 2015 by marty
\* Created Tue Jan 27 16:07:55 CET 2015 by marty

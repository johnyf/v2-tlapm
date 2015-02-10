------------------------------- MODULE tuples -------------------------------

VARIABLES x,y,z
LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE <<x,<<y,z>>>> \in (S \cup T) 


=============================================================================
\* Modification History
\* Last modified Tue Feb 10 11:22:09 CET 2015 by marty
\* Created Tue Feb 10 10:41:20 CET 2015 by marty

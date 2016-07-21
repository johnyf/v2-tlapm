------------------------------- MODULE tuples -------------------------------

VARIABLES x,y,z
LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE <<x,<<y,z>>>> \in (S \cup T)
      
LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE \E <<u,v>> \in S : <<u,v>> \in  T \* tlapm v1 does not parse this construct yet
 

=============================================================================
\* Modification History
\* Last modified Wed Feb 18 16:06:11 CET 2015 by marty
\* Created Tue Feb 10 10:41:20 CET 2015 by marty

------------------------------- MODULE tests -------------------------------
EXTENDS Naturals


 THEOREM TRUE OBVIOUS

 THEOREM FALSE OBVIOUS

 THEOREM ASSUME FALSE PROVE FALSE OBVIOUS

 THEOREM 1=1 OBVIOUS

 THEOREM 1=2 OBVIOUS

THEOREM \E x \in {0,1}: x=3 OBVIOUS


\*RECURSIVE fact(_)
\*fact(n) == IF n = 0 THEN 1 ELSE n*fact(n-1)
\*
\*THEOREM fact(3) = 6 OBVIOUS
\*
\*LEMMA \A x : x = x OBVIOUS
\*
\*\*THEOREM INTER == \A p, q : (p => q) => (\E i : (p => i) /\ (i => q) /\ ~(i => p) /\ ~(p => i) ) OBVIOUS

VARIABLES x,y,z
LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE <<x,<<y,z>>>> \in (S \cup T) OBVIOUS
      
LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE \E <<u,v>> \in S : <<u,v>> \in  T \* tlapm v1 does not parse this construct yet
    OBVIOUS


=============================================================================
\* Modification History
\* Last modified Tue Apr 05 16:21:06 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu

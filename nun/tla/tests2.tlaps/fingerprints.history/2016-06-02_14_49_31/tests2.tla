------------------------------- MODULE tests2 -------------------------------

THEOREM TRUE OBVIOUS

THEOREM FALSE OBVIOUS

THEOREM ASSUME FALSE PROVE FALSE OBVIOUS
 
THEOREM ASSUME (FALSE => FALSE) PROVE FALSE OBVIOUS

THEOREM ASSUME NEW a, NEW b PROVE a=b OBVIOUS

THEOREM ASSUME NEW a, NEW b PROVE a # b OBVIOUS

THEOREM ASSUME NEW a, NEW b, a#b, NEW P PROVE P[a] # P[a] OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c, a # b, b # c PROVE a # c OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a \/ c=b OBVIOUS

THEOREM ASSUME NEW a, NEW b, NEW c \in {a,b} PROVE c=a /\ c=b OBVIOUS

\* THEOREM ASSUME NEW a, NEW b, a = TRUE, b = FALSE PROVE \E c \in {a,b} : c \/ FALSE OBVIOUS

\* THEOREM 1=1 OBVIOUS

\* THEOREM 1=2 OBVIOUS

LEMMA \A x : x = x OBVIOUS

LEMMA \A x : x # x OBVIOUS

THEOREM INTER == \A p, q : (p => q) => (\E i : (p => i) /\ (i => q) /\ ~(i => p) /\ ~(p => i) ) OBVIOUS

(*
VARIABLES x,y,z

LEMMA ASSUME NEW CONSTANT S,
             NEW CONSTANT T, 
             <<x,<<y,z>>>> \in S 
      PROVE <<x,<<y,z>>>> \in (S \cup T) OBVIOUS
*)


=============================================================================
\* Modification History
\* Last modified Thu Jun 02 14:49:30 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu
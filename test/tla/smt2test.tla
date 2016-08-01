------------------------------ MODULE smt2test ------------------------------

EXTENDS Naturals, TLAPS

LEMMA
ASSUME NEW CONSTANT N,
       NEW VARIABLE active,
       NEW VARIABLE color,
       NEW VARIABLE tpos,
       NEW VARIABLE tcolor,
       N \in Nat \ {0} 
PROVE  (/\ active \in [0..N - 1 -> BOOLEAN]
        /\ color \in [0..N - 1 -> {"white", "black"}]
        /\ tpos = 0
        /\ tcolor = "black")
       => (/\ active \in [0..N - 1 -> BOOLEAN]
           /\ color \in [0..N - 1 -> {"white", "black"}]
           /\ tpos \in 0..N - 1
           /\ tcolor \in {"white", "black"})
           BY SMT
           
=============================================================================
\* Modification History
\* Last modified Tue Jun 21 16:21:19 CEST 2016 by marty
\* Created Tue Jun 21 16:18:58 CEST 2016 by marty

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
           BY veriT


LEMMA 1+2 = 3 BY veriT

LEMMA \A x \in Nat: (x+1) * x = x*x +x BY Z3

LEMMA \A x \in Nat, y \in Nat: x + y = y +x BY veriT

LEMMA \E x \in Nat : x * x + x = 12 BY veriT

=============================================================================
\* Modification History
\* Last modified Wed Nov 08 15:13:48 CET 2017 by marty
\* Created Tue Jun 21 16:18:58 CEST 2016 by marty

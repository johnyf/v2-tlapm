---------------------------- MODULE assume_prove ----------------------------

EXTENDS TLAPS, Naturals

THEOREM MP == \A p,q : (p /\ (p => q)) => q
<1>6 TAKE C,D
<1>7 (C /\ (C => D)) => D BY Isa
<1> QED BY <1>7

THEOREM MP2 == \A p,q : (p /\ (p => q)) => q
<1>1 SUFFICES ASSUME NEW P, NEW Q, P, P=>Q PROVE Q OBVIOUS
<1> QED BY <1>1

THEOREM MP3 == \A p,q : (p /\ (p => q)) => q
<1>1 ASSUME NEW P, NEW Q, P, P=>Q PROVE Q BY <1>1, Isa
<1>2 ASSUME NEW VARIABLE P, NEW VARIABLE Q, P, P=>Q PROVE Q BY <1>2
<1>3 ASSUME NEW STATE P, NEW STATE Q, P/\Q, P=>Q PROVE Q BY <1>3
<1>4 ASSUME NEW TEMPORAL P, NEW TEMPORAL Q, P, P=>Q PROVE Q BY <1>4
<1>5 PICK A,B : (A /\ A => B) => B OBVIOUS
<1>6 TAKE C,D
<1>7 (C /\ (C => D)) => D BY Isa
<1> QED OBVIOUS

\* sm: this one fails, as it should (e.g. consider p <=> q)
THEOREM INTER == \A p, q : (p => q) => (\E i : (p => i) /\ (i => q) /\ ~(i => p) /\ ~(p => i) )
<1> QED BY Zenon

LEMMA ASSUME NEW P \in {} PROVE P OBVIOUS
LEMMA ASSUME NEW P \in {TRUE} PROVE P OBVIOUS
LEMMA ASSUME NEW P \in {FALSE} PROVE ~P OBVIOUS
LEMMA ASSUME NEW P \in {TRUE,FALSE, 0} PROVE ~P \/ P OBVIOUS


LEMMA 0 \in {TRUE, FALSE} BY Isa, Zenon
LEMMA ~ (0 \in {TRUE, FALSE}) BY Isa, Zenon

LEMMA 0 \/ ~ 0 OBVIOUS
LEMMA 1+1 \/ ~ 2 BY Isa

\* sm: no proof provided, but this one is actually false in ZF set theory
LEMMA \E p : p \in p 

LEMMA ASSUME NEW P, NEW Q, P # Q PROVE P => ~Q OBVIOUS  

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 13:59:40 CEST 2018 by merz
\* Last modified Thu Sep 03 18:31:38 CEST 2015 by marty
\* Created Thu Sep 03 14:16:58 CEST 2015 by marty

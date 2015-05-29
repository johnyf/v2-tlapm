------------------------------ MODULE priming ------------------------------

EXTENDS Naturals

CONSTANT x

Prime[n \in Nat] == IF n = 0 THEN x ELSE Prime[n-1]'
CPrime[n \in Nat] == IF n = 0 THEN x ELSE CPrime[n-1]

LEMMA \A n : Prime[n] = CPrime[n] BY DEF Prime, CPrime



LEMMA PE == \E P : P = [n \in Nat |-> IF n = 0 THEN x ELSE P[n-1]' ]

LEMMA ASSUME PE PROVE Prime[0] = 0
<1>1 Prime BY DEF Prime, PE
<1>2 Prime[0] = 0
<1> QED BY <1>2


Def(f,n) == IF n = 0 THEN x ELSE f[n-1]'

\* LEMMA Def(Prime,0) = x BY DEF Def, Prime
LEMMA START == Def(CPrime,0) = x BY DEF Def, CPrime
LEMMA STEP == \A n \in Nat: Def(CPrime, n) = x => Def(CPrime, n+1) = x BY DEF CPrime, Def


=============================================================================
\* Modification History
\* Last modified Thu Feb 19 12:22:43 CET 2015 by marty
\* Created Thu Feb 19 09:37:41 CET 2015 by marty

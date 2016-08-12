---------------------------- MODULE overbinding ----------------------------
CONSTANT P(_)


f(x) == \E y : (P(y) /\ x)

CONSTANT y


LEMMA f(f(y)) BY DEF f

LEMMA f(f(y)) <=> \E u,v : ( P(u) /\ P(v) /\ y) BY DEF f 

LEMMA f(f(y)) <=> \E u : ( P(u) /\ P(u) /\ y) BY DEF f 


CONSTANT _ + _

g(x(_)) == x(0) \/ (\E z : x(z+1))


D(a,b) == a=b

LEMMA g(LAMBDA x : g(LAMBDA z : 1+z # 0) ) BY DEF g

LEMMA g(LAMBDA u: D(u,1)) BY DEF g,D

=============================================================================
\* Modification History
\* Last modified Thu Mar 31 14:26:34 CEST 2016 by marty
\* Created Wed Mar 30 15:02:21 CEST 2016 by marty

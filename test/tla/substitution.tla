---------------------------- MODULE substitution ----------------------------

EXTENDS Naturals

CONSTANT a,b
VARIABLE x,y

D(u,v) == x+u-v'
App(f(_), value) == f(value) 

LEMMA D(x,x) = x+x-x' BY DEF D

LEMMA App(LAMBDA u : D(u,u),x ) = x+x-x' BY DEF App, D

LEMMA App(LAMBDA u : D(u,u),x ) = D(x,x) BY DEF App

E(u) == \E v : v = u

LEMMA (\E v : E(v)) = (\E v,u : v=u) BY DEF E   \* should work

LEMMA (\E v : E(v)) # (\E v,u : v=v) BY DEF E   \* should fail


=============================================================================
\* Modification History
\* Last modified Fri Apr 20 15:47:26 CEST 2018 by merz
\* Last modified Mon Apr 04 16:42:09 CEST 2016 by marty
\* Created Tue Mar 15 16:06:06 CET 2016 by marty

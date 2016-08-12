---------------------------- MODULE Quantifiers ----------------------------
EXTENDS TLAPS, Testing

CONSTANT P(_,_)

UQ(y) == \A x : P(x,y) 
UQB == \A x:P(x,x)

\* tests overbinding renames of bound variables
THEOREM \A x : UQ(x) <=> UQB BY Zenon, ShouldFail DEF UQ, UQB \* must _not_ be provable

VARIABLE x

\* tests overbinding renames of free variables
THEOREM UQ(x) <=> UQB BY Zenon, ShouldFail DEF UQ, UQB \* must _not_ be provable


D(x_) == \A u \in (\A z : z = x_) : u 

\* these are not parsable by TLAPM v1

LEMMA D( (\A <<u,z>> \in {}: u#z) ) BY DEF D

LEMMA \A <<x_>> \in {<<0>>, <<1>>, <<2>> } : \E y : x_ = y

LEMMA (\A x_ \in TRUE : x_)



\* Test overbinding in domains of bound formal params
E(v) == \A u \in (\A y : P(v,y)) : (\A y : P(v,y))

LEMMA \E y : E(y) BY DEF E


=============================================================================
\* Modification History
\* Last modified Wed Apr 13 15:15:53 CEST 2016 by marty
\* Created Wed Jun 03 14:31:09 CEST 2015 by marty

----------------------------- MODULE nonleibniz -----------------------------

D(x,y) == x -+-> y
E(x) == ENABLED(x)
F(x,y) == x \cdot y

LEMMA ASSUME NEW x, NEW y PROVE D(x,y)
LEMMA ASSUME NEW x, NEW y PROVE E(x)
LEMMA ASSUME NEW x, NEW y PROVE F(x,y)


=============================================================================
\* Modification History
\* Last modified Thu Mar 31 17:28:23 CEST 2016 by marty
\* Created Thu Mar 31 16:53:19 CEST 2016 by marty

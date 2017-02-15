------------------------------ MODULE enabled4 ------------------------------
EXTENDS Naturals, TLC

VARIABLE x

S == {1,2}

---- MODULE Foo ----

VARIABLE v, x

D(fp) == ENABLED (fp # v')
E(fp) == ENABLED (fp = v')

====

VARIABLE a

I(fp) == INSTANCE Foo WITH v <- fp

LEMMA I(a)!D(a)


=============================================================================
\* Modification History
\* Last modified Tue Feb 14 14:32:21 CET 2017 by marty
\* Created Mon Feb 13 18:17:47 CET 2017 by marty

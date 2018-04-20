------------------------------ MODULE enabled4 ------------------------------
EXTENDS Naturals, TLC

VARIABLE x

S == {1,2}

---- MODULE Foo ----

VARIABLE v

D(fp) == ENABLED (fp # v')
E(fp) == ENABLED (fp = v')

====

VARIABLE a

I(fp) == INSTANCE Foo WITH v <- fp

LEMMA I(a)!D(a) BY DEF I!D


=============================================================================
\* Modification History
\* Last modified Fri Apr 20 14:32:02 CEST 2018 by merz
\* Last modified Tue Feb 14 14:32:21 CET 2017 by marty
\* Created Mon Feb 13 18:17:47 CET 2017 by marty

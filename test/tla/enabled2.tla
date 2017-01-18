------------------------------ MODULE enabled2 ------------------------------
EXTENDS TLAPS, Testing

\* some (non-)identities about enabled

---- MODULE Foo ----

VARIABLE bar
D == ENABLED (bar # bar')

====

E0 == ENABLED (0 # 0')
E(z) == ENABLED (z # z')
I == INSTANCE Foo WITH bar <- 0

LEMMA I!D # E0 BY DEF E0, I!D, IsTrue, ShouldSucceed
LEMMA E(0) # I!D BY DEF I!D, E, IsTrue, ShouldSucceed
LEMMA E(0) = E0 BY DEF E, E0, IsTrue, ShouldSucceed \* see def of I!G in Specifying Systems 17.8, p336

LEMMA I!D = E0 BY DEF E0, I!D, IsFalse, ShouldFail \* this is a bug in tlapm v1!!! 
LEMMA E(0) = E0 BY DEF E, E0, IsFalse, ShouldFail \* again this is a bug in tlapm v1


=============================================================================
\* Modification History
\* Last modified Thu Dec 15 15:10:54 CET 2016 by marty
\* Created Tue Jul 07 13:59:12 CEST 2015 by marty

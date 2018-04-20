------------------------- MODULE multi_obligations -------------------------
EXTENDS TLAPS, Naturals, Sequences

LEMMA 1+1 = 3 BY SMT, Isa, Zenon  \* should fail, of course

LEMMA Head(<<1,2,3>>) = 1 /\ Len(<<1,2,3>>) = 3 OBVIOUS

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 15:01:49 CEST 2018 by merz
\* Last modified Tue Jul 05 11:02:32 CEST 2016 by marty
\* Created Sun Jun 26 21:07:35 CEST 2016 by marty

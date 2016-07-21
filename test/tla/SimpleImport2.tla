--------------------------- MODULE SimpleImport2 ---------------------------

VARIABLE x

I == INSTANCE SimpleImport1  WITH x <- 123, y <- 123
J == INSTANCE SimpleImport1a WITH x <- 1234       \* level 0 substitution
K == INSTANCE SimpleImport1a WITH x <- x          \* level 1 substitution
\* L == INSTANCE SimpleImport1a WITH x <- 1'      \* level 2 substitution -- not supposed to parse
\* M == INSTANCE SimpleImport1a WITH x <- [](1=1) \* level 3 substitution  -- not supposed to parse

BOX(z) == []z
PRIME(z) == z'

LEMMA I!EQ1 BY DEF I!EQ1
LEMMA J!EQ1 BY DEF J!EQ1 
LEMMA K!EQ1 BY DEF K!EQ1


=============================================================================
\* Modification History
\* Last modified Tue Jan 27 15:48:27 CET 2015 by marty
\* Created Tue Jan 27 14:34:50 CET 2015 by marty

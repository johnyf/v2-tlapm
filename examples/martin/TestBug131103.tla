--------------------------- MODULE TestBug131103 ---------------------------

VARIABLES x

F(a) == ENABLED a

THEOREM F(x')' = F(x)'
OBVIOUS

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 11:01:31 CET 2015 by marty
\* Created Mon Jan 19 10:57:36 CET 2015 by marty

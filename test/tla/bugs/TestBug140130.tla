--------------------------- MODULE TestBug140130 ---------------------------
VARIABLES A,B,C

THEOREM C
  <3>4a. ASSUME A, B PROVE C BY <3>4a  \* sm 2018-04-20: fails – why is this a bug?
  <3> QED



=============================================================================
\* Modification History
\* Last modified Fri Apr 20 16:13:25 CEST 2018 by merz
\* Last modified Mon Jan 19 14:37:38 CET 2015 by marty
\* Created Mon Jan 19 14:34:22 CET 2015 by marty

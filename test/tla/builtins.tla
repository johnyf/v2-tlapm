------------------------------ MODULE builtins ------------------------------

EXTENDS Naturals

\* this file tries to enumerate all builtins, the statements themselves are nonsensical

LEMMA (TRUE \/ FALSE) /\ TRUE
LEMMA (TRUE = TRUE) => (FALSE # TRUE)
LEMMA [] TRUE /\ ~ [] FALSE
LEMMA ASSUME NEW VARIABLE x PROVE ([][x#x']_x) \/ (<><<x#x'>>_x)
LEMMA ASSUME NEW VARIABLE x PROVE TRUE.FALSE \/ TRUE ~> FALSE \/ (WF_<<x>> (x=x')) \/ (SF_<<x>> (x=x'))
LEMMA {} # {0}
LEMMA {} \in SUBSET {}
LEMMA ASSUME NEW P(_) PROVE P(CHOOSE x : P(x)) => \E x : P(x)
LEMMA ASSUME NEW P(_) PROVE (\A x : P(x)) => (\E x : P(x))
LEMMA ASSUME NEW P(_) PROVE (\AA x : P(x)) => (\EE x : P(x))

\* naturals

LEMMA {1..10} # 1..10




=============================================================================
\* Modification History
\* Last modified Mon Apr 10 15:09:00 CEST 2017 by marty
\* Created Mon Apr 10 13:27:00 CEST 2017 by marty

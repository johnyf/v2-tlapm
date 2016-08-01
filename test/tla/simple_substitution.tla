------------------------ MODULE simple_substitution ------------------------
EXTENDS Sequences, Integers, TLAPS

VARIABLE NewInv

CONSTANT _ ** _

LEMMA ASSUME NEW CONSTANT Data,
       NEW VARIABLE ASndg,
       NEW VARIABLE BRcvd,
       NEW VARIABLE AtoB,
       NEW VARIABLE BtoA,
       NewInv ,
       NEW CONSTANT a \in {0, 1},
       NEW CONSTANT b \in {0, 1},
       NEW CONSTANT f \in Seq({0, 1}),
       ASSUME NEW CONSTANT S,
              NEW CONSTANT a_1 \in S,
              NEW CONSTANT b_1 \in S,
              NEW CONSTANT f_1 \in Seq(S)
       PROVE  f_1 \in a_1 ** b_1
              => (\E i \in 0..Len(f_1) :
                     \A j \in 1..Len(f_1) :
                        f_1[j] = (IF j =< i THEN a_1 ELSE b_1))

PROVE  f \in a ** b
       => (\E i \in 0..Len(f) :
              \A j \in 1..Len(f) :
                 f[j] = (IF j =< i THEN a ELSE b)) BY IsaM("blast")
              
=============================================================================
\* Modification History
\* Last modified Sat Jul 25 15:54:09 CEST 2015 by marty
\* Created Mon Jul 20 11:02:29 CEST 2015 by marty

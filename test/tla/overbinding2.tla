---------------------------- MODULE overbinding2 ----------------------------

EXTENDS Testing

\* create a set for bounded quantifications which TLC can evaluate
S == {1,2}

\* claim that there is a new element x different from the argument u passed
\* then chain x as an argument to the operator v
P(u,v(_)) == \E x \in S: u # x /\ v(x)

\* all these operators can be a v(_) for A. T projects to TRUE, successive
\* definitions apply P to their predecessor 
T(x) == TRUE
A(x) == P(x, T)
B(y) == P(y, A)
C(z) == P(z, B)

\* claim the existence of a y such that one of the previous definitions of y is true
Check == \E y \in S: C(y)

(* Unfolding the definitions must lead to renamings
~(\E y \in {1, 2} :
     \E x \in {1, 2} :
        y # x
        /\ (\E x_1 \in {1, 2} :
               x # x_1 /\ (\E x_2 \in {1, 2} : x_1 # x_2 /\ TRUE)))
*)
LEMMA Check BY DEF Check, S, T, P, A, B, C, ShouldSucceed 
LEMMA ~Check BY DEF Check, S, T, P, A, B, C, ShouldFail 


=============================================================================
\* Modification History
\* Last modified Wed Jul 12 16:20:34 CEST 2017 by marty
\* Created Wed Jul 12 09:56:40 CEST 2017 by marty

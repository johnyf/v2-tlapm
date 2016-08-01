------------------------------ MODULE nunchaku ------------------------------

EXTENDS Naturals, TLAPS

LEMMA TRUE = FALSE OBVIOUS \* unsat
LEMMA TRUE # FALSE OBVIOUS \* valid

LEMMA {} = {{}} OBVIOUS \* unsat
LEMMA {} # {{}} OBVIOUS \* valid

LEMMA \A x : {} = {x} OBVIOUS \* unsat
LEMMA \A x : {} # {x} OBVIOUS \* valid


LEMMA \A x : x > 0 OBVIOUS          \* counter-sat
LEMMA \A x \in Nat : x > 0 OBVIOUS  \* counter-sat
LEMMA \A x \in Nat : x >= 0 OBVIOUS \* valid

S2 == {0,1}
S3 == {0,1,2}
LEMMA \A f \in [S3 -> S2]  : \A x, y \in S3 : x # y => f[x] # f[y] BY DEF S2, S3 \* unsat
LEMMA \A f \in [S3 -> S2]  : \E x, y \in S3 : x # y => f[x] = f[y] BY DEF S2, S3 \* valid


LEMMA \A f \in [S3 -> S3]  : \A x,y \in S3 : x # y => f[x] # f[y] BY DEF S2, S3 \* sat
LEMMA \E f \in [S3 -> S3]  : \A x,y \in S3 : x # y => f[x] # f[y] BY DEF S2, S3 \* valid

LEMMA \E f \in [{0} -> {0}] : f[0] # 0 OBVIOUS \* false
LEMMA \E f \in [{0} -> {0}] : f[0] = 0 BY Z3 \* true
LEMMA \A f \in [{0} -> {0}] : f[0] = 0 OBVIOUS \* true

LEMMA \E f \in [{0} -> {0}] : f[0] = 0
<1> \A f \in [{0} -> {0}] : f[0] = 0 OBVIOUS
<1> QED BY Z3


VARIABLE pc
vars == <<pc>>
TypeOK == pc \in {"s0", "s1"}
\* models a program: s0 : goto s1; s1 : goto s0;
Next == (pc = "s0" /\ pc' = "s1") \/ (pc = "s1" /\ pc' = "s0")

\* LEMMA TypeOK /\ [] [Next]_vars => [] [pc = pc']_pc BY DEF TypeOK, Next, vars \* counter-sat
\* LEMMA TypeOK /\ [] [Next]_vars => [] [pc # pc']_pc BY DEF TypeOK, Next, vars \* needs induction rule


LEMMA TypeOK /\ Next => pc = pc' BY DEF TypeOK, Next, vars \* unsat
LEMMA TypeOK /\ Next => pc # pc' BY DEF TypeOK, Next, vars \* valid


\* similar to Next, but with a possible self-loop on s1
Next2 == (pc = "s0" /\ pc' = "s1") \/ (pc = "s1" /\ pc' \in {"s0", "s1"})
LEMMA TypeOK /\ Next2 => pc = pc' BY DEF TypeOK, Next, vars \* sat
LEMMA TypeOK /\ Next2 => pc # pc' BY DEF TypeOK, Next, vars \* sat


\* restricting the domain
LEMMA \neg  \E x,y,z : x # y /\ x # z /\ y # z OBVIOUS \* counter-sat (contradicts ZFC)


=============================================================================
\* Modification History
\* Last modified Thu Mar 03 16:09:36 CET 2016 by marty
\* Created Wed Mar 02 16:32:32 CET 2016 by marty

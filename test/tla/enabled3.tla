------------------------------ MODULE enabled3 ------------------------------

---- MODULE Foo ----
VARIABLE x

S == {1,2}

D(u) == ENABLED (x' \in S /\ u' \in S /\ x' # u')
Dx == ENABLED (x ' \in S /\ x' \in S /\ x' # x')

\* E(u) == ENABLED (x' \in S /\ u \in S /\ x' # u)

Init == x = 1
Next == x' = x

Spec == Init /\ [][Next]_x

THEOREM T == Spec => []~D(x)

THEOREM Dx <=> D(x)

====

VARIABLE a

I == INSTANCE Foo WITH x <- a

THEOREM T2 == I!Spec => [] I!D(a) BY DEF I!Spec, I!Init, I!Next, I!D


=============================================================================
\* Modification History
\* Last modified Wed Mar 01 17:14:15 CET 2017 by marty
\* Created Fri Jan 06 17:32:56 CET 2017 by marty

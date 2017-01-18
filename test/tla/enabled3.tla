------------------------------ MODULE enabled3 ------------------------------

---- MODULE Foo ----
VARIABLE x

S == {1,2}

D(u) == ENABLED (x' \in S /\ u' \in S /\ x' # u')

\* E(u) == ENABLED (x' \in S /\ u \in S /\ x' # u)

Init == x = 1
Next == x' = x

Spec == Init /\ [][Next]_x

THEOREM T == Spec => []~D(x)

====

VARIABLE a

I == INSTANCE Foo WITH x <- a

THEOREM T2 == I!Spec => [] I!D(a)


=============================================================================
\* Modification History
\* Last modified Tue Jan 10 15:21:41 CET 2017 by marty
\* Created Fri Jan 06 17:32:56 CET 2017 by marty

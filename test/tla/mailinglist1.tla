---------------------------- MODULE mailinglist1 ----------------------------

  THEOREM InvariantHolds == Spec => []Invariant \* All green
  <1>1. Init => Invariant PROOF OMITTED \* For this e-mail
  <1>2. Invariant /\ [Next]_vars => Invariant'
    <2> SUFFICES ASSUME Invariant
                        [Next]_vars
                 PROVE Invariant'
        OBVIOUS
    <2>1. CASE NextComponent BY PTL
    <2>2. CASE NextComponent2 BY PTL
    \* ...
    <2>3. QED BY <2>1, <2>2 (* ... *) DEF Next
  <1>3. QED BY <1>1, <1>2, PTL DEF Spec


=============================================================================
\* Modification History
\* Last modified Fri Oct 16 13:29:30 CEST 2015 by marty
\* Created Fri Oct 16 13:29:21 CEST 2015 by marty

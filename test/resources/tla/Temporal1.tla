----------------------------- MODULE Temporal1 -----------------------------
EXTENDS TLAPS

(* This is an example from the TLA book *)
THEOREM DeductionPrinciple == ASSUME NEW ACTION A, 
                                     NEW ACTION B, 
                                     ASSUME A PROVE B
                              PROVE A => B

THEOREM AddBox == ASSUME NEW STATE P, P PROVE []P
THEOREM RemoveBoxPrime == ASSUME NEW STATE P, []P PROVE P'

VARIABLE x

THEOREM (x=1) => (x=1)' 
<1>1. ASSUME x=1 PROVE (x=1)'
     <2>1.  [](x=1) BY x=1, AddBox
     <2>2. QED BY <2>1, RemoveBoxPrime
<1>2. QED BY <1>1, DeductionPrinciple


VARIABLE F

THEOREM []F => [][]F
<1>1. ASSUME []F PROVE [][]F BY LS4
<1>2. QED BY <1>1


=============================================================================
\* Modification History
\* Last modified Fri Jan 16 11:12:28 CET 2015 by marty
\* Created Thu Jan 15 16:04:57 CET 2015 by marty

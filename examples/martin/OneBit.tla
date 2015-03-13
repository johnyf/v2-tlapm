------------------------------- MODULE OneBit -------------------------------
\* this example is from the tla hyperbook


EXTENDS Naturals, TLAPS

(****************************************************************************

--algorithm OneBit {
      variable x = [i \in {0,1} |-> FALSE] ;
      fair process (P \in {0,1})
       { ncs:- while (TRUE)
                 {     skip;
                   e1: x[self] := TRUE;
                   e2: if (~x[1-self]) { cs:- skip }
                       else { if (self = 0) { goto e2 }
                              else { e3: x[1] := FALSE ;
                                     e4: while (x[0]) { skip } ;
                                         goto e1
                                   } 
                             } ;
                  f:   x[self] := FALSE
                }
       }
}
 
****************************************************************************)
\* BEGIN TRANSLATION
VARIABLES x, pc

vars == << x, pc >>

ProcSet == ({0,1})

Init == (* Global variables *)
        /\ x = [i \in {0,1} |-> FALSE]
        /\ pc = [self \in ProcSet |-> "ncs"]

ncs(self) == /\ pc[self] = "ncs"
             /\ TRUE
             /\ pc' = [pc EXCEPT ![self] = "e1"]
             /\ x' = x

e1(self) == /\ pc[self] = "e1"
            /\ x' = [x EXCEPT ![self] = TRUE]
            /\ pc' = [pc EXCEPT ![self] = "e2"]

e2(self) == /\ pc[self] = "e2"
            /\ IF ~x[1-self]
                  THEN /\ pc' = [pc EXCEPT ![self] = "cs"]
                  ELSE /\ IF self = 0
                             THEN /\ pc' = [pc EXCEPT ![self] = "e2"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "e3"]
            /\ x' = x

cs(self) == /\ pc[self] = "cs"
            /\ TRUE
            /\ pc' = [pc EXCEPT ![self] = "f"]
            /\ x' = x

e3(self) == /\ pc[self] = "e3"
            /\ x' = [x EXCEPT ![1] = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "e4"]

e4(self) == /\ pc[self] = "e4"
            /\ IF x[0]
                  THEN /\ TRUE
                       /\ pc' = [pc EXCEPT ![self] = "e4"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "e1"]
            /\ x' = x

f(self) == /\ pc[self] = "f"
           /\ x' = [x EXCEPT ![self] = FALSE]
           /\ pc' = [pc EXCEPT ![self] = "ncs"]

P(self) == ncs(self) \/ e1(self) \/ e2(self) \/ cs(self) \/ e3(self)
              \/ e4(self) \/ f(self)

Next == (\E self \in {0,1}: P(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in {0,1} : WF_vars((pc[self] \notin {"ncs", "cs"}) /\ P(self))

\* END TRANSLATION

TypeOK == x \in [ProcSet -> {TRUE,FALSE}] /\ pc \in [ProcSet -> {"nc", "e1", "e2", "e3", "e4", "cs", "f"}]

THEOREM TypeOK /\ pc[0] = "e1" => ENABLED <<e1(0)>>_vars BY DEF e1, vars

THEOREM TypeOK /\ pc[0] = "e1" => \E pcp, xp:
                    /\ pc[0] = "e1"
                    /\ xp = [x EXCEPT ![0] = TRUE]
                    /\ pcp = [pc EXCEPT ![0] = "e2"]
\*                    /\ <<xp, pcp>> # <<x, pc>> 
BY DEF TypeOK, ProcSet

THEOREM TypeOK /\ pc[0] = "e2" => ENABLED <<e2(0)>>_vars BY DEF e2, vars

THEOREM TypeOK /\ pc[0] = "e2" /\ ~x[1]
       => \E pcp, xp : /\ pc[0] = "e2"
                    /\ IF ~x[1 - 0]
                         THEN /\ pcp = [pc EXCEPT ![0] = "cs"]
                         ELSE /\ IF 0 = 0
                                   THEN /\ pcp = [pc EXCEPT ![0] = "e2"]
                                   ELSE /\ pcp = [pc EXCEPT ![0] = "e3"]
                    /\ xp = x
                    /\ <<pcp, xp>> # <<pc, x>>
BY DEF TypeOK, ProcSet
(*
<1>. SUFFICES ASSUME TypeOK, pc[0] = "e2"
              PROVE  \E pcp, xp : /\ pc[0] = "e2"
                    /\ IF ~x[1 - 0]
                         THEN /\ pcp = [pc EXCEPT ![0] = "cs"]
                         ELSE /\ IF 0 = 0
                                   THEN /\ pcp = [pc EXCEPT ![0] = "e2"]
                                   ELSE /\ pcp = [pc EXCEPT ![0] = "e3"]
                    /\ xp = x
                 \*   /\ <<pcp, xp>> # <<pc, x>>
  OBVIOUS
<1>. WITNESS IF ~ x[1-0] THEN [pc EXCEPT ![0] = "cs"] ELSE [pc EXCEPT ![0] = "e2"], x
<1>. CASE x[1-0]
  BY DEF TypeOK, ProcSet
<1>. CASE ~ x[1-0]
<1>. QED
  OBVIOUS
*)











=============================================================================
\* Modification History
\* Last modified Thu Mar 12 17:10:47 CET 2015 by marty
\* Created Thu Mar 12 10:13:20 CET 2015 by marty

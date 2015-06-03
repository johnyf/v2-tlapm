---------------------------- MODULE OneBit2Procs ----------------------------
(***************************************************************************)
(* These are TLAPS checked proofs of the two-process one-bit mutual        *)
(* exclusion algorithm of Section 7.3 of the Hyperbook.  See that and the  *)
(* following sections for an explanation of the algorithm and proof.       *)
(* Since TLAPS does not yet reason about ENABLED, that reasoning was       *)
(* simulated here.  I expect that the proofs will require only minor       *)
(* changes to incorporate ENABLED reasoning when it is implemented.  See   *)
(* the discussion before the liveness proof below.                         *)
(***************************************************************************)
EXTENDS Integers, TLAPS
                     
(****************************************************************** 
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
 ******************************************************************)
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

\* For deadlock version
\* 
\*TypeOK == /\ pc \in [{0,1} -> {"ncs", "e1", "e2", "cs", "f"}]
\*          /\ x \in [{0,1} -> BOOLEAN]

TypeOK == /\ pc \in [{0,1} -> {"ncs", "e1", "e2", "e3", "e4", "cs", "f"}]
          /\ x \in [{0,1} -> BOOLEAN]
                 
InCS(i) == pc[i] = "cs"

Mutex == ~(InCS(0) /\ InCS(1))

Inv == /\ TypeOK
       /\ Mutex
       /\ \A i \in {0,1} : InCS(i) \/ (pc[i] = "e2") => x[i]
       /\ pc[0] \notin {"e3", "e4"}

THEOREM Spec => []Mutex
<1>1. Init => Inv
   BY SMT DEF Init, Inv, TypeOK, Mutex, InCS, ProcSet
<1>2. Inv /\ [Next]_vars => Inv'
  BY SMT DEF Inv, TypeOK, Next, Mutex, InCS, ProcSet, P,
              ncs, cs, e1, e2, e3, e4, f, vars
<1>3. Inv => Mutex
  BY DEF Inv 
<1>4. QED       
  BY <1>1, <1>2, <1>3, PTL DEF Spec                                                              
-----------------------------------------------------------------------------
(***************************************************************************)
(*                            LIVENESS                                     *)
(*                                                                         *)
(* Here is the formal statement and proof of the deadlock-freedom of the   *)
(* algorithm.  First come the definition of the invariant LInv used to     *)
(* prove liveness, then some other definitions, then the theorem.          *)
(*                                                                         *)
(* Two methods are used to simulate reasoning about ENABLED; they          *)
(* correspond to two proposed methods for allowing TLAPS to reason about   *)
(* ENABLED.  The fairness assumption for the algorithm is weak fairness of *)
(* a formula defined below to equal WFNext(i), for each process i = 0 or   *)
(* 1.  Hence, the only ENABLED formulas that occur in the proof are        *)
(* ENABLED <<WFNext(i)>> for i = 0 or 1.  (The fairness condition produced *)
(* by the PlusCal translation has the form \A self \in {0, 1} : ...  .     *)
(* Because we can't yet reason about quantified temporal formulas, we      *)
(* assert without proof the obvious fact that this formula is equivalent   *)
(* to the conjunction of the two formulas for self = 0 and 1.)             *)
(*                                                                         *)
(* The first method of reasoning about these ENABLED formulas defines      *)
(* EN(i) to be the semantic translation of ENABLED <<WFNext(i)>>, with all *)
(* definitions expanded.  The second method defines UPNext(i, s) so that   *)
(*                                                                         *)
(*    UPNext(i, [x |-> x', pc |-> pc'])                                    *)
(*                                                                         *)
(* equals WFNext(i).  It corresponds to a formula UP(WFNext(i), s) in a    *)
(* proposed method of reasoning about ENABLED.  For most of the cases,     *)
(* TLAPS can use the first method to prove the necessary ENABLED property  *)
(* in a single step.  In one instance it can't, and the second method      *)
(* would be easier to use in the possible implementations of the two       *)
(* methods.                                                                *)
(***************************************************************************)
LInv == /\ TypeOK
        /\ Mutex
        /\ pc[0] \notin {"e3", "e4"}
        /\ \A i \in {0,1} : x[i] <=> (pc[i] \in {"e2", "e3", "cs", "f"})
           
Trying(i) == pc[i] \in {"e1", "e2", "e3", "e4"}

DeadlockFree == (Trying(0) \/ Trying(1)) ~> (InCS(0) \/ InCS(1))

StarvationFree == \A i \in {0, 1} : Trying(i) ~> InCS(i)

Fairness == \A self \in {0,1} : WF_vars((pc[self] \notin {"ncs", "cs"}) /\ P(self))
-----------------------------------------------------------------------------
THEOREM Spec => DeadlockFree

<1> DEFINE T0 == Trying(0)
           T1 == Trying(1)
           Success == InCS(0) \/ InCS(1)

<1>1. Init /\ [][Next]_vars => []LInv                                                         
  (*************************************************************************)
  (* This is a standard invariance proof.                                  *)
  (*************************************************************************)
  <2>1. Init => LInv
    BY SMT DEF Init, LInv, TypeOK, Mutex, InCS, ProcSet
  <2>2. LInv /\ [Next]_vars => LInv'
    BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
                 ncs, cs, e1, e2, e3, e4, f, vars
  <2>3. QED
    BY <2>1, <2>2, PTL DEF Spec

<1> DEFINE
     WFNext(i) == (pc[i] \notin {"ncs", "cs"}) /\ P(i)
       (********************************************************************)
       (* Fairness == \A i \in {0,1} : WF_vars(WFNext(i))                  *)
       (********************************************************************)
     
     EN(i) == 
       (********************************************************************)
       (* EN(i) is the semantic definition of ENABLED <<WFNext(i)>>_vars   *)
       (********************************************************************)
       \E ssx, sspc :
         /\ pc[i] \notin {"ncs", "cs"}
         /\ \/  /\ pc[i] = "ncs"
                /\ TRUE
                /\ sspc = [pc EXCEPT ![i] = "e1"]
                /\ ssx = x
            \/  /\ pc[i] = "e1"
                /\ ssx = [x EXCEPT ![i] = TRUE]
                /\ sspc = [pc EXCEPT ![i] = "e2"]      
            \/  /\ pc[i] = "e2"
                /\ IF ~x[1-i]
                    THEN /\ sspc = [pc EXCEPT ![i] = "cs"]
                    ELSE /\ IF i = 0
                             THEN /\ sspc = [pc EXCEPT ![i] = "e2"]
                             ELSE /\ sspc = [pc EXCEPT ![i] = "e3"]
                /\ ssx = x      
            \/  /\ pc[i] = "cs"
                /\ TRUE
                /\ sspc = [pc EXCEPT ![i] = "f"]
                /\ ssx = x
               
            \/  /\ pc[i] = "e3"
                /\ ssx = [x EXCEPT ![1] = FALSE]
                /\ sspc = [pc EXCEPT ![i] = "e4"]
               
            \/  /\ pc[i] = "e4"
                /\ IF x[0]
                      THEN /\ TRUE
                           /\ sspc = [pc EXCEPT ![i] = "e4"]
                      ELSE /\ sspc = [pc EXCEPT ![i] = "e1"]
                /\ ssx = x      
            \/  /\ pc[i] = "f"
                /\ ssx = [x EXCEPT ![i] = FALSE]
                /\ sspc = [pc EXCEPT ![i] = "ncs"]       
         /\  <<ssx, sspc>> # <<x, pc>>     
      
 
(***************************************************************************)
(* The following step checks that the definition of EN is correct.         *)
(***************************************************************************)  
<1>EN_DefTest ASSUME NEW i \in {0,1}
               PROVE  <<(pc[i] \notin {"ncs", "cs"}) /\ P(i)>>_<<vars>> 
                           <=> EN(i)!(x', pc')
    <2>1. P(i) <=> EN(i)!(x',pc')!2
      <3>1. ncs(i) <=> EN(i)!(x',pc')!2!1
        BY DEF ncs
      <3>2. e1(i) <=> EN(i)!(x',pc')!2!2
        BY DEF e1
      <3>3. e2(i) <=> EN(i)!(x',pc')!2!3
        BY DEF e2
      <3>4. cs(i) <=> EN(i)!(x',pc')!2!4
        BY DEF cs
      <3>5. e3(i) <=> EN(i)!(x',pc')!2!5
        BY DEF e3
      <3>6. e4(i) <=> EN(i)!(x',pc')!2!6
        BY DEF e4
      <3>7. f(i) <=> EN(i)!(x',pc')!2!7
        BY DEF f
      <3>8. QED
       BY <3>1, <3>2, <3>3, <3>4, <3>5, <3>6, <3>7 DEF  P
    <2>2. QED
      BY <2>1 DEF vars

(***************************************************************************)
(* The following asserts that EN(i) is the desired ENABLED condition.      *)
(***************************************************************************)      
<1>EN /\ (ENABLED <<WFNext(0)>>_vars) = EN(0)
      /\ (ENABLED <<WFNext(1)>>_vars) = EN(1)
  PROOF OMITTED

<1> DEFINE
      UPncs(self, s) == /\ pc[self] = "ncs"
                        /\ TRUE
                        /\ s.pc = [pc EXCEPT ![self] = "e1"]
                        /\ s.x = x
      
      UPe1(self, s) == /\ pc[self] = "e1"
                       /\ s.x = [x EXCEPT ![self] = TRUE]
                       /\ s.pc = [pc EXCEPT ![self] = "e2"]
      
      UPe2(self, s) == /\ pc[self] = "e2"
                       /\ IF ~x[1-self]
                           THEN /\ s.pc = [pc EXCEPT ![self] = "cs"]
                           ELSE /\ IF self = 0
                                    THEN /\ s.pc = [pc EXCEPT ![self] = "e2"]
                                    ELSE /\ s.pc = [pc EXCEPT ![self] = "e3"]
                       /\ s.x = x
      
      UPcs(self, s) == /\ pc[self] = "cs"
                       /\ TRUE
                       /\ s.pc = [pc EXCEPT ![self] = "f"]
                       /\ s.x = x
      
      UPe3(self, s) == /\ pc[self] = "e3"
                       /\ s.x = [x EXCEPT ![1] = FALSE]
                       /\ s.pc = [pc EXCEPT ![self] = "e4"]
      
      UPe4(self, s) == /\ pc[self] = "e4"
                       /\ IF x[0]
                             THEN /\ TRUE
                                  /\ s.pc = [pc EXCEPT ![self] = "e4"]
                             ELSE /\ s.pc = [pc EXCEPT ![self] = "e1"]
                       /\ s.x = x
      
      UPf(self, s) == /\ pc[self] = "f"
                      /\ s.x = [x EXCEPT ![self] = FALSE]
                      /\ s.pc = [pc EXCEPT ![self] = "ncs"]
 
      UPNext(self, s) == UPncs(self, s) \/ UPe1(self, s) \/ UPe2(self, s) \/ UPcs(self, s) \/ UPe3(self, s)
                            \/ UPe4(self, s) \/ UPf(self, s)

(***************************************************************************)
(* The following essentially asserts that UP(<<WFNext(i)>>_vars, s) equals *)
(* UPNext(i, s) /\ (<<s.x, s.pc>> # vars)                                  *)
(***************************************************************************)                                  
<1>UP. /\ (ENABLED <<WFNext(0)>>_vars) = \E s : UPNext(0, s) /\ (<<s.x, s.pc>> # vars)
       /\ (ENABLED <<WFNext(1)>>_vars) = \E s : UPNext(1, s) /\ (<<s.x, s.pc>> # vars)
  PROOF OMITTED

<1>2. Fairness <=> /\ WF_vars(WFNext(0))
                   /\ WF_vars(WFNext(1))
      BY DEF Fairness, WFNext
<1> HIDE DEF WFNext

<1>3. SUFFICES ASSUME []LInv /\ [][Next]_vars /\ []Fairness
               PROVE  DeadlockFree                                           
  BY <1>1, <1>2, PTL DEF Spec, Fairness


<1>4. SUFFICES (T0 \/ T1) /\ []~Success ~> FALSE
    BY PTL DEF DeadlockFree
  (*************************************************************************)
  (* This is a standard temporal proof by contradiction, since             *)
  (* DeadlockFree equals (T0 \/ T1) ~> Success.                            *)
  (*************************************************************************) 
  
<1>5. T0 /\ []~Success ~> FALSE
  <2> SUFFICES ASSUME []~Success 
               PROVE  T0 ~> FALSE
    BY PTL
  <2>1. T0 /\ []~Success ~> [](pc[0] = "e2")                                                
    (***********************************************************************)
    (* Assumption []LInv implies process 0 is never at e3 or e4.           *)
    (* Therefore, by the code and assumption Fairness, we see that if T0   *)
    (* is true and process 0 never reaches cs (which is implied by the     *)
    (* assumption []~Success, then process 0 eventually reaches e2 and     *)
    (* stays there forever.                                                *)
    (***********************************************************************)  
    <3>1. T0 /\ LInv => (pc[0] = "e1") \/ (pc[0] = "e2")
      BY DEF T0, LInv, Trying
    <3>2. (pc[0] = "e1") ~> (pc[0] = "e2")
      <4> DEFINE A == WFNext(0)  Q == (pc[0] = "e2")
                 PP == (pc[0] = "e1") /\ TypeOK
      <4>1. PP /\ [Next]_vars => PP' \/ Q'
        BY DEF Next, P, ncs, e1, e2, cs, e3, e4, f, vars, TypeOK 
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF WFNext, vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f
      <4>3. PP => ENABLED <<A>>_vars
        (*******************************************************************)
        (* Proof using <1>EN                                               *)
        (*******************************************************************)
         \* BY <1>EN, Isa DEF TypeOK
        (*******************************************************************)
        (* Proof using <1>UP.                                              *)
        (*******************************************************************)
        <5> SUFFICES ASSUME PP
                     PROVE  \E s : UPNext(0, s) /\ (<<s.x, s.pc>> # vars)
          BY <1>UP
        <5> WITNESS [x  |-> [x EXCEPT ![0] = TRUE],
                     pc |-> [pc EXCEPT ![0] = "e2"]]
        <5> QED
          BY SMT DEF vars  \* Only SMT proves it
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv, Fairness
    <3>3. (pc[0] = "e2") /\ []~Success => [](pc[0] = "e2")
      <4>1. (pc[0] = "e2") /\ LInv /\ ~Success /\ ~Success' /\ [Next]_vars => (pc[0] = "e2")'
          BY  DEF InCS, LInv, vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
       <4>2. QED
        BY <4>1, <1>3, PTL
    <3>4. QED
      BY <3>1, <3>2, <3>3, <1>3, PTL
    
  <2>2. [](pc[0] = "e2") ~> []((pc[0] = "e2") /\ ~x[1])
    <3>1. SUFFICES ASSUME [](pc[0] = "e2")
                   PROVE  <>[]((pc[0] = "e2") /\ ~x[1])                        
      BY PTL
    <3>2. TRUE ~> ([](pc[1] = "ncs") \/ [](pc[1] = "e4"))                                
      (*********************************************************************)
      (* The code and assumption Fairness imply that if process 1 never    *)
      (* reaches cs (by the assumption []~Success), then eventually it     *)
      (* must reach and remain forever either at ncs or e4.                *)
      (*********************************************************************) 
      <4>1.[] x[0]
        <5> LInv /\ (pc[0] = "e2") => x[0]
             BY DEF LInv, TypeOK
        <5> QED
          BY <1>3, <3>1, PTL
      <4> DEFINE A ==  WFNext(1)
      <4> USE DEF WFNext
      <4>2. (pc[1] = "ncs") ~> [](pc[1] = "ncs") \/ (pc[1] = "e1")
        <5>  LInv /\ [Next]_vars /\ (pc[1] = "ncs") /\ ~(pc[1] = "ncs")' => (pc[1] = "e1")'
          BY DEF LInv, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f, vars
        <5> QED
          BY <1>3, PTL
      <4>3. (pc[1] = "e1") ~> (pc[1] = "e2")
        <5> DEFINE PP == (pc[1] = "e1") /\ TypeOK
                   Q  == pc[1] = "e2"
        <5>1. PP /\ [Next]_vars => PP' \/ Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>2. PP /\ <<A>>_vars => Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>3. PP => ENABLED <<A>>_vars
          BY <1>EN, Isa DEF TypeOK
        <5>4. QED
          BY <1>2, <1>3, <5>1, <5>2, <5>3, PTL DEF LInv               
      <4>4. (pc[1] = "e2") ~> (pc[1] = "e3")
        <5> DEFINE PP == (pc[1] = "e2") /\ TypeOK /\ x[0] 
                   Q  == pc[1] = "e3"
        <5>1. x[0]' /\ PP /\ [Next]_vars => PP' \/ Q'
          BY DEF vars, TypeOK,  Next, P, ncs, e1, e2, cs, e3, e4, f
        <5>2. PP /\ <<A>>_vars => Q'
          BY DEF vars, TypeOK, Next,  P, ncs, e1, e2, cs, e3, e4, f 
        <5>3. PP => ENABLED <<A>>_vars
          BY <1>EN, Isa DEF TypeOK
        <5>4. QED
          BY <1>2, <1>3, <4>1, <5>1, <5>2, <5>3, PTL DEF LInv               
      <4>5. (pc[1] = "e3") ~> (pc[1] = "e4")
        <5> DEFINE PP == (pc[1] = "e3") /\ TypeOK
                   Q  == pc[1] = "e4"
        <5>1. PP /\ [Next]_vars => PP' \/ Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>2. PP /\ <<A>>_vars => Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>3. PP => ENABLED <<A>>_vars
          BY <1>EN, Isa DEF TypeOK
        <5>4. QED
          BY <1>2, <1>3, <5>1, <5>2, <5>3, PTL DEF LInv               
      <4>6. (pc[1] = "e4") => [](pc[1] = "e4")
        <5>1. (pc[1] = "e4") /\ x[0] /\ TypeOK /\ [Next]_vars => (pc[1] = "e4")'
           BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>2. QED
          BY <5>1, <1>3, <4>1, PTL DEF LInv        
      <4>7. (pc[1] = "f") ~> (pc[1] = "ncs")
        <5> DEFINE PP == (pc[1] = "f") /\ TypeOK
                   Q  == pc[1] = "ncs"
        <5>1. PP /\ [Next]_vars => PP' \/ Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>2. PP /\ <<A>>_vars => Q'
          BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
        <5>3. PP => ENABLED <<A>>_vars
          BY <1>EN, Isa DEF TypeOK
        <5>4. QED
          BY <1>2, <1>3, <5>1, <5>2, <5>3, PTL DEF LInv               
      <4>8. LInv /\ ~Success =>
              (pc[1] = "ncs") \/ (pc[1] = "e1") \/ (pc[1] = "e2") 
                \/ (pc[1] = "e3") \/ (pc[1] = "e4") \/ (pc[1] = "f")
        BY DEF LInv, TypeOK, Success, InCS
      <4>9. QED
        BY <4>2, <4>3, <4>4, <4>5, <4>6, <4>7, <4>8, <1>3, PTL 
    <3>3. [](pc[1] = "ncs") => []~x[1]
      (*********************************************************************)
      (* []LInv implies x[1] equals FALSE when process 1 is at ncs.        *)
      (*********************************************************************)
      <4>1. LInv /\ (pc[1] = "ncs") => ~x[1]
        BY DEF LInv
      <4>2. QED
        BY <4>1, <1>3, PTL
    <3>4. [](pc[1] = "e4") => []~x[1]    
      (*********************************************************************)
      (* []LInv implies x[1] equals FALSE when process 1 is at e4.        *)
      (*********************************************************************)
      <4>1. LInv /\ (pc[1] = "e4") => ~x[1]
        BY DEF LInv
      <4>2. QED
        BY <4>1, <1>3, PTL
    <3>5. QED                                                                
       BY <3>1, <3>2, <3>3, <3>4, PTL
  <2>3. []((pc[0] = "e2") /\ ~x[1]) ~> FALSE                                  
    (***********************************************************************)
    (* The code and Fairness imply that (pc[0] = "e2") /\ []~x[1] leads to *)
    (* process 0 reaching cs, contradicting []~Success.                    *)
    (***********************************************************************)
    <3>1. ((pc[0] = "e2") /\ []~x[1]) ~> (pc[0] = "cs")
      <4> DEFINE PP == pc[0] = "e2"
                 Q  == pc[0] = "cs"
                 AA == (pc[0] \notin {"ncs", "cs"}) /\ P(0)
      <4>1. SUFFICES ASSUME []LInv /\ []~x[1]
                     PROVE  PP ~> Q                                           
        BY <1>3, PTL
      <4>2. LInv /\ ~x[1] (* /\ ~x[1]'*) => (PP /\ [Next]_vars => (PP' \/ Q'))
        BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
               ncs, cs, e1, e2, e3, e4, f, vars
      <4>3. LInv /\ ~x[1] => (PP /\ <<Next /\ AA>>_vars => Q')
        BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
               ncs, cs, e1, e2, e3, e4, f, vars
      <4>4. PP /\ ~x[1] => ENABLED <<AA>>_vars
      (* Here's the proof with EN 
        <5> SUFFICES ASSUME PP /\ ~x[1-0]
                     PROVE  EN(0)
          BY <1>EN, Isa DEF WFNext \* Only Isabelle proves this
        <5> WITNESS x, [pc EXCEPT ![0] = "cs"]
        <5> QED
          BY SMT \* Only SMT proves this   
      *)    
      (* Here's the proof with UP *)
        <5> SUFFICES ASSUME PP /\ ~x[1-0]
                     PROVE  \E s : UPNext(0, s) /\ (<<s.x, s.pc>> # vars)
          BY <1>UP, Isa DEF WFNext  \* Only Isabelle proves this
        <5> WITNESS [x |-> x, pc |-> [pc EXCEPT ![0] = "cs"]]
        <5> QED
          BY SMT DEF vars \* Only SMT proves this          
      <4>5. QED                                                               
        BY <1>3, <4>1, <4>2, <4>3, <4>4, <1>2, PTL DEF WFNext
    <3>2. (pc[0] = "cs") /\ ~Success => FALSE
      BY DEF Success, InCS
    <3>3. QED                                                                 
      BY <3>1, <3>2, PTL      
  <2>4. QED                                                                   
    BY <2>1, <2>2, <2>3, PTL
  
<1>6. T1 /\ []~Success ~> FALSE
  <2> SUFFICES ASSUME []~Success
               PROVE  T1 ~> FALSE
    BY PTL
  <2>1. T1 => []T1                                                            
    (***********************************************************************)
    (* From the code, we see that if T1 is true and process 1 never        *)
    (* reaches cs (which is implied by the assumption []~Success), then T1 *)
    (* remains forever true.                                               *)
    (***********************************************************************) 
    <3>1. T1 /\ ~Success' /\ TypeOK /\ [Next]_vars => T1'
      BY DEF Trying, InCS, vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f
    <3>2. QED
      BY <3>1, <1>3, PTL DEF LInv
  <2>2. []T1  ~>  (T0 \/ [](T1 /\ ~T0))
    BY PTL
    (***********************************************************************)
    (* By the tautologies F ~> (G \/ (F /\ []~G)) and                      *)
    (* []F /\ []G <=> [](F /\ G).                                          *)
    (***********************************************************************)
  <2>3. [](T1 /\ ~T0)  ~>  [](T1 /\ ~x[0])                                    
    (***********************************************************************)
    (* By the code and Fairness, []~T0 and []~Success imply that           *)
    (* eventually process 0 is always at ncs, which implies that x[0]      *)
    (* always equals FALSE.                                                *)
    (***********************************************************************)
    <3>1. TypeOK /\ ~T0 /\ ~Success => (pc[0] = "ncs") \/ (pc[0] = "f")
      BY DEF TypeOK, Trying, InCS
    <3>2. (pc[0] = "f") ~> (pc[0] = "ncs")
      <4> DEFINE PP == (pc[0] = "f") /\ TypeOK
                 Q  == pc[0] = "ncs"
                 A == WFNext(0)
      <4> USE DEF WFNext
      <4>1. PP /\ [Next]_vars => PP' \/ Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>3. PP => ENABLED <<A>>_vars
        BY <1>EN, Isa DEF TypeOK
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv                   
    <3>3. TypeOK /\ ~T0' /\ (pc[0] = "ncs") /\ [Next]_vars => (pc[0] = "ncs")'
      BY SMT DEF Trying, vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f
    <3>4. LInv /\ (pc[0] = "ncs") => ~x[0]
      BY DEF LInv, TypeOK
    <3>5. QED
      BY <3>1, <3>2, <3>3, <3>4, <1>3, PTL DEF LInv
  <2>4. [](T1 /\ ~x[0]) ~> FALSE                                              
    (***********************************************************************)
    (* The code, Fairness, and []~x[0] imply that process 1 eventually     *)
    (* reaches e2.  Assumption Fairness and []~x[0] then imply that        *)
    (* process 1 reaches cs, contradicting the assumption []~Success.      *)
    (***********************************************************************) 
    <3>1. T1 => 
           (pc[1] = "e1") \/ (pc[1] = "e2") \/ (pc[1] = "e3") \/ (pc[1] = "e4")  
      BY DEF Trying
    <3> DEFINE  A == WFNext(1)
    <3> USE DEF WFNext
    <3>2. (pc[1] = "e1") ~> (pc[1] = "e2")
      (*********************************************************************)
      (* Copied from same step <1>5, <2>2/<3>2/<4>3 above.                 *)
      (*********************************************************************)
      <4> DEFINE PP == (pc[1] = "e1") /\ TypeOK
                 Q  == pc[1] = "e2"                
      <4>1. PP /\ [Next]_vars => PP' \/ Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>3. PP => ENABLED <<A>>_vars
        BY <1>EN, Isa DEF TypeOK
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv               
    <3>3. (pc[1] = "e3") ~> (pc[1] = "e4")
      (*********************************************************************)
      (* Copied from same step <1>5/<2>2/<3>2/<4>5 above.                  *)
      (*********************************************************************)
      <4> DEFINE PP == (pc[1] = "e3") /\ TypeOK
                 Q  == pc[1] = "e4"
      <4>1. PP /\ [Next]_vars => PP' \/ Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF vars, TypeOK, Next, P, ncs, e1, e2, cs, e3, e4, f 
      <4>3. PP => ENABLED <<A>>_vars
        BY <1>EN, Isa DEF TypeOK
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv               
    <3>4. []~x[0] /\ (pc[1] = "e2") ~> (pc[1] = "cs")
      <4> DEFINE PP == (pc[1] = "e2") /\ TypeOK /\ ~x[0] 
                 Q  == pc[1] = "cs"
      <4>1. ~x[0]' /\ PP /\ [Next]_vars => PP' \/ Q'
        BY DEF vars, TypeOK,  Next, P, ncs, e1, e2, cs, e3, e4, f
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF vars, TypeOK, Next,  P, ncs, e1, e2, cs, e3, e4, f 
      <4>3. PP => ENABLED <<A>>_vars
        BY <1>EN, Isa DEF TypeOK
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv
    <3>5. []~x[0] /\ (pc[1] = "e4") ~> (pc[1] = "e1")
      <4> DEFINE PP == (pc[1] = "e4") /\ TypeOK /\ ~x[0] 
                 Q  == pc[1] = "e1"
      <4>1. ~x[0]' /\ PP /\ [Next]_vars => PP' \/ Q'
        BY DEF vars, TypeOK,  Next, P, ncs, e1, e2, cs, e3, e4, f
      <4>2. PP /\ <<A>>_vars => Q'
        BY DEF vars, TypeOK, Next,  P, ncs, e1, e2, cs, e3, e4, f 
      <4>3. PP => ENABLED <<A>>_vars
        BY <1>EN, Isa DEF TypeOK
      <4>4. QED
        BY <1>2, <1>3, <4>1, <4>2, <4>3, PTL DEF LInv              
    <3> QED 
      BY <3>1, <3>2, <3>3, <3>4, <3>5, PTL DEF InCS 
  <2>5. QED 
    BY <2>1, <2>2, <2>3, <2>4, <1>5, PTL     
    (***********************************************************************)
    (* BY <2>1 - <2>4 and Leads-To Induction.                              *)
    (***********************************************************************) 

<1>7. QED        
  BY <1>4, <1>5, <1>6, PTL
=======================================================================================
(***************************************************************************)
(* The following is an informal version of an earlier version of the proof *)
(* above, which assumes that one is reasoning about a behavior satisfying  *)
(* Spec.                                                                   *)
(***************************************************************************)

THEOREM DeadlockFree

<1> DEFINE T0 == Trying(0)
           T1 == Trying(1)
           Success == InCS(0) \/ InCS(1)

<1>2. SUFFICES ASSUME []~Success
               PROVE  (T0 \/ T1) ~> FALSE                                     PROOF
  (*************************************************************************)
  (* By standard temporal reasoning, since DeadlockFree equals             *)
  (* (T0 \/ T1) ~> Success.                                                *)
  (*************************************************************************) OMITTED
  
<1>3. T0 ~> FALSE
  <2>1. T0 ~> [](pc[0] = "e2")                                                PROOF
    (***********************************************************************)
    (* Process 0 is never at e3 or e4.  Therefore, from the code and       *)
    (* fairness, we see that if T0 is true and process 0 never reaches cs  *)
    (* (which is implied by the assumption []~Success), then process 0     *)
    (* eventually reaches e2 and stays there forever.                      *)
    (***********************************************************************) OMITTED
  <2>2. [](pc[0] = "e2") ~> []((pc[0] = "e2") /\ ~x[1])    
    <3>1. SUFFICES ASSUME []T0
                   PROVE  TRUE ~> []~x[1]                                     PROOF
      (*********************************************************************)
      (* By the []~> Rule.                                                 *)
      (*********************************************************************) OMITTED
    <3>2. TRUE  ~>  ([](pc[1] = "ncs") \/ []T1)                               PROOF
      (*********************************************************************)
      (* The code and fairness imply that if process 1 never reaches cs    *)
      (* (by the assumption []~Success), then eventually it must either    *)
      (* reach and remain forever at ncs, or T1 must become true and       *)
      (* remain true forever.                                              *)
      (*********************************************************************) OMITTED
    <3>3. [](pc[1] = "ncs") => []~x[1]                                        PROOF       
      (*********************************************************************)
      (* x[1] equals \FALSE when process 1 is at ncs.                      *)
      (*********************************************************************) OMITTED
    <3>4. []T1 ~> []~x[1]                                                     PROOF
      (*********************************************************************)
      (* T0 implies x[0], and the code, fairness, and []~Success imply     *)
      (* []x[0] leads to process 1 reaching and remaining forever at e4    *)
      (* with x[1] equal to FALSE.                                         *)
      (*********************************************************************) OMITTED
    <3>5. QED                                                                \* PROOF
      BY <3>1, <3>2, <3>3, <3>4, PTL
      (*********************************************************************)
      (* BY <3>1 - <3>4 and Leads-To Induction.                            *)
      (*********************************************************************) \* OMITTED
  <2>3. []((pc[0] = "e2") /\ ~x[1]) ~> FALSE                                  PROOF 
    (***********************************************************************)
    (* Fairness implies that (pc[0] = "e2") and []~x[1] leads to process 0 *)
    (* reaching cs, contradicting []~Success.                              *)
    (***********************************************************************) OMITTED
  <2>4. QED                                                                   PROOF
    (***********************************************************************)
    (* BY <2>1 - <2>3 and Leads-To Induction.                              *)
    (***********************************************************************) OMITTED
  
<1>4. T1 ~> FALSE
  <2>1. T1 => []T1                                                            PROOF
    (***********************************************************************)
    (* From the code, we see that if T1 is true and process 1 never        *)
    (* reaches cs (which is implied by the assumption []~Success), then T1 *)
    (* remains forever true.                                               *)
    (***********************************************************************) OMITTED
  <2>2. []T1  ~>  (T0 \/ [](T1 /\ ~T0))                                       PROOF
    (***********************************************************************)
    (* By the tautologies F ~> (G \/ (F /\ []~G)) and                      *)
    (* []F /\ []G <=> [](F /\ G).                                          *)
    (***********************************************************************) OMITTED
  <2>3. [](T1 /\ ~T0)  ~>  [](T1 /\ ~x[0])                                    PROOF
    (***********************************************************************)
    (* By the code and fairness, []~T0 implies that eventually process 0   *)
    (* is always at ncs, which implies that x[0] always equals FALSE.      *)
    (***********************************************************************) OMITTED
  <2>4. [](T1 /\ ~x[0]) ~> FALSE                                              PROOF
    (***********************************************************************)
    (* The code, fairness, and []~x[0] imply that process 1 eventually     *)
    (* reaches e2.  Fairness and []~x[0] then imply that process 1 reaches *)
    (* cs, contradicting the assumption []~Success.                        *)
    (***********************************************************************) OMITTED
  <2>5. QED                                                                   PROOF
    BY <2>1, <2>2, <2>3, <2>4, <1>3, PTL
    (***********************************************************************)
    (* BY <2>1 - <2>4 and Leads-To Induction.                              *)
    (***********************************************************************) \*OMITTED

<1>5. QED                                                                     PROOF
  (*************************************************************************)
  (* By <1>1 - <1>3 and a trivial application of Leads-To Induction.       *)
  (*************************************************************************) OMITTED

\* Modification History
\* Last modified Sun Mar 15 16:07:31 PDT 2015 by lamport
\* Created Tue Apr 17 15:33:34 PDT 2012 by lamport


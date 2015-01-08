---------------------------- MODULE OneBit2Procs ----------------------------
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
 
\* -- algorithm OneBit {
\*   variable x = [i \in {0,1} |-> FALSE] ;
\*   process (P \in {0,1})
\*    { ncs: while (TRUE)
\*             {     skip;
\*               e1: x[self] := TRUE;
\*               e2: if (self = 0) {await ~x[1]}
\*                   else { if (x[0]) { e3: x[1] := FALSE ;
\*                                      e4: await ~x[0] ;
\*                                          goto e1
\*                                    }
\*                         } ;
\*               cs:  skip ;
\*               f:   x[self] := FALSE
\*             }
\*    }
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
  (*************************************************************************)
  (* BY <1>1 -- <1>3 and simple TLA reasoning.                             *)
  (*************************************************************************) 
-----------------------------------------------------------------------------

LInv == /\ TypeOK
        /\ Mutex
        /\ pc[0] \notin {"e3", "e4"}
        /\ \A i \in {0,1} : x[i] <=> (pc[i] \in {"e2", "e3", "cs", "f"})
           
Trying(i) == pc[i] \in {"e1", "e2", "e3", "e4"}

DeadlockFree == (Trying(0) \/ Trying(1)) ~> (InCS(0) \/ InCS(1))

StarvationFree == \A i \in {0, 1} : Trying(i) ~> InCS(i)

Fairness == \A self \in {0,1} : WF_vars((pc[self] \notin {"ncs", "cs"}) /\ P(self))


-----------------------------------------------------------------------------

\*pcBar == [i \in {0, 1} |->
\*           IF pc[i] \in {"ncs",  "f"} THEN "r"
\*                                      ELSE pc[i]]
\*
\*Proto == INSTANCE OneBitProtocol WITH x <- x, pc <- pcBar
\*-----------------------------------------------------------------------------
\*A == INSTANCE OneBit2ProcsA
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

<1>2. Fairness <=> /\ WF_vars((pc[0] \notin {"ncs", "cs"}) /\ P(0))
                   /\ WF_vars((pc[1] \notin {"ncs", "cs"}) /\ P(1))
      BY DEF Fairness
      
<1>3. SUFFICES ASSUME []LInv /\ [][Next]_vars /\ []Fairness 
               PROVE  DeadlockFree                                           
  BY <1>1, <1>2, PTL DEF Spec, Fairness


<1>4. SUFFICES  (T0 \/ T1) /\ []~Success ~> FALSE 
    BY PTL DEF DeadlockFree
  (*************************************************************************)
  (* This is a standard temporal proof by contradiction, since             *)
  (* DeadlockFree equals (T0 \/ T1) ~> Success.                            *)
  (*************************************************************************) 
  
<1>5. T0 /\ []~Success ~> FALSE
  <2>1. T0 ~> [](pc[0] = "e2")                                                PROOF
    (***********************************************************************)
    (* Assumption []LInv implies process 0 is never at e3 or e4.           *)
    (* Therefore, by the code and assumption Fairness, we see that if T0   *)
    (* is true and process 0 never reaches cs (which is implied by the     *)
    (* assumption []~Success, then process 0 eventually reaches e2 and     *)
    (* stays there forever.                                                *)
    (***********************************************************************) OMITTED 
  <2>2. [](pc[0] = "e2") ~> []((pc[0] = "e2") /\ ~x[1])
    <3>1. SUFFICES ASSUME [](pc[0] = "e2")
                   PROVE  <>[]((pc[0] = "e2") /\ ~x[1])                         PROOF
      BY PTL
      (*********************************************************************)
      (* By the []~ Rule.                                                  *)
      (*********************************************************************) \* OMITTED
    <3>2. TRUE ~> ([](pc[1] = "ncs") \/ []T1)                                 PROOF
      (*********************************************************************)
      (* The code and assumption Fairness imply that if process 1 never    *)
      (* reaches cs (by the assumption []~Success), then eventually it     *)
      (* must either reach and remain forever at ncs, or T1 must become    *)
      (* true and remain true forever.                                     *)
      (*********************************************************************) OMITTED
    <3>3. [](pc[1] = "ncs") => []~x[1]                                        PROOF
      (*********************************************************************)
      (* []LInv implies x[1] equals FALSE when process 1 is at ncs.        *)
      (*********************************************************************) OMITTED
    <3>4. []T1 ~> []~x[1]                                                     PROOF
      (*********************************************************************)
      (* pc[0] = e2" implies x[0], so the step <3>1 assumption implies     *)
      (* []x[0].  The code, Fairness, []~Success, and []x[0] imply that T1 *)
      (* leads to process 1 reaching and remaining forever at e4 with x[1] *)
      (* equal to FALSE.                                                   *)
      (*********************************************************************) OMITTED
    <3>5. QED                                                                 PROOF
       BY <3>1, <3>2, <3>3, <3>4, PTL
      (*********************************************************************)
      (* By <3>1-<3>4 and Leads-To Induction.                              *)
      (*********************************************************************)\* OMITTED
\*    <3>1. T0 /\ LInv => (pc[0] = "e1") \/ (pc[0] = "e2")
\*      BY SMT DEF T0, Trying, LInv 
\*    <3>2. (pc[0] = "e1") ~> (pc[0] = "e2")
\*      <4> DEFINE PP == LInv /\ (pc[0] = "e1")
\*                 Q  == pc[0] = "e2"
\*                 AA == (pc[0] # "ncs") /\ P(0)
\*      <4>1. PP /\ [Next]_vars => (PP' \/ Q')
\*        BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*               ncs, cs, e1, e2, e3, e4, f, vars
\*      <4>2. PP /\ <<Next /\ AA>>_vars => Q'
\*        BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
\*               ncs, cs, e1, e2, e3, e4, f, vars
\*      <4>3. PP => ENABLED <<AA>>_vars
\*        PROOF OMITTED
\*      <4>4. QED                                                             PROOF
\*        (*****************************************************************)
\*        (* BY <4>1 - <4>3, <1>4, and rule WF1.                           *)
\*        (*****************************************************************) OMITTED
\*                   
\*    <3>3. (pc[0] = "e2") => [](pc[0] = "e2")
\*      <4>1. (pc[0] = "e2") /\ ~Success /\ ~Success' /\ LInv /\ [Next]_vars  
\*               => (pc[0] = "e2")'
\*         BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, Success
\*      <4>2. QED                                                               PROOF
\*        (*******************************************************************)
\*        (* BY <4>1, <1>4, and rule INV1.                                   *)
\*        (*******************************************************************) OMITTED
\*    <3>4. QED                                                                 PROOF
\*      (*********************************************************************)
\*      (* By <3>1 - <3>3 and Leads-To Induction.                            *)
\*      (*********************************************************************) OMITTED
\*  <2>2. [](pc[0] = "e2") ~> []((pc[0] = "e2") /\ ~x[1])    
\*    <3>1. SUFFICES ASSUME [](pc[0] = "e2")
\*                   PROVE  TRUE ~> []~x[1]                                     PROOF
\*      (*********************************************************************)
\*      (* By the []~> Rule.                                                 *)
\*      (*********************************************************************) OMITTED
\*    <3>2. TRUE  ~>  ([](pc[1] = "ncs") \/ []T1)                               PROOF
\*      (*********************************************************************)
\*      (* The code and fairness imply that if process 1 never reaches cs,   *)
\*      (* then eventually it must either reach and remain forever at ncs,   *)
\*      (* or T1 must become true and remain true forever.                   *)
\*      (*********************************************************************) 
\*      <4>1. TRUE ~> (pc[1] = "ncs") \/ T1
\*        <5>1. LInv /\ ~Success => (pc[1] = "ncs") \/ T1 \/ (pc[1] = "f")
\*          BY SMT DEF LInv, TypeOK, Mutex, Success, T1, Trying, InCS
\*        <5>2. (pc[1] = "f") ~> (pc[1] = "ncs")
\*          <6>1. LInv /\ (pc[1] = "f") /\ [Next]_vars => 
\*                  (pc[1] = "f")' \/ (pc[1] = "ncs")'
\*            BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars
\*          <6>2. LInv /\ (pc[1] = "f") /\ <<Next /\ P(1)>>_vars => (pc[1] = "ncs")'
\*            BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars
\*          <6>3. LInv /\ (pc[1] = "f") => ENABLED <<P(1)>>_vars                
\*            PROOF OMITTED
\*          <6>4. QED                                                           PROOF
\*            (***************************************************************)
\*            (* BY <6>1, <6>2, <6>3, Rule WF1, and <1>4.                    *)
\*            (***************************************************************) OMITTED
\*        <5>3. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* By <5>1, <5>2, and Leads-To Induction.                        *)
\*          (*****************************************************************) OMITTED
\*      <4>2. (pc[1] = "ncs") ~> T1 \/ ((pc[1] = "ncs") /\ []~T1)                         PROOF
\*        (*******************************************************************)
\*        (* BY the tautology F ~> (G \/ (F /\ []~G)).                       *)
\*        (*******************************************************************) OMITTED
\*      <4>3. (pc[1] = "ncs") /\ []~T1 => [](pc[1] = "ncs")
\*        <5>1. LInv /\ ~T1' /\ (pc[1] = "ncs") /\ [Next]_vars => (pc[1] = "ncs")'
\*          BY SMT DEF LInv, TypeOK, Next, ProcSet, P,
\*                      ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>2. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <1>4, <5>1, and rule INV1.                                 *)
\*          (*****************************************************************) OMITTED
\*      <4>4. T1 => []T1
\*        <5>1. LInv /\ ~Success /\ ~Success' /\ T1 /\ [Next]_vars => T1'
\*          BY SMT DEF LInv, TypeOK, Next, ProcSet, P, InCS, Success,
\*                      ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>2. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <5>1, <1>4, and rule INV1.                                 *)
\*          (*****************************************************************) OMITTED
\*      <4>5. QED                                                               PROOF
\*        (*******************************************************************)
\*        (* BY <4>1 - <4>4 and Leads-To Induction.                          *)
\*        (*******************************************************************) OMITTED
\*    <3>3. [](pc[1] = "ncs") => []~x[1]                                        PROOF       
\*      (*********************************************************************)
\*      (* x[1] equals FALSE when process 1 is at ncs.                       *)
\*      (*********************************************************************) 
\*      <4>1. LInv /\ (pc[1] = "ncs") => ~x[1]
\*        BY SMT DEF LInv, TypeOK
\*      <4>2. QED                                                               PROOF
\*        (*******************************************************************)
\*        (* BY <1>4, <4>1, and proof rule INV1.                             *)
\*        (*******************************************************************) OMITTED      
\*    <3>4. []T1 ~> []~x[1]                                                     PROOF
\*      (*********************************************************************)
\*      (* T0 implies x[0], and the code, fairness, and []~Success imply     *)
\*      (* []x[0] leads to process 1 reaching and remaining forever at e4    *)
\*      (* with x[1] equal to FALSE.                                         *)
\*      (*********************************************************************) 
\*      <4>1. SUFFICES ASSUME []T1
\*                     PROVE  TRUE ~> []~x[1]                                   PROOF
\*        (*******************************************************************)
\*        (* BY the []~> rule.                                               *)
\*        (*******************************************************************) OMITTED
\*      <4>2. (pc[1] = "e1") ~> (pc[1] = "e2")
\*        <5> DEFINE PP == LInv /\ T1 /\ (pc[1] = "e1")
\*                   Q ==  pc[1] = "e2"
\*                   AA == (pc[1] # "ncs") /\ P(1)
\*        <5>1. T1' => (PP /\ [Next]_vars => (PP' \/ Q'))
\*          BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>2. T1' => (PP /\ <<Next /\ AA>>_vars => Q')
\*          BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>3. PP => ENABLED <<AA>>_vars
\*          PROOF OMITTED
\*        <5>4. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <5>1 - <5>3, <4>1, and rule WF1.                           *)
\*          (*****************************************************************) OMITTED
\*      <4>3. (pc[1] = "e2") ~> (pc[1] = "e3")
\*        <5> DEFINE PP == LInv /\ T1 /\ (pc[1] = "e2")
\*                   Q ==  pc[1] = "e3"
\*                   AA == (pc[1] # "ncs") /\ P(1)
\*        <5>1. T1' => (PP /\ [Next]_vars => (PP' \/ Q'))
\*          BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>2. T1' => (PP /\ <<Next /\ AA>>_vars => Q')
\*          BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>3. PP => ENABLED <<AA>>_vars
\*          PROOF OMITTED
\*        <5>4. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <5>1 - <5>3, <4>1, and rule WF1.                           *)
\*          (*****************************************************************) OMITTED
\*      <4>4. (pc[1] = "e3") ~> (pc[1] = "e4")
\*        <5> DEFINE PP == LInv /\ T1 /\ (pc[1] = "e3")
\*                   Q ==  pc[1] = "e4"
\*                   AA == (pc[1] # "ncs") /\ P(1)
\*        <5>1. T1' => (PP /\ [Next]_vars => (PP' \/ Q'))
\*          BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>2. T1' => (PP /\ <<Next /\ AA>>_vars => Q')
\*          BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
\*                 ncs, cs, e1, e2, e3, e4, f, vars, T1, Trying
\*        <5>3. PP => ENABLED <<AA>>_vars
\*          PROOF OMITTED
\*        <5>4. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <5>1 - <5>3, <4>1, and rule WF1.                           *)
\*          (*****************************************************************) OMITTED
\*      <4>5. (pc[1] = "e4") => []~x[1]
\*        <5>1. (pc[0] = "e2") /\ LInv /\ (pc[1] = "e4") /\ [Next]_vars 
\*                => (pc[1] = "e4")'
\*          BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
\*                   ncs, cs, e1, e2, e3, e4, f, vars, T0, Trying
\*        <5>2. (pc[1] = "e4") => [](pc[1] = "e4")                              PROOF
\*          (*****************************************************************)
\*          (* BY <5>1, <3>1, <1>4, and rule INV1.                           *)
\*          (*****************************************************************) OMITTED 
\*        <5>3. LInv /\ (pc[1] = "e4") => ~x[1]
\*          BY SMT DEF LInv, TypeOK
\*        <5>4. [](pc[1] = "e4") => []~x[1]                                     PROOF
\*          (*****************************************************************)
\*          (* BY <5>3, <1>4, and the proof rule F => G |- []F => []G.       *)
\*          (*****************************************************************) OMITTED
\*        <5>5. QED                                                             PROOF
\*          (*****************************************************************)
\*          (* BY <5>2, <5>4.                                                *)
\*          (*****************************************************************) OMITTED
\*      <4>6. QED                                                               PROOF
\*        (*******************************************************************)
\*        (* BY <4>1 -- <4>6, Leads-To Induction, and the definition of T1.  *)
\*        (*******************************************************************) OMITTED                     
\*    <3>5. QED                                                                 PROOF
\*      (*********************************************************************)
\*      (* BY <3>1 - <3>4 and Leads-To Induction.                            *)
\*      (*********************************************************************) OMITTED
  <2>3. []((pc[0] = "e2") /\ ~x[1]) ~> FALSE                                  PROOF 
    (***********************************************************************)
    (* The code and Fairness implythat (pc[0] = "e2") /\ []~x[1] leads to  *)
    (* process 0 reaching cs, contradicting []~Success.                    *)
    (***********************************************************************) \* OMITTED
    <3>1. ((pc[0] = "e2") /\ []~x[1]) ~> (pc[0] = "cs")
      <4> DEFINE PP == pc[0] = "e2"
                 Q  == pc[0] = "cs"
                 AA == (pc[0] # "ncs") /\ P(0)
      <4>1. SUFFICES ASSUME []LInv /\ []~x[1]
                     PROVE  PP ~> Q                                           PROOF
        (*******************************************************************)
        (* BY <1>4 and temporal reasoning.                                 *)
        (*******************************************************************) OMITTED
      <4>2. LInv /\ ~x[1] /\ ~x[1]' => (PP /\ [Next]_vars => (PP' \/ Q'))
        BY SMT DEF LInv, TypeOK, Next, Mutex, InCS, ProcSet, P,
               ncs, cs, e1, e2, e3, e4, f, vars
      <4>3. LInv /\ ~x[1] => (PP /\ <<Next /\ AA>>_vars => Q')
        BY SMT DEF LInv, TypeOK, Next, (* Mutex, InCS, *) ProcSet, P,
               ncs, cs, e1, e2, e3, e4, f, vars
      <4>4. PP => ENABLED <<AA>>_vars
        <5>1. SUFFICES (* |- *) ASSUME PP /\ LInv  /\ ~x[1]
                                PROVE  ENABLED <<AA>>_vars                    PROOF
          (*****************************************************************)
          (* By <4>1.                                                      *)
          (*****************************************************************) OMITTED
        <5>2. e2(0) => <<AA>>_vars  
          BY <5>1, SMT DEF LInv, TypeOK, Next, ProcSet, P,
                            ncs, cs, e1, e2, e3, e4, f, vars
        <5>3. ENABLED e2(0) => ENABLED <<AA>>_vars                            PROOF
          (*****************************************************************)
          (* By <5>2, rule (|- A => B) => (|- (ENABLED A( => (ENABLED B)). *)
          (*****************************************************************) OMITTED
        <5>4. \E xP, pcP : /\ pc[0] = "e2"
                           /\ IF ~x[1-0]
                                THEN /\ pcP = [pc EXCEPT ![0] = "cs"]
                                ELSE /\ IF 0 = 0
                                         THEN /\ pcP = [pc EXCEPT ![0] = "e2"]
                                         ELSE /\ pcP = [pc EXCEPT ![0] = "e3"]
                                     /\ xP = x
           BY <5>1  \* SMT could not prove this.
        <5>5. QED                                                             PROOF
          (*****************************************************************)
          (* BY <5>3 and <5>4, which equals ENABLED e2(0).                 *)
          (*****************************************************************) OMITTED
      <4>5. QED                                                               PROOF
        (*******************************************************************)
        (* BY <4>2 - <4>4, <4>2, and rule WF1.                             *)
        (*******************************************************************) OMITTED
    <3>2. (pc[0] = "cs") /\ ~Success => FALSE
      BY DEF Success, InCS
    <3>3. QED                                                                 PROOF
      (*********************************************************************)
      (* By <3>1, <3>2, and <1>4.                                          *)
      (*********************************************************************) OMITTED
      
  <2>4. QED                                                                   PROOF
    (***********************************************************************)
    (* BY <2>1 - <2>3 and Leads-To Induction.                              *)
    (***********************************************************************) OMITTED
  
<1>6. T1 /\ []~Success ~> FALSE
  <2>1. T1 => []T1                                                            PROOF
    (***********************************************************************)
    (* From the code, we see that if T1 is true and process 1 never        *)
    (* reaches cs (which is implied by the assumption []~Success), then T1 *)
    (* remains forever true.                                               *)
    (***********************************************************************) OMITTED
  <2>2. []T1  ~>  (T0 \/ [](T1 /\ ~T0))
    BY LS4
    (***********************************************************************)
    (* By the tautologies F ~> (G \/ (F /\ []~G)) and                      *)
    (* []F /\ []G <=> [](F /\ G).                                          *)
    (***********************************************************************)
  <2>3. [](T1 /\ ~T0)  ~>  [](T1 /\ ~x[0])                                    PROOF
    (***********************************************************************)
    (* By the code and Fairness, []~T0 implies that eventually process 0   *)
    (* is always at ncs, which implies that x[0] always equals FALSE.      *)
    (***********************************************************************) OMITTED
  <2>4. [](T1 /\ ~x[0]) ~> FALSE                                              PROOF
    (***********************************************************************)
    (* The code, Fairness, and []~x[0] imply that process 1 eventually     *)
    (* reaches e2.  Assumption Fairness and []~x[0] then imply that        *)
    (* process 1 reaches cs, contradicting the assumption []~Success.      *)
    (***********************************************************************) OMITTED
  <2>5. QED 
    BY <2>1, <2>2, <2>3, <2>4, PTL                                           \* PROOF                      PROOF
    (***********************************************************************)
    (* BY <2>1 - <2>4 and Leads-To Induction.                              *)
    (***********************************************************************) \* OMITTED

<1>7. QED                                                                     \* PROOF
  BY <1>4, <1>5, <1>6, PTL
  (*************************************************************************)
  (* By <1>4 - <1>6 and Leads-To Induction.                                *)
  (*************************************************************************)\* OMITTED

-----------------------------------------------------------------------------
(***************************************************************************)
(* The following is an informal version of the proof above, which assumes  *)
(* that one is reasoning about a behavior satisfying Spec.                 *)
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
    <3>5. QED                                                                 PROOF
      (*********************************************************************)
      (* BY <3>1 - <3>4 and Leads-To Induction.                            *)
      (*********************************************************************) OMITTED
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
    (***********************************************************************)
    (* BY <2>1 - <2>4 and Leads-To Induction.                              *)
    (***********************************************************************) OMITTED

<1>5. QED                                                                     PROOF
  (*************************************************************************)
  (* By <1>1 - <1>3 and a trivial application of Leads-To Induction.       *)
  (*************************************************************************) OMITTED
-----------------------------------------------------------------------------
\* EXPERIMENTS WITH LS4

\* Following needed because at the moment, LS4 doesn't deal
\* properly with TRUE and FALSE
False == x /\ ~x
True == x \/ ~x
USE DEF False, True

VARIABLE Success, T0, T1

THEOREM  \* Proof of <1>2 step
/\ [](Spec => []LInv /\ [][Next]_vars /\ Fairness)
/\ [](Fairness <=> []Fairness)
/\ [](([]LInv /\ [][Next]_vars /\ Fairness /\ []~Success
          => (T0 \/ T1 ~> False)))
=> (Spec => (T0 \/ T1 ~> Success))
BY LS4
    
THEOREM \* Another proof of <1>2 step by  DEF Fairness
/\ [](Spec => []LInv /\ [][Next]_vars /\ Fairness)
/\ [](Fairness <=> []Fairness)
=> 
([](([]LInv /\ [][Next]_vars /\ Fairness /\ []~Success
          => (T0 \/ T1 ~> False)))
  => (Spec => (T0 \/ T1 ~> Success)))
BY LS4


   THEOREM ASSUME x = 0
           PROVE  [](x=0)
   BY LS4

THEOREM

([]Inv /\ []~T1 => [](~T0))
=> 
([]Inv => (T0 ~> T1))
BY LS4

THEOREM ASSUME TEMPORAL F, TEMPORAL G
        PROVE  F ~> (G \/ (F /\ []~G))
BY PTL

THEOREM ASSUME TEMPORAL F, TEMPORAL G
        PROVE  []([]F => (True ~> G)) => ([]F ~> []F /\ G)
BY LS4

THEOREM ASSUME TEMPORAL F, TEMPORAL G
        PROVE  <>[](F /\ G) \equiv (<>[]F) /\ (<>[]G)
BY PTL

VARIABLES  T
THEOREM  
\*/\ [](T1 => []T1)  
/\ T0 ~> False                                                         
/\ ([]T1  ~>  (T0 \/ [](T1 /\ ~T0)))

/\  ([](T1 /\ ~T0)  ~>  [](T1 /\ ~T))                                    

/\   ([](T1 /\ ~T) ~> False)                                              
    (***********************************************************************)
    (* The code, fairness, and []~x[0] imply that process 1 eventually     *)
    (* reaches e2.  Fairness and []~x[0] then imply that process 1 reaches *)
    (* cs, contradicting the assumption []~Success.                        *)
    (***********************************************************************) 
=>  ([]T1 ~> False)
    BY LS4                                           \*                       PROOF
    (***********************************************************************)
    (* BY <2>1 - <2>4 and Leads-To Induction.                              *)
    (***********************************************************************) \* OMITTED


THEOREM 
/\  ([](T1 /\ ~T0)  ~>  [](T1 /\ ~T))                                    

/\   ([](T1 /\ ~T) ~> False)                                              
=> ([](T1 /\ ~T0)  ~>  False) 
BY LS4

THEOREM 
/\ ([](T1 /\ ~T0)  ~>  False)
/\ T0 ~> False
=> ((T0 \/ [](T1 /\ ~T0) ~> False))
BY LS4

THEOREM []T1  ~>  (T0 \/ [](T1 /\ ~T0))
BY LS4

THEOREM 
/\ ([](T1 /\ ~T0)  ~>  False)
/\ T0 ~> False
=> ([]T1 ~> False)
BY LS4

THEOREM 
/\ T0 ~> False
/\ ([](T1 /\ ~T0)  ~>  False)
/\ T0 ~> False
/\ []T1  ~>  (T0 \/ [](T1 /\ ~T0))
/\  ([](T1 /\ ~T0)  ~>  [](T1 /\ ~T))
/\   ([](T1 /\ ~T) ~> False)
=> ([]T1 ~> False)
BY LS4

THEOREM 
/\ ([](T1 /\ ~T0)  ~>  False)
/\ T0 ~> False
/\ []T1  ~>  (T0 \/ [](T1 /\ ~T0))
/\  ([](T1 /\ ~T0)  ~>  [](T1 /\ ~T))
/\   ([](T1 /\ ~T) ~> False)
=> ([]T1 ~> False)
BY LS4

=============================================================================

\* Modification History
\* Last modified Wed Jan 07 16:32:09 PST 2015 by lamport
\* Created Tue Apr 17 15:33:34 PDT 2012 by lamport

---------------------------- MODULE Prisoner ---------------------------------
EXTENDS Naturals, TLAPS, SequenceTheorems

(* We model the prisoner's dilemma (for details see e.g.[1]). A game
   is set up by players P and Q with each player trying to achieve the 
   highest reward. Each player has the choice to defect or cooperate, where
   the score pscore of P and qscore of Q are updated according to the matrix:
           \Q|
          P \| Cooperate | Defect
   ----------+-------------------   
   Cooperate | R / R     | S / T
   Defect    | T / S     | P / P
   
   If both players cooperate, they are rewarded (R) by going free, if both
   defect, they are both punished (P). The other two cases are symmetric, 
   where one player gets the reward for giving in to temptation to defect (T),
   where the cooperating player is the sucker (S).
   
   The original game is defined for utilities T,R,P,S of 5,3,1,0 but the reasoning
   still applies as long as T > R > P > S. At the moment we use the fixed values
   but theorem Ineq proves the inequality s.t. it the reasoning can be adapted
   to symbolic reasoning. 
       
  [1] http://plato.stanford.edu/entries/prisoner-dilemma/
*)

CONSTANT R,S,T,P \* reward, sucker, temptation, punishment

ASSUME UtilityAssumption ==
  /\ R \in Nat /\ S \in Nat /\ T \in Nat /\ P \in Nat
  /\ T > R /\ R > P /\ P > S

Move == {"defect", "cooperate"}
USE DEF Move

rewardP == [ pq \in Move \X Move |->
                CASE pq = <<"cooperate", "cooperate">> -> R
                []   pq = <<"cooperate", "defect">> -> S
                []   pq = <<"defect", "cooperate">> -> T
                []   pq = <<"defect", "defect">> -> P ]
rewardQ == [ pq \in Move \X Move |->
                CASE pq = <<"cooperate", "cooperate">> -> R
                []   pq = <<"cooperate", "defect">> -> T
                []   pq = <<"defect", "cooperate">> -> S
                []   pq = <<"defect", "defect">> -> P ]

LEMMA
  \A y \in Move : rewardP[<<"defect",y>>] > rewardP[<<"cooperate",y>>]
BY UtilityAssumption DEF Move, rewardP

VARIABLE pscore, qscore

vars == <<pscore,qscore>>


(* We use f and g to model different strategies, but only f is defined so far.
   It represents the strategy which always defects.
*) 
Strategy == [Move \X Move -> Move]

betray == [pq \in Move \X Move |-> "defect"]

LEMMA betrayStrategy == betray \in Strategy
BY DEF betray, Strategy

-------------------------------------------------------------------------------------------

TypeOK == /\ pscore \in [Move \X Move -> Nat] 
          /\ qscore \in [Move \X Move -> Nat] 

Init == /\ pscore = [x \in Move \X Move |-> 0] 
        /\ qscore = [x \in Move \X Move |-> 0] 

(* The next state relation parametrized by the moves x and y of P and Q, where the score
   is updated according to the payoff matrix above. *)
Round(x,y) == 
   /\ pscore' = [arg \in Move \X Move |-> pscore[<<x,y>>] + rewardP[<<arg[1],arg[2]>>] ]
   /\ qscore' = [arg \in Move \X Move |-> qscore[<<x,y>>] + rewardQ[<<arg[1],arg[2]>>] ]

LEMMA \A y \in Move : Round("defect", "defect") => pscore[<<"defect",y>>]' > pscore[<<"cooperate",y>>]' 
BY UtilityAssumption DEF Round, rewardP, rewardQ

Next == \E x,y \in Move : Round(x,y)

Spec == Init /\ [][Next]_vars

(* Proves type correctness of the specification. *)
THEOREM Spec => []TypeOK
<1>1. Init => TypeOK
  BY DEF Init, TypeOK
<1>2. TypeOK /\ Next => TypeOK'
  BY UtilityAssumption DEF TypeOK, Next, Round, rewardP, rewardQ
<1>3. TypeOK /\ UNCHANGED vars => TypeOK'
  BY DEF TypeOK, vars
<1>. QED  BY <1>1, <1>2, <1>3, PTL DEF Spec

Myseq == [ x \in 1..2 |->
           CASE x = 1 -> 1
           []   x = 2 -> 0
         ]

LEMMA Myseq \in Seq(Nat)
<1>1 Myseq[1] \in Nat BY Myseq[1] = 1 DEF Myseq
<1>2 Myseq[2] \in Nat BY Myseq[2] = 0 DEF Myseq
<1> DEFINE e(x) == Myseq[x]
<1> QED BY <1>1, <1>2, IsASeq DEF e

=============================================================================
\* Modification History
\* Last modified Wed Dec 02 18:43:52 CET 2015 by marty
\* Last modified Wed Dec 02 17:15:46 CET 2015 by merz
\* Created Mon Nov 30 08:09:16 CET 2015 by marty

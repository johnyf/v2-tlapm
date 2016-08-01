--------------------------------- MODULE pd ---------------------------------

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


EXTENDS Naturals, TLAPS
CONSTANT Defect, Coop
CONSTANT R,S,T,P \* reward, sucker, temptation, punishment

VARIABLE pscore, qscore

vars == <<pscore,qscore>>
Move == {Defect, Coop}

USE DEF Move

(* We use f and g to model different strategies, but only f is defined so far.
   It represents the strategy which always defects.
*) 
CONSTANT f,g

\* we need extensionality to prove type correctness
Ext(D,I,fun) == fun \in [D -> I] <=> DOMAIN fun = D /\ \A x \in D : fun[x] \in I

\* assume all functions are extensional
ASSUME ExtAssumption == \A D,I,fun : Ext(D,I,fun) 

TypeOK == /\ pscore \in Nat 
          /\ qscore \in Nat 
          /\ f \in [Move \X Move -> Move]

Init == /\ pscore = 0 
        /\ qscore = 0
        /\ T = 5
        /\ R = 3
        /\ P = 1
        /\ S = 0
        /\ \A x \in Move \X Move : f[x] = Defect

\* a theorem confiriming the inequality T > R > P > S
THEOREM Ineq == Init => [](T >R /\ R > P /\ P > S)
<1> SUFFICES ASSUME Init PROVE [](T >R /\ R > P /\ P > S) OBVIOUS
<1>1 Init => T >R /\ R > P /\ P > S BY DEF Init
<1> QED BY PTL, <1>1

(* The next state relation parametrized by the moves x and y of P and Q, where the score
   is updated according to the payoff matrix above. *)
Next(x,y) == 
   /\ x = Defect /\ y = Defect /\ pscore' = pscore + P /\ qscore' = qscore + P 
   /\ x = Coop   /\ y = Defect /\ pscore' = pscore + S /\ qscore' = qscore + T 
   /\ x = Defect /\ y = Coop   /\ pscore' = pscore + T /\ qscore' = qscore + S
   /\ x = Coop   /\ y = Coop   /\ pscore' = pscore + R /\ qscore' = qscore + R

(* Proves type correctness of the Next relation. *)
THEOREM Init /\ [][\E x,y \in Move: Next(x,y)]_vars => []TypeOK
<1> SUFFICES ASSUME Init, [][\E x,y \in Move: Next(x,y)]_vars PROVE []TypeOK OBVIOUS
<1>1 Init => TypeOK 
  <2> ( \A x \in Move \X Move : f[x] = Defect) => (\A x \in Move \X Move: f[x] \in Move ) BY Isa
  <2> (\A x \in Move \X Move: f[x] \in Move ) => f \in [(Move \X Move) -> Move] BY Isa, ExtAssumption DEF Ext
  <2> QED BY DEF Init, TypeOK
<1>2 (TypeOK /\ \Ex,y \in Move : Next(x,y)) => TypeOK' BY DEF Init, TypeOK, Next
<1> QED BY PTL, <1>1, <1>2

(* Template for proving a property -- so far it is unclear how to compare two moves with different strategies *)
THEOREM TypeOK /\ Init /\ [][\E x,y \in Move : Next(x,y)]_vars => TRUE
<1> SUFFICES ASSUME TypeOK, Init, [][\E x,y \in Move: Next(x,y)]_vars PROVE TRUE OBVIOUS
<1> QED OBVIOUS


=============================================================================
\* Modification History
\* Last modified Wed Dec 02 18:02:24 CET 2015 by marty
\* Created Mon Nov 30 08:09:16 CET 2015 by marty

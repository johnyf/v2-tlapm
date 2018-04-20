------------------------------ MODULE homepage ------------------------------
EXTENDS Naturals, Integers 

THEOREM ~ \E x \in Nat : x + 1 = 0 
<1>1. SUFFICES ASSUME \E x \in Nat : x + 1 = 0 PROVE FALSE OBVIOUS (* new facts: \E x \in Nat : x + 1 = 0 *) (* goal: FALSE *) 
<1>2. PICK u \in Nat : u = -1 (* goal: \E u \in Nat : u = -1 *) 
<2>1. \A n \in Nat : n + 1 = 0 => n = -1 OBVIOUS 
<2>2. QED BY <2>1, <1>1 (* new facts: u \in Nat, u = -1 *) (* goal: FALSE *) 
<1> QED BY -1 \notin Nat, <1>2


VARIABLE x 
A == /\ x = 1 
     /\ x' = 2 
     
B == /\ x = 2 
     /\ x' = 1 
     
Next == A \/ B 

Inv == x \in {1, 2}


THEOREM Inv /\ Next => Inv' 
<1>1. ASSUME Inv, Next PROVE Inv' 
 <2>1. ASSUME A PROVE Inv' 
 <2>2. ASSUME B PROVE Inv' 
 <2>3. QED BY <1>1, <2>1, <2>2 DEF Next 
<1>2. QED BY <1>1

THEOREM Inv /\ Next => Inv' 
<1>1. SUFFICES /\ Inv /\ A => Inv' 
               /\ Inv /\ B => Inv' 
               BY DEF Next 
<1>2. ASSUME Inv, A PROVE Inv' 
<1>3. ASSUME Inv, B PROVE Inv' 
<1>4. QED BY <1>2, <1>3




=============================================================================
\* Modification History
\* Last modified Fri Apr 20 14:51:57 CEST 2018 by merz
\* Last modified Wed Apr 12 15:54:09 CEST 2017 by marty
\* Created Mon Mar 16 14:18:20 CET 2015 by marty

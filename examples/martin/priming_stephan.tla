---------------------------- MODULE priming_stephan ----------------------------
EXTENDS Naturals, NaturalsInduction

VARIABLE x

Prime[n \in Nat] == IF n = 0 THEN x ELSE Prime[n-1]'

THEOREM Prime = [n \in Nat |-> x]
<1>. DEFINE DefPr(g,n) == IF n = 0 THEN x ELSE g[n-1]
            DefPrime(g,n) == IF n = 0 THEN x ELSE g[n-1]'
<1>1. ASSUME NEW n \in Nat, NEW g, NEW h
      PROVE  DefPrime(g,n) = DefPr(g,n)
  OBVIOUS
<1>2. Prime = CHOOSE g : g = [n \in Nat |-> DefPr(g,n)]
  BY <1>1 DEF Prime
<1>3. ASSUME NEW n \in Nat, NEW g, NEW h,
             \A i \in 0 .. (n-1) : g[i] = h[i]
      PROVE  DefPr(g,n) = DefPr(h,n)
  BY <1>3
<1>. HIDE DEF DefPr
<1>4. Prime = [n \in Nat |-> DefPr(Prime, n)]
  BY <1>2, <1>3, RecursiveFcnOfNat, Isa
<1>5. \A n \in Nat : DefPr(Prime, n) = x
  <2>1. DefPr(Prime, 0) = x
    BY DEF DefPr
  <2>2. ASSUME NEW n \in Nat, DefPr(Prime, n) = x
        PROVE  DefPr(Prime, n+1) = x
    <3>1. DefPr(Prime, n+1) = Prime[n]
      BY DEF DefPr
    <3>2. Prime[n] = DefPr(Prime, n)
      BY <1>4
    <3>. QED
      BY <2>2, <3>1, <3>2
  <2>. QED
    BY <2>1, <2>2, NatInduction, Isa
<1>. QED
  BY <1>4, <1>5

=============================================================================
\* Modification History
\* Last modified Fri Feb 20 14:08:43 CET 2015 by marty
\* Last modified Fri Feb 20 13:48:56 CET 2015 by merz
\* Created Fri Feb 20 11:19:19 CET 2015 by merz

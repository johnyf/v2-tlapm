------------------------------- MODULE NTape3 -------------------------------
EXTENDS Naturals, TLAPS

CONSTANT f
VARIABLES h0,h1,m

symbols == {"a", "b" }

TypeOK == f \in [Nat -> symbols] /\
          h0 \in [Nat -> Nat] /\
          h1 \in [Nat -> Nat] /\
          m \in Nat 

(* expresses that g is monotonic up to n *)
MON(g,n) == \A i, j \in 0 .. n : (i<j => g[i] < g[j])
(* expresses that s occurs n times up to n *)
NOCC(g,n,s) == \A i \in 0 .. n : (f[g[i]] = s)

(* main lemma: the function g in is monotonic up to n and indexes n symbols s *)
A(g,n,s) == MON(g,n) \land NOCC(g,n,s)

I(s) == \A n \in Nat : \E k \in Nat  : n < k /\ f[k] = s

IncM == m' = m+1
\*Inv(n) == A(h0,n,"a") \/ A(h1,n,"b")  


Init == /\ m = 0
        /\ h0[0] = CHOOSE p \in Nat : f[h0[0]] = "a"   
        /\ h1[0] = CHOOSE p \in Nat : f[h1[0]] = "b"

Next == /\ m' = m +1
        /\ h0' = [h0 EXCEPT ![m+1] = CHOOSE p \in Nat : h0[m]<p /\  f[h0[p]] = "a" ]
        /\ h1' = [h1 EXCEPT ![m+1] = CHOOSE p \in Nat : h1[m]<p /\  f[h1[p]] = "b" ]
        
Inv == \/ (\E p : h0[m]<p /\  f[h0[p]] = "a")
       \/ (\E p : h1[m]<p /\  f[h1[p]] = "b")


LEMMA Init /\ TypeOK /\ (I("a") \/ I("b")) => Inv 
<1>1 ASSUME Init, TypeOK, I("a") \/ I("b") PROVE Inv BY <1>1 DEF Init, TypeOK, Inv, I
<1> QED BY <1>1



(*
TwoEqualSymbols == f[0] = f[1] \/ f[0] = f[2] \/ f[1] = f[2]


LEMMA Infty == ASSUME TypeOK PROVE I("a") \/ I("b")


\*LEMMA L1 == ASSUME TypeOK, (\neg (f[0] = f[1])) /\ (\neg (f[1] = f[2])) PROVE FALSE
LEMMA L1 == ASSUME TypeOK, ~TwoEqualSymbols PROVE FALSE
<1>1. f[0] = "a" \/ f[0] = "b"
    BY DEF TypeOK, symbols, Nat
<1>2. f[1] = "a" \/ f[1] = "b"
    BY DEF TypeOK, symbols, Nat
<1>3. f[2] = "a" \/ f[2] = "b"
    BY DEF TypeOK, symbols, Nat
<1>. QED BY <1>1,<1>2,<1>3, SMT DEF TypeOK, TwoEqualSymbols

LEMMA L2 == ASSUME TypeOK PROVE TwoEqualSymbols
<1>1. f[0] = "a" \/ f[0] = "b"
    BY DEF TypeOK, symbols, Nat
<1>2. f[1] = "a" \/ f[1] = "b"
    BY DEF TypeOK, symbols, Nat
<1>3. f[2] = "a" \/ f[2] = "b"
    BY DEF TypeOK, symbols, Nat
<1>. QED BY <1>1,<1>2,<1>3, SMT DEF TypeOK, TwoEqualSymbols

*)


=============================================================================
\* Modification History
\* Last modified Thu Feb 12 16:19:44 CET 2015 by marty
\* Created Tue Jan 06 11:16:11 CET 2015 by marty

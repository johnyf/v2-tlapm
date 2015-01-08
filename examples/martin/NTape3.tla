------------------------------- MODULE NTape3 -------------------------------
EXTENDS Naturals, TLAPS

CONSTANT f
VARIABLES h1,h2,n1,n2,m

symbols == {"a", "b" }

TypeOK == f \in [Nat -> symbols] /\
          h1 \in [Nat -> Nat] /\
          h2 \in [Nat -> Nat] /\
          n1 \in Nat /\
          n2 \in Nat /\
          m \in Nat 

(* expresses that g is monotonix up to n *)
MON(g,n) == \A i, j \in 0 .. n : (i<j => g[i] < g[j])
(* expresses that s occurs n times up to n *)
NOCC(g,n,s) == \A i \in 0 .. n : (f[g[i]] = s)

(* main lemma: the function g in is monotonic up to n and indexes n symbols s *)
A(g,n,s) == MON(g,n) \land NOCC(g,n,s)

I(s) == \A n \in Nat : \E k \in Nat  : n < k /\ f[k] = s

IncM == m' = m+1
Inv(n) == A(h1,n,"a") \/ A(h2,n,"b")  


Init == m = 0 /\ n1 = 0 /\ n2 = 0
Next == IncM  


TwoEqualSymbols == f[0] = f[1] \/ f[0] = f[2] \/ f[1] = f[2]



LEMMA Infty == ASSUME TypeOK PROVE I("a") \/ I("b")
<1>1. ASSUME x \in Nat 


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




=============================================================================
\* Modification History
\* Last modified Wed Jan 07 10:55:36 CET 2015 by marty
\* Created Tue Jan 06 11:16:11 CET 2015 by marty

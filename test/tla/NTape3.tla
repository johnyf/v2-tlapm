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


Init == /\ m = 0
        /\ h0[0] = CHOOSE p \in Nat : f[h0[0]] = "a"   
        /\ h1[0] = CHOOSE p \in Nat : f[h1[0]] = "b"

Next == /\ m' = m +1
        /\ h0' = [h0 EXCEPT ![m+1] = CHOOSE p \in Nat : h0[m]<p /\  f[h0[p]] = "a" ]
        /\ h1' = [h1 EXCEPT ![m+1] = CHOOSE p \in Nat : h1[m]<p /\  f[h1[p]] = "b" ]
        
LemmaInv == \/ (\E p : h0[m]<p /\  f[h0[p]] = "a")
            \/ (\E p : h1[m]<p /\  f[h1[p]] = "b")

LEMMA ASSUME NEW P(_), 
             ASSUME NEW x PROVE P(x) 
      PROVE \A p : P(p) OBVIOUS  

LEMMA Init /\ TypeOK /\ (I("a") \/ I("b")) => LemmaInv 
<1> SUFFICES ASSUME Init, TypeOK, I("a") \/ I("b"), NEW pos \in Nat PROVE LemmaInv
  OBVIOUS
<1>1 f[pos] = "a" \/ f[pos] = "b" BY DEF TypeOK, symbols
<1>2 (f[pos] = "a" /\ CHOOSE p \in Nat : f[h0[0]] = "a") => \E q \in Nat : f[h0[0]] = "a"
  <2>1. ASSUME NEW p \in Nat, f[h0[0]] = "a"
        PROVE  f[h0[0]] = "a"
(*  <2>1. (CHOOSE p \in Nat : f[h0[0]] = "a") => f[h0[0]] = "a" OBVIOUS *)
  <2>. QED  BY <2>1
<1>3 f[pos] = "b" /\ CHOOSE p \in Nat : f[h1[0]] = "b" => \E q \in Nat : f[h1[0]] = "b"
<1> QED BY <1>1, <1>2, <1>3 DEF Init, TypeOK, LemmaInv, I


Inv == [](NOCC(h0,m,"a") \/ NOCC(h1,m,"b"))

LEMMA ASSUME NEW CONSTANT P(_), NEW VARIABLE v PROVE []( [(ENABLED P(v))']_v)

=============================================================================
\* Modification History
\* Last modified Fri Mar 20 14:49:26 CET 2015 by marty
\* Created Tue Jan 06 11:16:11 CET 2015 by marty

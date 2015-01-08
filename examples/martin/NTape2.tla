------------------------------- MODULE NTape1 -------------------------------
EXTENDS Naturals, TLAPS

CONSTANT f
VARIABLES x,h

symbols == {"a", "b" }

TypeOK == f \in [Nat -> symbols] /\
          h \in [Nat -> Nat] 

TwoEqualSymbols == f[0] = f[1] \/ f[0] = f[2] \/ f[1] = f[2]


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
\* Last modified Tue Jan 06 17:27:34 CET 2015 by marty
\* Created Tue Jan 06 11:16:11 CET 2015 by marty

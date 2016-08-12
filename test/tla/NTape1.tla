------------------------------- MODULE NTape1 -------------------------------
EXTENDS Naturals, TLAPS

CONSTANT f
VARIABLES h

symbols == {"a", "b" }

TypeOK == f \in [Nat -> symbols] /\
          h \in [Nat -> Nat] 

TwoEqualSymbols(i,j,k) == i<j /\ j<k /\ (f[i] = f[j] \/ f[i] = f[k] \/ f[j] = f[k])


\*LEMMA L1 == ASSUME TypeOK, (\neg (f[0] = f[1])) /\ (\neg (f[1] = f[2])) PROVE FALSE
LEMMA L1 == ASSUME TypeOK, NEW x \in Nat, NEW y \in Nat, NEW z \in Nat , x <y, y<z, ~TwoEqualSymbols(x,y,z) PROVE FALSE
<1>1. f[x] = "a" \/ f[x] = "b"
    BY DEF TypeOK, symbols, Nat
<1>2. f[y] = "a" \/ f[y] = "b"
    BY DEF TypeOK, symbols, Nat
<1>3. f[z] = "a" \/ f[z] = "b"
    BY DEF TypeOK, symbols, Nat
<1>. QED BY <1>1,<1>2,<1>3, SMT DEF TypeOK, TwoEqualSymbols, Nat

LEMMA L2 == ASSUME TypeOK, NEW x \in Nat, NEW y \in Nat, NEW z \in Nat, x < y, y < z PROVE TwoEqualSymbols(x,y,z)
<1>1. f[x] = "a" \/ f[x] = "b"
    BY DEF TypeOK, symbols, Nat
<1>2. f[y] = "a" \/ f[y] = "b"
    BY DEF TypeOK, symbols, Nat
<1>3. f[z] = "a" \/ f[z] = "b"
    BY DEF TypeOK, symbols, Nat
<1>. QED BY <1>1,<1>2,<1>3, SMT DEF TypeOK, TwoEqualSymbols, Nat




=============================================================================
\* Modification History
\* Last modified Wed Jan 07 09:46:10 CET 2015 by marty
\* Created Tue Jan 06 11:16:11 CET 2015 by marty

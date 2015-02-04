------------------------------- MODULE Choose -------------------------------

CONSTANT A(_), B(_)

OP ==  \A x : A(x) \/ B(x)
(* The manual says BY is followed by proof steps and optionally DEF statements. Apparently the parser SANY accepts it otherwise too in some cases. *)

LEMMA OP => CHOOSE n : A(n) \/ B(n)
<1>1. ASSUME NEW x, A(x) \/ B(x) PROVE CHOOSE n : A(n) \/ B(n)
     <2>1. ASSUME A(x) PROVE A(x) \/ B(x) BY A(x)
     <2>2. ASSUME B(x) PROVE A(x) \/ B(x) BY B(x)
     <2> QED
<1>2. QED BY <1>1 DEF OP \* this one is accepted by tlapm and sany

LEMMA ASSUME NEW VARIABLE x PROVE (OP => A(x)) \/ (OP => B(x))


LEMMA ASSUME NEW VARIABLE x PROVE A(x) => CHOOSE n : A(n)
  <1>1. SUFFICES ASSUME A(x)
               PROVE  CHOOSE n : A(n)
    OBVIOUS
  <1> QED OBVIOUS 
  

LEMMA ASSUME NEW x, A(x) PROVE \A y : A(y) BY A(x)


=============================================================================
\* Modification History
\* Last modified Thu Jan 22 14:57:09 CET 2015 by marty
\* Created Tue Jan 20 10:40:06 CET 2015 by marty

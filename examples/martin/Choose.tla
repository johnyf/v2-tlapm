------------------------------- MODULE Choose -------------------------------

CONSTANT A(_), B(_)

OP ==  \A x : A(x) \/ B(x)
(* The manual says BY is followed by proof steps and optionally DEF statements. Apparently the parser SANY accepts it otherwise too in some cases. *)

LEMMA ASSUME NEW VARIABLE x PROVE A(x) => A(CHOOSE n : A(n)) OBVIOUS
LEMMA ASSUME \E x : A(x) PROVE A(CHOOSE n : A(n)) OBVIOUS


LEMMA OP => \E n: A(n) \/ B(n) BY DEF OP

LEMMA ASSUME NEW VARIABLE x PROVE (OP => A(x)) \/ (OP => B(x)) BY DEF OP

LEMMA \A x : (OP => A(x)) \/ (OP => B(x)) BY DEF OP

LEMMA ASSUME NEW x, \A y : A(y) PROVE A(x) OBVIOUS

LEMMA ASSUME NEW x, A(x) PROVE \A y : A(y) BY A(x) \* not provable since \A y quantifiers over all levels

LEMMA ASSUME NEW VARIABLE x, A(x) PROVE \A y : A(y) BY A(x) \* again not provable


=============================================================================
\* Modification History
\* Last modified Wed Feb 11 14:38:13 CET 2015 by marty
\* Created Tue Jan 20 10:40:06 CET 2015 by marty

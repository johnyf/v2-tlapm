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

LEMMA ASSUME NEW x, A(x) PROVE \A y : A(y) OMITTED \* not provable since the new x in the assumption is an existential witness, not a universal one

LEMMA ASSUME 
  ASSUME NEW x 
  PROVE A(x) 
PROVE \A y : A(y) OBVIOUS \* but this is provable


LEMMA ASSUME NEW VARIABLE x, A(x) PROVE \A y : A(y) OMITTED \* again not provable

LEMMA ASSUME 
  ASSUME NEW VARIABLE x 
  PROVE A(x) 
PROVE \A y : A(y) OBVIOUS \* but this is provable

LEMMA \E u : (CHOOSE x \in {0,1} : (x=0 /\ x /= 0)) = u OBVIOUS \* careful, choose always returns an element, even though it might not be well defined

LEMMA (\E u : u=(CHOOSE x \in {0,1} : (x=0 /\ x /= 0))) => (\E x \in {0,1} : (x=0 /\ x /= 0)) \* this is not valid  

LEMMA (\E u : u=(CHOOSE x \in {0,1} : (x=0 /\ x /= 0))) /\ \neg (\E x \in {0,1} : (x=0 /\ x /= 0)) OBVIOUS \* .. actually, in this case it is contradictory

(* choose with tuples *)
LEMMA \E t : t = CHOOSE <<x,y>> : x = y OMITTED \* not supported by tlapm v1 
LEMMA ASSUME NEW S PROVE \E t : t = CHOOSE <<x,y>> \in S : x = y OMITTED \* not supported by tlapm v1 

=============================================================================
\* Modification History
\* Last modified Wed Feb 18 16:25:25 CET 2015 by marty
\* Created Tue Jan 20 10:40:06 CET 2015 by marty

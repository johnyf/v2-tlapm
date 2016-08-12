--------------------------- MODULE TestBug130619 ---------------------------

EXTENDS Naturals

(* this alone is parsed *)
  THEOREM TRUE \in {<<x, y>> \in (Nat \X Nat) : TRUE}
  OBVIOUS

(* assing this produces a tlapm parsing error: Operator "T1" not found *)
  CONSTANT Bar(_)
  Foo(S) == {<<T1, T2>> \in Bar(S) \X Bar(S) : (T1 \subseteq T2) /\ (T1 # T2)}
  THEOREM <<1, 2>> \in Foo(Nat)
  OBVIOUS 



=============================================================================
\* Modification History
\* Last modified Mon Jan 19 10:51:37 CET 2015 by marty
\* Created Mon Jan 19 10:49:05 CET 2015 by marty

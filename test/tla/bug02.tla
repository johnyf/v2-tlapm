------------------------------- MODULE bug02 -------------------------------

EXTENDS Naturals

(* LEMMA ASSUME NEW c \in Nat PROVE (c+1)*(c+1) - (c+1) +1 >=  c*c - c +1 OBVIOUS *)

LEMMA ASSUME NEW c \in Nat PROVE TRUE OBVIOUS

\* LEMMA ~ \E x : x+x =1 OBVIOUS

=============================================================================
\* Modification History
\* Last modified Sat Jul 30 12:12:30 CEST 2016 by marty
\* Created Fri Jul 22 11:56:06 CEST 2016 by marty

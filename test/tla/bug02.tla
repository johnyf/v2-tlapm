------------------------------- MODULE bug02 -------------------------------

EXTENDS Naturals

(* LEMMA ASSUME NEW c \in Nat PROVE (c+1)*(c+1) - (c+1) +1 >=  c*c - c +1 OBVIOUS *)

LEMMA ASSUME NEW c \in Nat PROVE TRUE OBVIOUS

=============================================================================
\* Modification History
\* Last modified Mon Aug 08 14:07:09 CEST 2016 by marty
\* Created Fri Jul 22 11:56:06 CEST 2016 by marty

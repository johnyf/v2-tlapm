------------------------------- MODULE nunchaku_tests_negative -------------------------------

\* all of these lemmas are valid. if nunchaku finds a counter-model, it is wrong.

EXTENDS Naturals, TLAPS

VARIABLE u,v,w
CONSTANT a,b,c

LEMMA ASSUME  u = 0, [] [ (u = 0 /\ u' = 1) \/ (u = 1 /\ u' = 0) ]_u PROVE
              [](u=0 \/ u = 1)
<1> ASSUME u = 0 \/ u = 1, (u = 0 /\ u' = 1) \/ (u = 1 /\ u' = 0) PROVE u'=0 \/ u' = 1 BY PTL
<1> (u'=0 \/ u' = 1) <=> (u=0 \/ u = 1)' OBVIOUS
<1> QED OBVIOUS
              
LEMMA TRUE OBVIOUS

LEMMA {a,b} \cap {c} = {} OBVIOUS

LEMMA {{}} # {} OBVIOUS

LEMMA {a,b} = {b,a} OBVIOUS

(* valid in the theory of naturals *)

LEMMA ~ \E x : x+x=1 OBVIOUS

LEMMA ~ \E x \in {0,1,2} : x+x=1 OBVIOUS
    
LEMMA 1 # 2 OBVIOUS


=============================================================================
\* Modification History
\* Last modified Mon Aug 08 13:48:38 CEST 2016 by marty
\* Last modified Fri Jul 22 13:53:14 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu

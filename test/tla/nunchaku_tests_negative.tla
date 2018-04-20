------------------------------- MODULE nunchaku_tests_negative -------------------------------

\* all of these lemmas are valid. if nunchaku finds a counter-model, it is wrong.

EXTENDS Naturals

VARIABLE u,v,w
CONSTANT a,b,c

              
LEMMA TRUE OBVIOUS

LEMMA {a,b} \cap {c} = {} OBVIOUS

LEMMA {{}} # {} OBVIOUS

LEMMA {a,b} = {b,a} OBVIOUS

(* valid in the theory of naturals *)

LEMMA ~ \E x \in Nat : x+x=1 OBVIOUS 

LEMMA ~ \E x \in {0,1,2} : x+x=1 OBVIOUS
    
LEMMA 1 # 2 OBVIOUS


=============================================================================
\* Modification History
\* Last modified Fri Apr 20 15:12:28 CEST 2018 by merz
\* Last modified Mon Aug 08 17:13:59 CEST 2016 by marty
\* Last modified Fri Jul 22 13:53:14 CEST 2016 by Matthieu
\* Created Wed Mar 23 16:18:29 CET 2016 by Matthieu

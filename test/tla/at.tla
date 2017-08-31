--------------------------------- MODULE at ---------------------------------

EXTENDS Naturals, TLC

\* perhaps NoAssign should move to expression testing 
NoAssign == [ test |-> "" ] \* testing empty string assignations
LEMMA NoAssign.test = "" BY DEF NoAssign

VARIABLE f

Init == f = [r1 |-> 100, r2 |-> 20, r3 |-> "abc"]

Next ==  f' = [f EXCEPT !.r1 = @ +2, !.r1 = @+3  ]              \* should we reject this?
Next2 == f' = [f EXCEPT !.r1 = @ +2] @@ [f EXCEPT !.r1 = @+3  ] \* should we reject this?

LEMMA [ f EXCEPT ![42] = 2 * @] = [ f EXCEPT ![42] = 2 * f [42] ] OBVIOUS \* this is from the tla book

LEMMA ASSUME Init, Next PROVE f'.r1 = 103 
<1> QED BY DEF Init, Next

LEMMA ASSUME Init, Next PROVE f'.r1 = 103
<1> QED BY DEF Init, Next


=============================================================================
\* Modification History
\* Last modified Tue Apr 04 10:37:14 CEST 2017 by marty
\* Created Fri Feb 20 13:41:37 CET 2015 by marty

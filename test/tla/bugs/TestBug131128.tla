--------------------------- MODULE TestBug131128 ---------------------------
 EXTENDS Integers, NaturalsInduction 
   
   (* This is exactly GeneralNatInduction unfolded -- it is not proved automatically, because 
      the NEW CONSTANT introduces two constants P_1 and P in the obligations *)
   THEOREM ASSUME NEW CONSTANT P(_), 
                  \A n \in Nat : (\A m \in 0..(n-1) : P(m)) => P(n)
           PROVE  \A n \in Nat : P(n)
   BY GeneralNatInduction

=============================================================================
\* Modification History
\* Last modified Mon Jan 19 14:03:13 CET 2015 by marty
\* Created Mon Jan 19 12:15:23 CET 2015 by marty

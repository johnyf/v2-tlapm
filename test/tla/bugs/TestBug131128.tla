--------------------------- MODULE TestBug131128 ---------------------------
 EXTENDS Integers, NaturalsInduction 
   
   (* This is exactly GeneralNatInduction unfolded -- it is not proved automatically, because 
      the NEW CONSTANT introduces two constants P_1 and P in the obligations *)
   THEOREM ASSUME NEW CONSTANT P(_), 
                  \A n \in Nat : (\A m \in 0..(n-1) : P(m)) => P(n)
           PROVE  \A n \in Nat : P(n)
   BY GeneralNatInduction \* sm 2018-04-20: adding ", Blast" proves it

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 16:10:05 CEST 2018 by merz
\* Last modified Mon Jan 19 14:03:13 CET 2015 by marty
\* Created Mon Jan 19 12:15:23 CET 2015 by marty

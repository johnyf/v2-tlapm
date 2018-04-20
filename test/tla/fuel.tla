-------------------------------- MODULE fuel --------------------------------
EXTENDS Naturals

\* abstract specification of a car which goes on, but stops at some point  
---- MODULE acar ----
VARIABLE x

Spec == x=0 /\ [] [x'=x+1]_x /\ <>[] [FALSE]_x

====

\* concrete implementation with a fueltank which runs out
---- MODULE ccar ----
VARIABLES x

Spec(t) == t \in Nat /\ x = 0 /\ [][t>0 /\ x' = x +1 /\ t' = t +1]_<<x,t>> 
InterfaceSpec == \EE f : Spec(f) 

\* sm: v1-tlapm throws a syntax error when attempting to prove this
THEOREM ASSUME NEW VARIABLE fuel
        PROVE Spec(fuel) => InterfaceSpec BY DEF Spec, InterfaceSpec

====

VARIABLE x

Abstract == INSTANCE acar

Concrete == INSTANCE ccar

\* sm: v1-tlapm doesn't handle this due to \EE being unimplemented
THEOREM Concrete!InterfaceSpec => Abstract!Spec 
  BY DEF Concrete!InterfaceSpec, Concrete!Spec, Abstract!Spec


=============================================================================
\* Modification History
\* Last modified Fri Apr 20 14:45:30 CEST 2018 by merz
\* Last modified Wed Oct 07 18:37:55 CEST 2015 by marty
\* Created Tue Oct 06 17:33:10 CEST 2015 by marty

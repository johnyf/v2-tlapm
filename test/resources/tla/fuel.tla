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

THEOREM ASSUME NEW VARIABLE fuel
        PROVE Spec(fuel) => InterfaceSpec BY DEF Spec, InterfaceSpec

====

VARIABLE x

Abstract == INSTANCE acar

Concrete == INSTANCE ccar

THEOREM Concrete!InterfaceSpec => Abstract!Spec 
  BY DEF Concrete!InterfaceSpec, Concrete!Spec, Abstract!Spec


=============================================================================
\* Modification History
\* Last modified Wed Oct 07 18:37:55 CEST 2015 by marty
\* Created Tue Oct 06 17:33:10 CEST 2015 by marty

--------------------------- MODULE TestBug140521 ---------------------------

EXTENDS TLAPS

   Foo == []TRUE
   THEOREM Foo => [](1=1)
\*   <1>1. SUFFICES ASSUME []Foo \* uncomment this first, compile the full proof 
   <1>1. SUFFICES ASSUME Foo  \* then comment the upper line and uncomment this one, compile everything again
                  PROVE [](1=1) 
     BY PTL DEF Foo
   <1>2. 1=1  
     OBVIOUS
   <1>3. QED  
     BY <1>2, PTL



=============================================================================
\* Modification History
\* Last modified Mon Jan 19 15:51:19 CET 2015 by marty
\* Created Mon Jan 19 15:28:15 CET 2015 by marty

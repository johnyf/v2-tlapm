-------------------------------- MODULE expr --------------------------------
EXTENDS Naturals

String == "abcdef"        \* string node
EmptyString == ""         \* string node
Num == 11233444223        \* numeral node 
\* LargeNum == 11233444223123345555555124  \* numeral node \* TODO: handle big int 
Decimal == 123.22001      \* decimal
\* LargeDecimal == 1221387687123.2200112287897987      \* decimal \* TODO: handle big int
ite == IF 1 THEN TRUE ELSE "somestring" \* an if then else statement

tuple0 == <<>>
tuple1 == <<0,1>>
tuple2 == <<0, <<1,2>> >>
tuple3 == <<0, 1, 2 >>

\* cannot be parsed by TLAPM v1.x
set0 == \A <<>> \in {} : TRUE  
set1 == \A <<x>> \in {} : TRUE 
\* set3 == \A << <<x>>,<<y>> >> \in {} : TRUE \* sany doesn't allow nested tuples
set4 == \A <<x,y>> \in {} : TRUE

qfset1(x) == x \in {}
qfset2(x) == <<x>> \in {}
qfset3(x,y) == <<x,y>> \in {}
qfset4(x,y) == <<x, <<x,y>> >> \in {}
qfset5(x,y) == << <<y,x>>, <<x,y>> >> \in {}
qfset6(x,y) == <<x, x, y, y >> \in {}


=============================================================================
\* Modification History
\* Last modified Tue Dec 08 15:25:44 CET 2015 by marty
\* Created Fri Feb 20 16:04:14 CET 2015 by marty

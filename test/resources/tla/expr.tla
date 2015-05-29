-------------------------------- MODULE expr --------------------------------
EXTENDS Naturals

String == "abcdef"        \* string node
EmptyString == ""         \* string node
Num == 11233444223        \* numeral node 
\* LargeNum == 11233444223123345555555124  \* numeral node \* TODO: handle big int 
Decimal == 123.22001      \* decimal
\* LargeDecimal == 1221387687123.2200112287897987      \* decimal \* TODO: handle big int
ite == IF 1 THEN TRUE ELSE "somestring" \* an if then else statement


=============================================================================
\* Modification History
\* Last modified Fri Feb 20 16:56:33 CET 2015 by marty
\* Created Fri Feb 20 16:04:14 CET 2015 by marty

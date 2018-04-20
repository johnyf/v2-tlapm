----------------------- MODULE extends_use_def_inner -----------------------

D(x) == x'

USE DEF D

\* fails, obviously
THEOREM T == FALSE OBVIOUS

USE 1=1

=============================================================================
\* Modification History
\* Last modified Fri Apr 20 14:39:04 CEST 2018 by merz
\* Last modified Wed Jan 27 10:51:38 CET 2016 by marty
\* Created Tue Jan 26 15:15:50 CET 2016 by marty

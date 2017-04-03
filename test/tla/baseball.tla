------------------------------ MODULE baseball ------------------------------

(* Algorithmic Baseball is played on N (usually four) bases which are arranged
   in a circle. Each base is assigned to a team and hosts up to two players.
   Team 1 has only one player and all other teams have two players. In the
   beginning, players are randomly distributed over the bases. Since team 1
   has only one player, exactly one position is open. Each turn, a player
   from an adjacent base may move to the empty spot.
   
   The question now is: independent of the starting arrangement, is there
   a series of moves such that in the end each player ends up on their home
   base.
*)


EXTENDS Naturals

CONSTANT N \* Number of bases, each team is represented by a color in 1 .. N

ASSUME N \in Nat /\ N > 1

Bases == 1 .. N \* Each base is assigned to a team  
Colors == 0 .. N \* Each spot is either empty (0) or assigned to a team 

VARIABLE base1 \* maps the base to the team of the player on the first spot 
VARIABLE base2 \* maps the base to the team of the player on the second spot


AllDifferent(b) == \A x,y \in Bases : x#y => b[x] # b[y]

\* True iff c occurs once on b1 and not at all on b2
OccursOnce1(c, b1, b2) == \E x \in Bases : 
                        /\ b1[x] = c
                        /\ \A y \in Bases : (y # x) => b1[y] # c
                        /\ \A y \in Bases : b2[y] # c

\* The symmetric closure on OccursOnce1
OccursOnce(c, b1, b2) == OccursOnce1(c, b1, b2) \/ OccursOnce1(c, b2, b1)


OccursTwiceOnSame(b1, b2, color, x, y) ==
           /\ x # y
           /\ b1[x] = color
           /\ b1[y] = color
           /\ \A z \in (Bases \ {x,y}) : b1[z] # color 
           /\ \A z \in Bases : b2[z] # color

OccursTwiceOnDifferent(b1, b2, color, x, y) ==
           /\ b1[x] = color
           /\ b2[y] = color
           /\ \A z \in (Bases \ {x}) : b1[z] # color
           /\ \A z \in (Bases \ {y}) : b2[z] # color


\* True iff all teams but team 1 have exactly two players on the bases 
OccursTwice(b1, b2) ==
     \A color \in 2 .. N :
     \E x,y \in Bases :
        \/ OccursTwiceOnSame(b1, b2, color, x, y)
        \/ OccursTwiceOnSame(b2, b1, color, x, y)
        \/ OccursTwiceOnDifferent(b1, b2, color, x, y)

\* True iff base1 and base2 are functions assigning bases to colors,
\* Team 1 has exactly one player on the field, all other teams have
\* exactly two players on the field and one position is open.
TypeOK == /\ base1 \in [Bases -> Colors]
          /\ base2 \in [Bases -> Colors] 
          /\ OccursOnce(0, base1, base2)
          /\ OccursOnce(1, base1, base2)
          /\ OccursTwice(base1, base2)

right_of(n) == 1 + ((n + 1) % N)
left_of(n)  == 1 + ((n + (N-1)) % N)

\* Action for moving a player from left/rightto the empty spot on b1[n]
Move1(next(_), n, b1, b2) == 
          /\ b1[n] = 0
          /\ b1' = [b1 EXCEPT ![n] = b1[next(n)], ![next(n)] = 0 ]
          /\ UNCHANGED b2 
\* Action for moving a player from b2[n-1] to the empty spot on b1[n]
Move2(next(_), n, b1, b2) ==
          /\ b1[n] = 0
          /\ b1' = [b1 EXCEPT ![n] = b2[next(n)]]
          /\ b2' = [b2 EXCEPT ![next(n)] = 0 ] 



\* a sample starting position for four bases
Start1 ==
       /\ base1[1 + (3 % N)] = 0
       /\ base2[1 + (6 % N)] = 1
       /\  \A x \in 1..N : /\ base1[x] = 1 + ((x+2) % N)
                           /\ base1[x] = 1 + ((x+5) % N)


\* The initial assignment is just a correct assignment of players to bases
Init == TypeOK

\* All possible moves
Next == \E n \in Bases :
          \/ Move1(right_of, n, base1, base2)
          \/ Move1(right_of, n, base2, base1)
          \/ Move2(right_of, n, base1, base2)
          \/ Move2(right_of, n, base2, base1)
          \/ Move1(left_of,  n, base1, base2)
          \/ Move1(left_of,  n, base2, base1)
          \/ Move2(left_of,  n, base1, base2)
          \/ Move2(left_of,  n, base2, base1)

Spec == Init /\ [] [Next]_<<base1, base2>>

\* The game stops if each base hase only members of their own team.
\* Base one must have an empty spot.
Stops == /\ \A x \in 2 .. N : base1[x] = x /\ base2[x] = x 
         /\
          \/ (base1[1] = 0 /\ base2[1] = 1)
          \/ (base1[1] = 1 /\ base2[1] = 0)



\* In each spec, the game eventually stops
Inv == Spec ~> Stops


=============================================================================
\* Modification History
\* Last modified Mon Apr 03 11:08:23 CEST 2017 by marty
\* Created Thu Mar 30 21:33:35 CEST 2017 by marty

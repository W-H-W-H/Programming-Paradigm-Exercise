/*
    This file contains Exercise 4-LP.5 to Exercise 4-LP.10
*/

% Exercise 4-LP.5
goal( 
    state(_, _, _, has) 
    ).

% Exercise 4-LP.6
init( 
    state(atDoor,onFloor,atWindow,hasnot)
    ).


% Exercise 4-LP.7

/*
state(Monkey,Pos,Box,Has),

Monkey and Box indicate the location of the monkey and the box in the room,
respectively (at the door, at the window, or in the middle); 

Pos indicates the position of the monkey (on the box or on the floor)
*/

% Arrangement affect the result?

% (2)
move(
    state(inMiddle,onBox,inMiddle,hasnot),
    grasp,
    state(inMiddle,onBox,inMiddle,has)
).

% (1)
move( 
    state(Pos, onFloor, Pos, Has),
    climb,
    state(Pos, onBox, Pos, Has) 
    ).

% (4)
move(
    state(L1,onFloor,L1,Has),
    push(L1,L2),
    state(L2,onFloor,L2,Has)
).

% (3)
move(
    state(L1,onFloor,Box,Has),
    walk(L1,L2),
    state(L2,onFloor,Box,Has)
).

% Exercise 4-LP.8
solve(
    state(_, _, _, has) 
).

% Exercise 4-LP.9: Switching two clauses may make the prog never terminated
solve(State1) :- move(State1,_, State2), solve(State2).

% Compound Query init(S), solve(S).

% Exercise 4-LP.10
solve(
    state(_, _, _, has),
    []
).

solve(S1, [Move|L]) :- move(S1,Move,S2), solve(S2, L).

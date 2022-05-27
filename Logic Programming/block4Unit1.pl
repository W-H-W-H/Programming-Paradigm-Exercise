% The original filename is royal_family_facts.pl

/*
    This file contains Exercise 4-LP.1 and Exercise 4-LP.4
*/

mother(emma,wilhelmina).
mother(wilhelmina,juliana).
mother(juliana,beatrix).
mother(juliana,margriet).
mother(juliana,irene).
mother(juliana,christina).
mother(margriet,maurits).
mother(margriet,bernhard_jr).
mother(margriet,pieter_christiaan).
mother(margriet,floris).
mother(beatrix,alexander).
mother(beatrix,friso).
mother(beatrix,constantijn).
mother(maxima,amalia).
mother(maxima,alexia).
mother(maxima,ariane).

husband(bernhard,juliana).
husband(claus,beatrix).
husband(pieter,margriet).
husband(alexander,maxima).
husband(friso,mabel).
husband(constantijn,laurentien).

female(irene).
female(christina).
female(amalia).
female(alexia).
female(ariane).
female(X) :- mother(X,_).
female(X) :- husband(_,X). 

male(maurits).
male(bernhard_jr).
male(pieter_christiaan).
male(floris).
male(X) :- husband(X,_).

% Exercise 4-LP.1

father(X,Y) :- husband(X,Z), mother(Z,Y). % This rule OK

% child/2
child(X,Y) :- father(Y,X) ; mother(Y,X). % This rule OK

% Helper
grandfather(X,Y) :- child(Y,Z),child(Z,X),male(X).
grandmother(X,Y) :- child(Y,Z),child(Z,X),female(X).

% grandparent/2.
grandparent(X,Y) :- grandfather(X,Y) ; grandmother(X,Y). 

% Exercise 4-LP.2

% brother/2
brother(X,Y) :- child(X,Z), child(Y,Z), male(X), male(Y), X \= Y. 

% aunt/2
aunt(X,Y) :- female(X), child(Y, Z), ( sibling(X,Z) ; ( sibling(U,Z) , husband(U, X) ) ).

% Helper
spouse(X,Y) :- husband(X,Y) ; husband(Y,X).
sibling(X,Y) :- child(X,Z), child(Y,Z), X \= Y. 
uncle(X, Y) :- ( brother(X, Z), child(Y, Z) ); ( aunt(Z,Y) , husband(X,Z) ).

% cousin/2
cousin(X,Y) :- ( uncle(Z,Y) ; aunt(Z,Y) ) , child(X, Z).


% A sibling-in-law is the spouse of one's sibling, or the sibling of one's spouse, 
sibling-in-law(X,Y) :- ( spouse(X,Z) , sibling(Y,Z) ) ;  ( spouse(Y,Z) , sibling(X,Z) ) .

% nephew/2
nephew(X, Y) :- male(X), child(X, Z), (sibling(Y, Z) ; sibling-in-law(Y, Z)).

% Exercise 4-LP.3
% Same person match rules multiple times

% Exercise 4-LP.4
ancestor(X,Y) :- grandparent(X, Z) , child(Y, Z). % Base
ancestor(X,Y) :- child(Y, Z), ancestor(X, Z).

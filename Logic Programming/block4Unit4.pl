% Exercise 4-LP.24

:- use_module(library(clpfd)).

/*
    This contains only 4-LP.24
    This exercise isn't completed. Sorry
*/

/*

    1. Arend's Ice Cream wasn't near Twickel. Sherlock didn't get peppermint stick ice cream on Thursday
    night.

    2. He got coffee bean ice cream on Wednesday, but not at Marieke's Ice Cream.
    
    3. At Jaco's Ice Cream he got peanut butter ice cream but not on Tuesday.
    
    4. Sherlock stopped at the ice cream stand near Westerflier on the day before he got the chocolate chip
    ice cream and the day after he stopped at Marco's Ice Cream.
    
    5. He stopped at the stand near Weldam the day before he stopped at Arend's Ice Cream.

*/


theNextDay(tuesday, wednesday).
theNextDay(wednesday, thursday).
theNextDay(thursday,friday).

go(
    [
        stand(tuesday , Name2 , Castle2 , Kind2),
        stand(wednesday , Name3 , Castle3 , coffee_bean),
        stand(thursday , Name4 , Castle4 , Kind4),
        stand(friday , Name5 , Castle5 , Kind5) 
    ]
    
) :- 

    /*
    Names ins [arend, marieke, jaco, marco],
    Castles ins [twickel, westerfilier, weldam],
    Kinds ins [peppermint_stick, peanut_butter, chocolate_chip, coffee_bean],
    */

    icecream( stand(tuesday , Name2 , Castle2 , Kind2) ),
    icecream( stand(wednesday , Name3 , Castle3 , coffee_bean) ),
    icecream( stand(thursday , Name4 , Castle4 , Kind4) ),
    icecream( stand(friday , Name5 , Castle5 , Kind5) )
    .


% Arend's Ice Cream wasn't near Twickel. 
%  i.e. ~(Name == arend AND Castle == twickel).



% Sherlock didn't get peppermint stick ice cream on Thursday night.
icecream( stand(thursday, _, _, Kind) ) :-
    member(Kind, [peanut_butter, chocolate_chip, coffee_bean]).

icecream( stand(Day, _, _, peppermint_stick) ) :-
    member(Day, [tuesday, wednesday, friday]).


% At Jaco's Ice Cream he got peanut butter ice cream but not on Tuesday.
icecream( stand(tuesday , Name , Castle , Kind) ) :-
    member(Name, [arend, marieke, marco] ),
    member(Castle, [twickel, westerfilier, weldam] ),
    member(Kind, [peppermint_stick, chocolate_chip, coffee_bean] ),
    ( (Name = arend) , (Castle \= twickel) ) ; ( (Name \= arend) , (Castle = twickel) ) .

% At Jaco's Ice Cream he got peanut butter ice cream but not on Tuesday.
icecream( stand(Day, jaco, Castle, peanut_butter) ) :-
    member( Day, [wednesday, thursday, friday] ),
    member( Castle, [twickel, westerfilier, weldam] ).

% He got coffee bean ice cream on Wednesday, but not at Marieke's Ice Cream.
icecream( stand(wednesday , Name , Castle , coffee_bean) ) :-
    member( Name, [arend, jaco, marco] ),
    member( Castle, [twickel, westerfilier, weldam] ),
    ( (Name = arend) , (Castle \= twickel) ) ; ( (Name \= arend) , (Castle = twickel) ) .


% Incorrect
% Sherlock stopped at the ice cream stand near Westerflier on the day before he got the chocolate chip
% ice cream and the day after he stopped at Marco's Ice Cream.

icecream( stand(Day3, marco, _, _ ) ) :- 
    theNextDay(Day2, Day3), icecream( stand(Day2, _, _, chocolate_chip) ),
    theNextDay(Day1,Day2), icecream( stand(Day1, _, westerfilier, _) ).

icecream( stand(Day2, _, _, chocolate_chip) ) :- theNextDay(Day1,Day2), icecream( stand(Day1, _, westerfilier, _) ).

% He stopped at the stand near Weldam the day before he stopped at Arend's Ice Cream.
icecream( stand(Day2, arend, _, _) ) :- theNextDay(Day1, Day2), icecream( stand(Day1, _, weldam, _) ).


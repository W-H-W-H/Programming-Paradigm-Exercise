% remember, filename must be start with lowercase

/*
   This file includes 4-LP.12 to 4-LP.20 but not the optional
*/

% Exercise 4-LP.12
istree(nil).
istree( t(L,_,R) ) :- istree(L), istree(R).

% Exercise 4-LP.13

% min/2
min(t(nil,N,_),N).
min( t(L,_,_), N) :- min(L,N).

% max/2
max(t(_,N,nil),N).
max( t(_,_,R), N ) :- max(R, N).

% max(t(t(t(nil, 1, nil), 2, t(nil, 8, nil)), 18, t(t(nil, 21, nil), 81, t(nil, 218, nil))),R).

% Exercise 4-LP.14
issorted(nil).
issorted(t(nil,_,nil)). % forgeting to make this case leads to error result
issorted(t(LeftTree,Value,RightTree)) :- 
   ( LeftTree = nil ; max(LeftTree,LeftMax), (LeftMax =< Value) ), 
   ( RightTree = nil ; min(RightTree,RightMin), (Value =< RightMin) ), 
   issorted(LeftTree), 
   issorted(RightTree).


% Exercise 4-LP.15
% find/3 :: find(Tree, N, Subtree)

find(t(LeftTree, N, RightTree), N, t(LeftTree, N, RightTree)).

find(t(LeftTree, _, RightTree), Target, Subtree) :- ( find(LeftTree, Target, Subtree) ) ; ( find(RightTree, Target, Subtree) ) .

% Exercise 4-LP.16
% insert/3 :: insert(Tree, Target, ResultTree)

insert(nil, Target, t(nil, Target, nil)).

insert( t(LeftTree, Value, RightTree), Target, t(ResultLTree, Value, ResultRTree) ) :- 
   ( Target =< Value ), insert(LeftTree, Target, ResultLTree), ResultRTree = RightTree ;
   ( Target > Value), insert(RightTree, Target, ResultRTree), ResultLTree = LeftTree .


% Exercise 4-LP.17
deleteAll(nil, _, nil). % delete nil yields nil
deleteAll(t(nil, Target, nil), Target, nil). % delete tree with 1 elements

deleteAll(t(LeftTree, Value, RightTree), Target, ResultTree ) :-
   deleteAll(LeftTree, Target, ResultLTree),
   deleteAll(RightTree, Target, ResultRTree),
   (
      % IF Value == Target and both sub-trees are nil
      ( (Value = Target), (ResultLTree = nil), (ResultRTree = nil), ResultTree = nil );
      % IF Value == Target but one of sub-tree is not nil
      ( (Value = Target), (ResultLTree \= nil), (ResultRTree = nil), ResultTree = ResultLTree );
      ( (Value = Target), (ResultLTree = nil), (ResultRTree \= nil), ResultTree = ResultRTree );
      % IF Value \= Target, THEN perserve the node
      ( (Value \= Target), ResultTree = t(ResultLTree, Value, ResultRTree  ) )
   ).


% Exercise 4-LP.18
listtree([],nil). % Empty list yields empty tree

% It has error initially :: Arguments are not sufficiently instantiated
% Order is matter
listtree([Head|Tail], NewTree) :- listtree(Tail, OldTree), insert(OldTree, Head, NewTree). 


% Exercise 4-LP.19
treelist(nil, []). % Empty tree yields empty list

% TA said this is not good way since it is functional programming but not logical
treelist( t(LeftTree, Value, RightTree) , NewList ) :- Singleton = [Value], 
   deleteAll(LeftTree, Value, LeftTree2),
   deleteAll(RightTree, Value, RightTree2),
   treelist(LeftTree2, LeftList), 
   treelist(RightTree2, RightList),
   append(LeftList, Singleton, TempList),
   append(TempList, RightList, NewList).


% Exercise  4-LP.20
% treesort/2, treesort(L1,L2)
treesort([],[]).
treesort([Head|Tail], ResultList ) :- 
   treesort(Tail, PartialList), 
   listtree(PartialList, PartialTree), 
   insert(PartialTree, Head, ResultTree), 
   treelist(ResultTree, ResultList) .

% Q: What happens if L1 contains duplicate entries?
% A: It remove all duplicate entries

% Q: Explain why this happens.
% A: treelist remove all duplicate entries.

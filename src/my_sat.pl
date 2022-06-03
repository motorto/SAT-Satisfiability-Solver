% Yap needs this 
% Gprolog doesn't
% :- use_module(library(lists)).

sat(Expr,Ans) :- 
	get_literals(Expr,Lts),
	solve(Lts,Expr,S),
	sort(S,Ans).

solve([],_,[]). 
solve([X|Xs],As,[R1s|Rs]) :-
	append([[X]],As,R),
	\+absurd(R),
	solve_aux(R,[],R1),
	sort(R1,R1s),
	solve(Xs,As,Rs) , !.
solve([_X|Xs],As,R) :- solve(Xs,As,R), !.

solve_aux([],R,R).
solve_aux(A,Sls,Rs) :-
	sort_sublists(A,As),
	\+absurd(As),
	check_unit(As,L,Asc),
	L \==[],
	propagate(L,Asc,Ap),
	add_solution(L,Sls,Sr),
	solve_aux(Ap,Sr,Rs),!.
solve_aux(A,Sls,Rs) :-
	sort_sublists(A,As),
	\+absurd(As),
	pick_literal(As,L),
	(
		propagate(L,As,R),
		add_solution(L,Sls,Sr) ; get_simtetric(L,M),
		propagate(M,As,R),
		add_solution(M,Sls,Sr)
	),
	solve_aux(R,Sr,Rs),!.

% Adicona na solucao aquele literal a true.
add_solution(K,L,R) :- K\==[], \+member(K-true,L), append([K-true],L,R).

% Gets all literals
% (Expr, ListofLiteral)
get_literals(Expr,X) :- flatten(Expr,R), sort(R,X).

% Picks the first literal of Expr
pick_literal([[X|_Xs]|_Xss],X).

% Returns Literal at the head of Expr if its unitary else
% returns empty
check_unit([[X|Xs]|Xss],X,Xss) :- length(Xs,0), !.
check_unit(X,[],X).

% Propagate Unit
propagate(_A,[],[]).
propagate(A,[X|Xs],R) :-
	member(A,X),
	propagate(A,Xs,R).
propagate(A,[X|Xs],[Xr|R]) :-
	remove_literal(A,X,Xr),
	propagate(A,Xs,R), !.

% (Literal, Expression , NewExpression)
remove_literal(_V,[],[]).
remove_literal(V,[E|Es],[]):- member(V,[E|Es]).
remove_literal(V,[E|Es],R):- member(-V,[E|Es]), delete([E|Es],-V,R),!.
remove_literal(-V,[E|Es],R):- member(V,[E|Es]), delete([E|Es],V,R),!.
remove_literal(_V,[E|Es],[E|Es]).

%converts an atom to its simetric
get_simtetric(-L,L).
get_simtetric(L,-L).

%---------------------------------------------------------
% sorts a list of lists according to length of sublists
%
% Para ordenar a lista vamos usar o keysort pq é ISO standard do prolog
% de acordo com: http://www.gprolog.org/manual/html_node/gprolog044.html#sort%2F2
%
% Para isso temos que transformar a lista para o seguinte formato: 
%
% [Length-SubLista1, Length-SubLista2, ...]. <--- Keygen
% 
% O keysort ordena.
%
% Delete_Key transforma a lista de Length-SubLista1 para Sublista1

sort_sublists([],[]).
sort_sublists(A,R) :- keygen(A,RR), keysort(RR,RRR), undo_keygen(RRR,R).

keygen([],[]).
keygen([X|Xs],[L-X|R]) :- length(X,L), keygen(Xs,R).

undo_keygen([],[]).
undo_keygen([_K-X|Xs],[X|R]) :- undo_keygen(Xs,R).
%---------------------------------------------------------

% Verifica casos absurdos onde A e -A são verdadee 
%
% genero ([a],c1,c2,c3,-a,c4)
absurd([[-X|Xs]|Xss]) :- length([X|Xs],1), member([X],Xss).
absurd([[X|Xs]|Xss]) :- length([X|Xs],1), member([-X],Xss).
absurd([_X|Xs]) :- absurd(Xs),!.

% test ---------------------------------------------------------

testdata([[a,b],[a],[-a,-b,c]]).
testdata([[-p, -q],[-p,q],[p, -q],[p,q]]).
testdata([[p,q,r],[-p, -q, -r],[p, -q, -r2,q,-r],[-p,q],[-p,r],[p, -q,r]]).

test :- 
   testdata(Expr),
   write(Expr), nl,
   sat(Expr,S),
   write(S), nl, nl,
   fail.
test.


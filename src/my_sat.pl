% :- use_module(library(lists)).

sat(Expr,S) :- 
	get_literals(Expr,Lts),
	solve(Lts,Expr,S1),
	sort(S1,S).

solve([],_,[]). 
solve([L|Ls],Exp,[S1|S]) :-
	append([[L]],Exp,Exp2),
	\+absurd(Exp2),
	solve_aux(Exp2,[],S2),
	sort(S2,S1),
	solve(Ls,Exp,S) , !.
solve([_L|Ls],Exp,S) :- 
	solve(Ls,Exp,S), !.

solve_aux([],S,S).
solve_aux(Exp,S,UpSol) :-
	sort_sublists(Exp,ExpS),
	\+absurd(ExpS),
	check_unit(ExpS,L,ExpR),
	L \==[],
	propagate(L,ExpR,ExpP),
	add_solution(L,S,S1),
	solve_aux(ExpP,S1,UpSol),!.
solve_aux(Exp,S,UpSol) :-
	sort_sublists(Exp,ExpS),
	\+absurd(ExpS),
	pick_literal(ExpS,L),
	(
		propagate(L,ExpS,ExpP), add_solution(L,S,S1) 
		;
		get_simtetric(L,Ls), propagate(Ls,ExpS,ExpP),add_solution(Ls,ExpS,S1)
	),
	solve_aux(ExpP,S1,UpSol),!.

add_solution(L,Ls,S) :- 
	L\==[], 
	\+member(L-true,Ls), 
	append([L-true],Ls,S).

get_literals(Expr,X) :- 
	flatten(Expr,R), 
	sort(R,X).

pick_literal([[X|_Xs]|_Xss],X).

check_unit([[L|[]]|Lss],L,Lss).
check_unit(L,[],L).

propagate(_L,[],[]).
propagate(L,[C|Cs],S) :-
	member(L,C),
	propagate(L,Cs,S).
propagate(L,[C|Cs],[S1|S]) :-
	remove_literal(L,C,S1),
	propagate(L,Cs,S), !.

remove_literal(_L,[],[]).
remove_literal(L,[E|Es],[]):- 
	member(L,[E|Es]).
remove_literal(L,[E|Es],R):- 
	member(-L,[E|Es]),
	delete([E|Es],-L,R),!.
remove_literal(-L,[E|Es],R):- 
	member(L,[E|Es]),
	delete([E|Es],L,R),!.
remove_literal(_L,[E|Es],[E|Es]).

get_simtetric(-L,L).
get_simtetric(L,-L).

sort_sublists([],[]).
sort_sublists(Exp,ExpS) :- 
	keygen(Exp,Ek), 
	keysort(Ek,Eks), 
	undo_keygen(Eks,ExpS).

keygen([],[]).
keygen([X|Xs],[L-X|R]) :- 
	length(X,L), 
	keygen(Xs,R).

undo_keygen([],[]).
undo_keygen([_K-X|Xs],[X|R]) :- 
	undo_keygen(Xs,R).

absurd([[-X]|Xss]) :- 
	length([-X],1), 
	member([X],Xss).
absurd([[X]|Xss]) :- 
	length([X],1), 
	member([-X],Xss).
absurd([_X|Xs]) :- 
	absurd(Xs),!.

% tests --------------------------------------------------------

testdata([[a,b],[a],[-a,-b,c]]).
testdata([[-p, -q],[-p,q],[p, -q],[p,q]]).
testdata([[p,q,r],[-p, -q, -r],[p, -q, -r2,q,-r],[-p,q],[-p,r],[p, -q,r]]).
testdata([[q],[w,-r],[s,r],[-y,-q]]).
testdata([[q,-p],[r,-p],[r,q,p],[r,-q,p],[-r,-q,-p],[-r,q,-rp,-q,p]]).

% This one takes longer...
testdata([[q,-p,-t],[x],[y,j],[y,-u],[t,-j],[g,-j],[u,p],[u,-r],[-y,-x],[l,-q,-p],[r,-l,-g]]).

test :- 
   testdata(Expr),
   write(Expr), nl,
   sat(Expr,S),
   write(S), nl, nl,
   fail.
test.


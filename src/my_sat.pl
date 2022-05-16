sat(Expr,Solution) :- split(Expr,ClausesTmp),flatten(ClausesTmp,VarList), dpll(Clauses,Solution).
	% , sort(VarList,Solution).

split(Expr,[Expr]) :- (atom(Expr); Expr=..[-,_]), !.
split(Expr,[Split|Tailsplit]) :-  Expr=..[*,NewList,Ignore] , split_inside_clause(Ignore,Split) ,split(NewList,Tailsplit), !.
split(Expr,[Split]) :- Expr=..[+,_,_], split_inside_clause(Expr,Split), !.

split_inside_clause(X,[X]) :- (atom(X); X=..[-,_]), !.
split_inside_clause(X,[Clause|TailClause]) :- X=..[+,NewList,Clause], split_inside_clause(NewList,TailClause), !.

% A = [[a,b],a]
% A = [[b]]
% sat([[false-X, true-Y], [false-X,false-Z]], [X, Y, Z]).

dpll([],SOL).
dpll([C|CTail],SOL) :- atom(C), append([C-true],SOL,NewSolution), dpll(CTail,NewSolution) , ! .
dpll([C|CTail],SOL) :- dpll(Ctail,SOL) , !.


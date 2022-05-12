sat(Expr,Solution) :- split(Expr,Clauses), dpll(Clauses,Solution).
	% flatten(SimplifiedExpr,VarList), sort(VarList,Solution).

split(Expr,[Expr]) :- (atom(Expr); Expr=..[-,_]), !.
split(Expr,[Split|Tailsplit]) :-  Expr=..[*,NewList,Ignore] , split_inside_clause(Ignore,Split) ,split(NewList,Tailsplit), !.
split(Expr,[Split]) :- Expr=..[+,_,_], split_inside_clause(Expr,Split), !.

split_inside_clause(X,[X]) :- (atom(X); X=..[-,_]), !.
split_inside_clause(X,[Clause|TailClause]) :- X=..[+,NewList,Clause], split_inside_clause(NewList,TailClause), !.

dpll([],SOL) :- write(SOL).
dpll([C|CTail],SOL) :- length(C,1), append([C-true],SOL,NewSolution), dpll(CTail,[[C] |NewSolution]) , !.
dpll([[A,B|_]|Ctail],SOL) :- dpll(Ctail,SOL).

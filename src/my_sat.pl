sat(Expr,Solution) :- createListUnitClauses(Expr,Expr2), 
	flatten(Expr2,Expr3), sort(Expr3, Expr4),
	satAux(Expr4,Expr,Solution).

% propagationOfNonUnitClause([],Solution,Solution).
% propagationOfNonUnitClause([],[],Solution).
% propagationOfNonUnitClause([X|Xs],Expr,Solution) :- propagationOfUnitClause(Expr,X,Expr,Expr2).

satAux([],Solution,Solution).
satAux([],[],Solution).
satAux([X|TailListUnit],Expr,Solution) :- removeUnitClauses(Expr,X,Expr2)
	,propagationOfUnitClause(Expr2,X,Expr2,Expr3) , satAux(TailListUnit,Expr3,Solution), !.

% createListUnitClauses(input,output)
createListUnitClauses([],[]).
createListUnitClauses([C|CTail],[C|S]) :- length(C,1), createListUnitClauses(CTail,S) , ! .
createListUnitClauses([C|CTail],S) :- createListUnitClauses(CTail,S) , !.

% createListUnitClauses(input,output)
removeUnitClauses([],[],[]).
removeUnitClauses(Expr,Var,Expr2) :- delete(Expr,[Var],Expr2).

% propagationOfUnitClause( [[a,b],[-b]] ,).

propagationOfUnitClause([],[],[],[]).
propagationOfUnitClause([],Var,Expr,Expr).
propagationOfUnitClause([E|Tail],Var,[],Sol) :- propagationOfUnitClauseAux(E,Var,E,Expr)
, propagationOfUnitClause(Tail,Var, [Expr], Sol), !.
propagationOfUnitClause([E|Tail],Var,Expr2,Sol) :- propagationOfUnitClauseAux(E,Var,E,Expr)
, propagationOfUnitClause(Tail,Var, [Expr|[]], Sol), !.

propagationOfUnitClauseAux([],[],[],[]).
propagationOfUnitClauseAux([],Var,Expr,Expr).
propagationOfUnitClauseAux([Elem|Tail],Var,Expr,[]) :- Elem == Var.
propagationOfUnitClauseAux([Elem|Tail],Var,Expr,Expr2) :- Elem == -Var, delete(Expr,Elem,Expr2).
propagationOfUnitClauseAux([_|Tail],Var,Expr,Expr2) :- propagationOfUnitClauseAux(Tail,Var,Expr,Expr2).



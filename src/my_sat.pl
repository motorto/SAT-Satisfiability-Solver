sat(Expr,Solution,ListaSol) :- createListUnitClauses(Expr,Expr2,ListaSol), 
	flatten(Expr2,Expr3), sort(Expr3, Expr4),
	satAux(Expr4,Expr,Expr5),delete(Expr5,[],Expr6),
	flatten(Expr6,Expr7),sort(Expr7,Solution).

satAux([],Solution,Solution).
satAux([],[],Solution).
satAux([X|TailListUnit],Expr,Solution) :- removeUnitClauses(Expr,X,Expr2)
	,propagationOfUnitClause(Expr2,X,Expr2,Expr3) , satAux(TailListUnit,Expr3,Solution), !.

list_vars([]). 
list_vars([Var | Vars]) :- 
	list_vars(Vars), (propagationOfUnitClause(Var); propagationOfUnitClause(-Var)).

createListUnitClauses([],[],[]).
createListUnitClauses([C|CTail],[C|S],[C-true|X]) :- length(C,1), createListUnitClauses(CTail,S,X) , ! .
createListUnitClauses([C|CTail],S,X) :- createListUnitClauses(CTail,S,X) , !.

removeUnitClauses([],[],[]).
removeUnitClauses(Expr,Var,Expr2) :- delete(Expr,[Var],Expr2).

propagationOfUnitClause([],Var,Expr,[]).
propagationOfUnitClause([],-Var,Expr,[]).
propagationOfUnitClause([E|Es],-Var,Expr,[SolAux|Sol]):- propagationOfNegativeUnitClauseAux(E,-Var,E,SolAux), propagationOfUnitClause(Es,-Var,Expr,Sol).

propagationOfUnitClause([E|Es],Var,Expr,[SolAux|Sol]):- propagationOfUnitClauseAux(E,Var,E,SolAux),
	propagationOfUnitClause(Es,Var,Expr,Sol), !.

propagationOfUnitClauseAux([],[],[],[]).
propagationOfUnitClauseAux([],Var,Expr,Expr).
propagationOfUnitClauseAux([Elem|Tail],Var,Expr,[]) :- Elem == Var.
propagationOfUnitClauseAux([Elem|Tail],Var,Expr,Expr2) :- Elem == -Var, delete(Expr,Elem,Expr2).
propagationOfUnitClauseAux([_|Tail],Var,Expr,Expr2) :- propagationOfUnitClauseAux(Tail,Var,Expr,Expr2).

propagationOfNegativeUnitClauseAux([],[],[],[]).
propagationOfNegativeUnitClauseAux([],-Var,Expr,Expr).
propagationOfNegativeUnitClauseAux([Elem|Tail],-Var,Expr,[]) :- Elem == -Var.
propagationOfNegativeUnitClauseAux([Elem|Tail],-Var,Expr,Expr2) :- Elem == Var, delete(Expr,Elem,Expr2).
propagationOfNegativeUnitClauseAux([_|Tail],Var,Expr,Expr2) :- propagationOfNegativeUnitClauseAux(Tail,Var,Expr,Expr2).


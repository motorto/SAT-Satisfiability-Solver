sat(Expr,Solution,Resultado) :- createListUnitClauses(Expr,Expr2,ListaSol),
	flatten(Expr2,Expr3), sort(Expr3, Expr4),
	satAux(Expr4,Expr,_,Expr5),delete(Expr5,[],Expr6),checkUnit(Expr6,Resultado,_,ListaSol,Solution).

checkUnit([],[],_,Sol,Sol):- !.
checkUnit(A,A,_,Sol,Sol) :-!.
checkUnit(E,ES,VarUnitarias,ListaSol,Sol1) :- checkUnitAux(E,VarUnitarias,ListaSol,Sol) ,flatten(VarUnitarias,Vars), satAux(Vars,E,Sol,E1),
  	checkUnit(E1,ES,_,Sol,Sol1).




checkUnitAux([],B,A,NewList).
checkUnitAux([],[],A,NewList).
checkUnitAux([C|CTail],[C|[]],X,NewList) :- length(C,1), append(X,[C-true],NewList)
	, checkUnitAux(CTail,S,X,NewList) , ! .
checkUnitAux([C|CTail],S,X,NewList) :- checkUnitAux(CTail,S,X,NewList) , !.


satAux([],Solution,Solution,Solution).
satAux([],[],Solution,[]).
satAux([],A,B,A).
satAux([X|TailListUnit],Expr,Solution,FinalExpr) :- removeUnitClauses(Expr,X,Expr2)
	,propagationOfUnitClause(Expr2,X,Expr2,Expr3) , satAux(TailListUnit,Expr3,Solution,FinalExpr), !.

%list_vars([]).
%list_vars([Var | Vars]) :-
%	list_vars(Vars), (propagationOfUnitClause(Var); propagationOfUnitClause(-Var)).

createListUnitClauses([],[],[]).
createListUnitClauses([C|CTail],[C|S],[C-true|X]) :- length(C,1), createListUnitClauses(CTail,S,X) , ! .
createListUnitClauses([C|CTail],S,X) :- createListUnitClauses(CTail,S,X) , !.

removeUnitClauses([],[],[]).
removeUnitClauses(Expr,Var,Expr2) :- delete(Expr,[Var],Expr2).

propagationOfUnitClause([],Var,Expr,[]).
propagationOfUnitClause([],-Var,Expr,[]).
propagationOfUnitClause([E|Es],-Var,Expr,[SolAux|Sol]):- propagationOfNegativeUnitClauseAux(E,-Var,E,SolAux) , propagationOfUnitClause(Es,-Var,Expr,Sol).

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

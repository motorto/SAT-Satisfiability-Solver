/*
Exemplos:
sat([[a,b],[a],[-a,-b,c]],A,X).
sat([[a,b],[a],[-a,-b,c],[-a,b]],A,X).
sat([[a,b],[a],[-a,-b,c],[d,k]],A,X).

*/

sat(Expr,R,S) :- createListUnitClauses(Expr,Expr2,ListaSol),
	flatten(Expr2,Expr3), sort(Expr3, Expr4),
	satAux(Expr4,Expr,_,Expr5), delete(Expr5,[],Expr6), checkUnit(Expr6,Resultado,_,ListaSol,Solution),
	verifica(Resultado,Solution, R,S).



verifica([], Solution, [], Solution, Solution).
verifica(Resultado, Solution, R, S, S1):- flatten(Resultado,ListaVars),
												sort(ListaVars, NovaListVars),
												removeNeg(NovaListVars, List), todasOp(List, List2),
												propagation(List2, Resultado, Solution, R, [], S1),
												%trolhice
												verifica([], [], [], R, [], S1).
verifica([], [], R, S1, S1).


todasOp([],[]).
todasOp([L|Tail], [L,-L|L1]) :- todasOp(Tail,L1).

/*
%resultado =  exp
%solution = a-true
%list2 = var e -var para todas  b,-b,c,-c
*/


propagation([], _, _,[], Solution2, Solution2).
propagation([List2], Resultado, Solution, Resultado2, Solution2, FIM) :-
					propagationAux(List2, Resultado, Sol),
					append(Solution, [[List2] - true], FinalResult),
					prop([List2], Resultado, Solution, Sol, FinalResult, Solution2).
propagation([List2|TailList], Resultado, Solution, Resultado2, Solution2, FIM) :-
          propagationAux(List2, Resultado, Sol),
					append(Solution, [[List2] - true], FinalResult),
					prop([List2|TailList], Resultado, Solution, Sol, FinalResult, Solution2, FIM).
					
prop([], _, _, Solution2, Solution2).
prop([List2|TailList], Resultado ,Solution, [[]], FinalResult, Solution2, FIM) :-
    propagation(TailList, Resultado, Solution, Sol, [FinalResult|Solution2], FIM).

prop([List2|TailList], Resultado, Solution, Sol, FinalResult, Solution2, FIM) :-
      checkUnit(Sol, SolFinal, VarUnitarias, FinalResult, F),
      propagation(TailList, Resultado, Solution, Sol, [F|Solution2], FIM).

propagationAux(List2, Sol, Sol3) :- propagationOfUnitClause(Sol, List2, Sol, Sol3).
propagationAux(List2, Sol, []) :- !.


satAux([],Solution,Solution,Solution).
satAux([],A,B,A).
satAux([X|TailListUnit],Expr,Solution,FinalExpr) :- removeUnitClauses(Expr,X,Expr2)
	,propagationOfUnitClause(Expr2,X,Expr2,Expr3) , satAux(TailListUnit,Expr3,Solution,FinalExpr), !.
satAux([],[],Solution,[]).

removeNeg([],[]).
removeNeg([-A|As],[A|L]) :- removeNeg(As,L).
removeNeg([A|As],[A|L]) :- removeNeg(As,L).


checkUnit([],[],_,Sol,Sol):- !.
checkUnit(E,ES,VarUnitarias,ListaSol,Sol1) :- checkUnitAux(E,VarUnitarias,ListaSol,Sol), VarUnitarias \==[] ,
        flatten(VarUnitarias,Vars), satAux(Vars,E,Sol,E1), checkUnit(E1,ES,_,Sol,Sol1).
checkUnit(A,A,[],Sol,Sol) :- !.


checkUnitAux([],[],A,NewList).
checkUnitAux([],B,A,NewList).
checkUnitAux([C|CTail],[C|[]],X,NewList) :- length(C,1), append(X,[C-true],NewList)
	, checkUnitAux(CTail,S,X,NewList) , ! .
checkUnitAux([C|CTail],S,X,NewList) :- checkUnitAux(CTail,S,X,NewList) , !.

createListUnitClauses([],[],[]).
createListUnitClauses([C|CTail],[C|S],[C-true|X]) :- length(C,1), createListUnitClauses(CTail,S,X) , ! .
createListUnitClauses([C|CTail],S,X) :- createListUnitClauses(CTail,S,X) , !.

removeUnitClauses([],[],[]).
removeUnitClauses(Expr,Var,Expr2) :- delete(Expr,[Var],Expr2).

propagationOfUnitClause([],Var,Expr,[]).
propagationOfUnitClause([],-Var,Expr,[]).
propagationOfUnitClause([E|Es],-Var,Expr,[SolAux|Sol]):- propagationOfNegativeUnitClauseAux(E,-Var,E,SolAux) ,
                       propagationOfUnitClause(Es,-Var,Expr,Sol).
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

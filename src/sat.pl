% TODO: 
%		* Solver isn't doing nothing

satSolver(Cnf,Solution):- 
	fixValue(Solution),
	solver(Cnf).

fixValue([]).
fixValue([Var|Solution]):-
	(Var = true ; Var = false) , fixValue(Solution).

clauseValue( [-Var | Vars]) :- false.
clauseValue( [Var | Vars]) :- true.

solver([]).
solver([CHead | Ctail]) :-
	solveClause(CHead),
	solver(Ctail).

solveClause(Var,[]) :- Var = clauseValue(Var).
solveClause(Var,[ Var2 | Tail]) :- 
	solveClauseAux(Var,clauseValue(Var), Var2, clauseValue(Var2),Tail).


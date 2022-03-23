% satSolver(Clauses,Solution):- 
% 	fixValue(Solution) , .

fixValue([]).
fixValue([Var|Solution]):-
	(Var = true ; Var = false) , fixValue(Solution).

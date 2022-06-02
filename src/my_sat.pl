% Gnu Prolog não se queixa, 
% mas o yap queixa-se por isso fica aqui comentado 
% para facilitar a vida ao prof. 
% (Sim testei no incrivel compilador yap)
%
%
% :- use_module(library(lists)).

sat(Expr,Ans) :- 
	get_literals(Expr,Lts),
	sol(Lts,Expr,S),
	sort(S,Ans).

sol([],_,[]).
sol([L|Ls],Expr,[S1|S]) :- 
	append([[L]],Expr,TExp), 
	sol_aux(TExp,[],UnsSol), 
	sort(UnsSol,S1), 
	sol(Ls,Expr,S),
	!.

% Exp,Solucao, SolucaoParaCima <-- Talvez exista uma maneira melhor
% Este predicado é o que aplica o "algoritmo DLPP" por assim dizer.
%
% Comecamos por verificar se à cabeça da expressão temos unitarios (Temos
% sempre no primeiro caso pq adicionamos o Literal no "sol"
%
% Propagar ---> Adicionar lá o L-true 
% E chamar recursivamente.
%
% Para a 3 entrada do predicado, é onde está a magia (obrigado Bernardo) 
%
% nos pegamos num literal que está na cabeça da expressão,
%
% [[a,b],[b,c]] ,  ficamos com o "a"
%
% e agora a magia é propagamos para "a" OU propagamos para -a, 
% (fica qual eliminar alguma cena), se eliminarem os dois ficam os dois 
% é tão giro o prolog ...
% e chamamos recursivamente.

sol_aux([[]],S,S). % Está aqui por causa deste caso: --> sat([[c],[b,-c]],S)
sol_aux([],S,S).
sol_aux(Exp,S,UpSolution) :- 
	check_unit(Exp,L,RExp),
	L \==[],
	propagate(L,Exp,ExprP),
	add_solution(L,S,S2),
	sol_aux(ExprP,S2,UpSolution),!.
sol_aux(Exp,S,UpSolution) :- 
	pick_literal(TmpExpr,L),
	(
	(
		propagate(L,Exp,ExprP),
		add_solution(L,S,S2)
	)
	;
	(
		get_simtetric(L,LInverted)),
		propagate(LInverted,Exp,ExprP),
		add_solution(LInverted,S,S2)
	),
	sol_aux(ExprP,S2,UpSolution),!.
	

% Adicona na solucao aquele literal a true.
add_solution(K,L,R) :- K\==[], \+member(K-true,L), append([K-true],L,R).

% Gets all literals
% (Expr, ListofLiteral)
get_literals(Expr,X) :- flatten(Expr,R), sort(R,X).

% Picks the first literal of Expr
pick_literal([[X|Xs]|Xss],X).

% Returns Literal at the head of Expr if its unitary else
% returns empty
check_unit([[X|Xs]|Xss],X,Xss) :- length(Xs,0), !.
check_unit(X,[],X).

% Propagate Unit
propagate(A,[],[]).
propagate(A,[X|Xs],R) :-
	member(A,X),
	propagate(A,Xs,R).
propagate(A,[X|Xs],[Xr|R]) :-
	remove_literal(A,X,Xr),
	propagate(A,Xs,R), !.

% (Literal, Expression , NewExpression)
remove_literal(V,[],[]). % Outro caso que está a bater mas caguei mesmo --> propagate(b,[[b],[]],S).
remove_literal(V,[E|Es],[]):- member(V,[E|Es]).
remove_literal(V,[E|Es],R):- member(-V,[E|Es]), delete([E|Es],-V,R),!. 
remove_literal(-V,[E|Es],R):- member(V,[E|Es]), delete([E|Es],V,R),!.
remove_literal(V,[E|Es],[E|Es]).

%converts an atom to its simetric
get_simtetric(-L,L).
get_simtetric(L,-L).

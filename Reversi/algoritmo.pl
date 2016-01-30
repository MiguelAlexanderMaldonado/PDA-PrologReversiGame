% Autor: 			Guilherme Balena Versiani https://code.google.com/archive/p/tictactoe-prolog/
% Modificado por:	Miguel Alxander Maldonado Lenis

:- module(algoritmo,
        [ 
			minimax/6            % The minimax algorithm
        ]).

:- use_module(reversi,
        [
			moves/3,
			put/8,
			opponent/1,
			me/1
        ]).

:- use_module(heuristica,
        [ 
			value/5,
			win/3
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Minimax
%%%
%%%
%%%     This is a minimax algorithm using alpha-beta pruning
%%%
%%%         minimax(+Board, -BestMove, )
%%%
%%%         where Board is the current board state
%%%               BestMove is the best selected move
%%%               BestValue is the heuristic value of this move
%%%               Branches is the number of branches used
%%%               Depth is the lookahead of the algorithm
%%%
%%%         if there is no more moves available, it fails

:- dynamic bounds/1.

:- asserta(bounds(-10000000/10000000)).
% X1 y X2 son las listas de donde se encuentran las fichas de cada jugador
minimax(Board, BestMove, BestValue, Depth, X1, X2) :-
  Depth > 0,
  bounds(Bounds),  
  alphabeta(Board, Bounds, BestMove, BestValue, Depth, X1, X2, min),!.

alphabeta(Board, _, _, Val, 0, X1, X2, ToMove) :-
  player(ToMove, X),
  heuristica:value(Board, X, Val, X1, X2), !.   % Se inserta la heuristica nueva

alphabeta(Board, Bounds, GoodMove, GoodVal, Depth, X1, X2, ToMove) :-
  min_or_max(ToMove, NextToMove),
  reversi:moves(Board, NextToMove, Moves),     % Obtiene todos los movimientos posibles   
  OneDeeper is Depth - 1,
  boundedbest(Board, Moves, Bounds, GoodMove, GoodVal, OneDeeper, X1, X2, NextToMove), !.

boundedbest(Board, [], Bounds, GoodMove, GoodVal, Depth, X1, X2, ToMove) :- !. % No existe movimiento posible
boundedbest(Board, [Move|TailMoves], Bounds, GoodMove, GoodVal, Depth, X1, X2, ToMove) :-
  player(ToMove, X),
  reversi:put(Board, Move, X, X1, X2, NewBoard, NX1, NX2),    % Nuevo tablero con el movimiento aplicado
  alphabeta(NewBoard, Bounds, _, Val, Depth, NX1, NX2, ToMove),
  goodenough(Board, TailMoves, Bounds, Move, Val, GoodMove, GoodVal, Depth, X1, X2, ToMove), !.

goodenough(_, [], _, Move, Val, GoodMove, Val, _, _, _, _) :- !.

goodenough(_, _, Alpha/Beta, Move, Val, Move, Val, _, _, _, ToMove) :-
  ((ToMove = min, Val > Beta, !)
  ;
  (ToMove = max, Val < Alpha, !)).

goodenough(Board, TailMoves, Bounds, Move, Val, GoodMove, GoodVal, Depth, X1, X2, ToMove)  :-
  newbounds(Bounds, Val, NewBounds, ToMove),
  boundedbest(Board, TailMoves, NewBounds, NewMove, NewVal, Depth, X1, X2, ToMove),
  betterof(NewMove, NewVal, Move, Val, GoodMove, GoodVal, ToMove), !.

newbounds(Alpha/Beta, Val, Val/Beta, ToMove) :-
  ToMove = min, Val > Alpha, !.

newbounds(Alpha/Beta, Val, Alpha/Val, ToMove) :-
  ToMove = max, Val < Beta, !.

newbounds(Alpha/Beta, _, Alpha/Beta, _) :- !.

betterof(Move1, Val1, _, Val2, Move1, Val1, ToMove) :-
  ((ToMove = min, Val1 > Val2, !)
  ;
  (ToMove = max, Val1 < Val2, !)).

betterof(_, _, Move2, Val2, Move2, Val2, _) :- !.

min_or_max(max, min) :- !.
min_or_max(min, max) :- !.

player(max, X) :- % max = it's me!
  reversi:me(X), !.

player(_, X) :-   % min = my opponent!
  reversi:opponent(X), !.


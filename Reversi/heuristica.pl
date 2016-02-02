% Autor: 			Guilherme Balena Versiani https://code.google.com/archive/p/tictactoe-prolog/
% Modificado por:	Miguel Alxander Maldonado Lenis

:- module(heuristica,
        [ value/5  
	    ]).

:- use_module(reversi,
        [ 
			longitud/2,
			maxFilasTablero/2,
			maxColumnasTablero/2
        ]).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%     Heuristics
%%%
%%%
%%%     The h_func calculates the heuristics function for a
%%%     given position of the board.
%%%
%%%         h_func(+Board, +Player, -Weight)
%%%
%%%         where Board is the current board state
%%%               Player is the piece of current player (x / / o)
%%%               Weight is the heuristic value


h_func(Tablero,V,Jugador,ListaJug1,ListaJug2):-
	
	win(Jugador,ListaJug1,ListaJug2,R0),
	(	Jugador = 1 ->
		AuxJug is 2
		;
		AuxJug is 1
	),
	% Cuatos más grupos de fichas hayan del oponente indica una mayr posibilidad de realizar más flanqueos
	fichasAgrupadasVertical(Tablero,AuxJug,R1),
	fichasAgrupadasHorizontal(Tablero,AuxJug,R2),
	NewV is (R0+(R1*R2)),
	V is NewV.

win(Jugador,ListaJug1,ListaJug2,R):-
	reversi:longitud(ListaJug1,L1),
	reversi:longitud(ListaJug2,L2),	
	( 
		Jugador = 1, L1>L2, R=20000;
		Jugador = 2, L2>L1, R=20000;
		R=0
	).
		

fichasAgrupadasVertical(Tablero,Jugador,R):-
	reversi:maxColumnasTablero(Tablero,MC),
	reversi:fichasAgrupadasPorColumna(Tablero,Jugador,MC,0,R).	
		
fichasAgrupadasHorizontal(Tablero,Jugador,R):-
	reversi:maxFilasTablero(Tablero,MF),
	reversi:fichasAgrupadasPorFila(Tablero,Jugador,MF,0,R).	

value(Tablero, Jugador, Val, X1, X2) :-
  h_func(Tablero, W1, 1, X1, X2),
  h_func(Tablero, W2, 2, X2, X1),
  scoreof(W1, W2, Jugador, Val).

scoreof(W1, W2, 1, Val) :-
  Val is W1-W2, !.

scoreof(W1, W2, _, Val) :-
  Val is W2-W1.
% Autor: 			Guilherme Balena Versiani https://github.com/MiguelAlexanderMaldonado/tictactoe-prolog
% Modificado por:	Miguel Alxander Maldonado Lenis

:- module(heuristica,
        [ value/5,               % The heuristic for board state
          win/4                  % Whether some player wins          
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
	fichasAgrupadasVertical(Tablero,Jugador,ListaJug1,ListaJug2,R1),
	fichasAgrupadasHorizontal(Tablero,Jugador,ListaJug1,ListaJug2,R2),
	NewV is (R0+R1+R2),
	V is NewV.
	
win(Jugador,ListaJug1,ListaJug2,R):-
	reversi:longitud(ListaJug1,L1),
	reversi:longitud(ListaJug2,L2),	
	( 
		Jugador = 1, L1>L2, R=200;
		Jugador = 1, L1<L2, R=0;
		Jugador = 2, L1>L2, R=0;
		Jugador = 2, L1<L2, R=200	
	).

fichasAgrupadasVertical(Tablero,Jugador,ListaJug1,ListaJug2,R):-
	reversi:maxColumnasTablero(Tablero,MC),
	reversi:fichasAgrupadasPorColumna(Tablero,Jugador,MC,ListaJug1,ListaJug2,0,R).	
		
fichasAgrupadasHorizontal(Tablero,Jugador,ListaJug1,ListaJug2,R):-
	reversi:maxFilasTablero(Tablero,MF),
	reversi:fichasAgrupadasPorFila(Tablero,Jugador,MF,ListaJug1,ListaJug2,0,R).	

value(Tablero, Jugador, Val, X1, X2) :-
  h_func(Tablero, W1, 1, X1, X2),
  h_func(Tablero, W2, 2, X2, X1),
  scoreof(W1, W2, Jugador, Val).

scoreof(W1, W2, 1, Val) :-
  Val is W1-W2, !.

scoreof(W1, W2, _, Val) :-
  Val is W2-W1.
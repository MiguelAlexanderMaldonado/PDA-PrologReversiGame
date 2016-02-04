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
	
	win(Tablero,Jugador,ListaJug1,ListaJug2,R0),
	fichasAgrupadasVertical(Tablero,Jugador,R1),
	fichasAgrupadasHorizontal(Tablero,Jugador,R2),
	fichasEnEsquinasTablero(Tablero,Jugador,R3),
	fichasEnLateralesTablero(Tablero,Jugador,R4),
	fichasEnCasillasXTablero(Tablero,Jugador,R5),	
	fichasEnFronterasTablero(ListaJug1,ListaJug2,Jugador,R6),
	V is (R0+R1+R2+R3+R4+R5+R6).

win(Tablero,Jugador,ListaJug1,ListaJug2,R):-
	
	reversi:longitud(ListaJug1,L1),
	reversi:longitud(ListaJug2,L2),	
	( 
		Jugador = 1, L1>L2, R=(10000*(L1-L2));
		Jugador = 2, L2>L1, R=(10000*(L2-L1));
		R=0
	).

fichasAgrupadasVertical(Tablero,Jugador,S):-
	reversi:maxColumnasTablero(Tablero,MC),
	reversi:fichasAgrupadasPorColumna(Tablero,Jugador,MC,0,R),
	S is R*500.
		
fichasAgrupadasHorizontal(Tablero,Jugador,S):-
	reversi:maxFilasTablero(Tablero,MF),
	reversi:fichasAgrupadasPorFila(Tablero,Jugador,MF,0,R),
	S is R*500.
		
fichasEnEsquinasTablero(Tablero,Jugador,S):-
	reversi:fichasEnEsquinas(Tablero,Jugador,R),
	S is R*10000.

fichasEnLateralesTablero(Tablero,Jugador,S):-
	reversi:fichasEnLaterales(Tablero,Jugador,R),
	S is R*1000.
		
fichasEnCasillasXTablero(Tablero,Jugador,S):-
	reversi:fichasEnCasillasX(Tablero,Jugador,R),
	S is (R*(-50)).

fichasEnFronterasTablero(ListaJu1,ListaJug2,Jugador,S):-
	reversi:fichasEnFronteras(ListaJu1,ListaJug2,Jugador,R),
	S is (R*(-30)).
	
value(Tablero, Jugador, Val, X1, X2) :-
  h_func(Tablero, W1, 1, X1, X2),
  h_func(Tablero, W2, 2, X1, X2),
  scoreof(W1, W2, Jugador, Val).

scoreof(W1, W2, 1, Val) :-
  Val is W1-W2, !.

scoreof(W1, W2, _, Val) :-
  Val is W2-W1.
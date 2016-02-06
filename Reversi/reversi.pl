% Autor: Miguel Alexander Maldonado Lenis

% 0 == vacio, 1 == jug1 (Jugador), 2 == jug2 (Máquina)

% Tablero inicial
% tablero([0,0,0,0,0,0,0,0]    
%        ,[0,0,0,0,0,0,0,0]   
%        ,[0,0,0,0,0,0,0,0]
% 		 ,[0,0,0,1,2,0,0,0]
% 	     ,[0,0,0,2,1,0,0,0]
% 		 ,[0,0,0,0,0,0,0,0]
% 		 ,[0,0,0,0,0,0,0,0]
% 		 ,[0,0,0,0,0,0,0,0]).

:- module(reversi,
			[
				reversi/0,
				longitud/2,
				maxFilasTablero/2,
				maxColumnasTablero/2,
				moves/4,
				put/8,
				opponent/1,
				me/1
			
			]).

			
:- use_module(algoritmo,
        [ 
			minimax/6            % The minimax algorithm
        ]).

% Limpia la pantalla
limpia:-
        format('~c~s~c~s',[0x1b,"[H",0x1b,"[2J"]).

:- dynamic 	tablero/8,bloqueado/1,listaJug1/1,listaJug2/1.

% Referente a la máquina
me(2).
opponent(1).
			
% ------------------------------------------------------------------------------------------------------------------------	
		
dibujaTablero:-
		% limpia,
		% write('___________________REVERSI_________________________________'),nl,nl,
        tablero([A0,A1,A2,A3,A4,A5,A6,A7]    
				,[B0,B1,B2,B3,B4,B5,B6,B7]   
				,[C0,C1,C2,C3,C4,C5,C6,C7]
				,[D0,D1,D2,D3,D4,D5,D6,D7]
				,[E0,E1,E2,E3,E4,E5,E6,E7]
				,[F0,F1,F2,F3,F4,F5,F6,F7]
				,[G0,G1,G2,G3,G4,G5,G6,G7]
				,[H0,H1,H2,H3,H4,H5,H6,H7]),
		write(' '),write('      '),write('1'),write('     '),write('2'),write('     '),write('3'),write('     '),write('4'),write('     '),write('5'),write('     '),write('6'),write('     '),write('7'),write('     '),write('8'),nl,		
        write('___________________________________________________________'),nl,nl,
		write('1'),write('      '),write(A0),write('     '),write(A1),write('     '),write(A2),write('     '),write(A3),write('     '),write(A4),write('     '),write(A5),write('     '),write(A6),write('     '),write(A7),nl,nl,
		write('2'),write('      '),write(B0),write('     '),write(B1),write('     '),write(B2),write('     '),write(B3),write('     '),write(B4),write('     '),write(B5),write('     '),write(B6),write('     '),write(B7),nl,nl,
		write('3'),write('      '),write(C0),write('     '),write(C1),write('     '),write(C2),write('     '),write(C3),write('     '),write(C4),write('     '),write(C5),write('     '),write(C6),write('     '),write(C7),nl,nl,
		write('4'),write('      '),write(D0),write('     '),write(D1),write('     '),write(D2),write('     '),write(D3),write('     '),write(D4),write('     '),write(D5),write('     '),write(D6),write('     '),write(D7),nl,nl,
		write('5'),write('      '),write(E0),write('     '),write(E1),write('     '),write(E2),write('     '),write(E3),write('     '),write(E4),write('     '),write(E5),write('     '),write(E6),write('     '),write(E7),nl,nl,
		write('6'),write('      '),write(F0),write('     '),write(F1),write('     '),write(F2),write('     '),write(F3),write('     '),write(F4),write('     '),write(F5),write('     '),write(F6),write('     '),write(F7),nl,nl,
		write('7'),write('      '),write(G0),write('     '),write(G1),write('     '),write(G2),write('     '),write(G3),write('     '),write(G4),write('     '),write(G5),write('     '),write(G6),write('     '),write(G7),nl,nl,
		write('8'),write('      '),write(H0),write('     '),write(H1),write('     '),write(H2),write('     '),write(H3),write('     '),write(H4),write('     '),write(H5),write('     '),write(H6),write('     '),write(H7),nl,
		write('___________________________________________________________'),nl,nl,
		write('Para salir del juego introduce un valor menor que 1'),nl,nl.
		
% ------------------------------------------------------------------------------------------------------------------------	

muestraFichas([]):- write('.').
muestraFichas([(F,C)|[]]):- write('('),write(F),write(','),write(C),write('). ').
muestraFichas([(F,C)|Xs]):- write('('),write(F),write(','),write(C),write('), '),muestraFichas(Xs).	
			
% ------------------------------------------------------------------------------------------------------------------------	

% escribeFichasJugadores: Muestra por pantalla las fichas que tienen hasta el momento ambos jugadores en el tablero

escribeFichasJugadores:-

	write('Fichas del jugador 1: '),
	listaJug1(X),
	muestraFichas(X),nl,
	write('Fichas del jugador 2: '),
	listaJug2(Y),
	muestraFichas(Y),nl.

% ------------------------------------------------------------------------------------------------------------------------	
	
% Llamada inicial de la aplicación

reversi:-
	% Se inicializa el tablero
	% assertz permite añadir una nueva cláusula en el programa, esto permite la modificación dinámica del programa
	% assert(tablero( [1,1,1,1,1,1,1,1]   	
	%				,[1,1,1,1,1,1,1,1]		
	%				,[1,1,0,2,1,1,1,1]		
	%				,[1,1,1,1,1,1,1,1]		
	%				,[1,1,1,1,1,1,1,1]		
	%				,[1,1,1,1,1,1,1,1]		
	%				,[1,1,1,1,1,1,1,1]		
	%				,[1,1,1,1,1,1,1,1])),	
		
	assert(tablero(	 [0,0,0,0,0,0,0,0]
					,[0,0,0,0,0,0,0,0]   
					,[0,0,0,0,0,0,0,0]
					,[0,0,0,1,2,0,0,0]
					,[0,0,0,2,1,0,0,0]
					,[0,0,0,0,0,0,0,0]
					,[0,0,0,0,0,0,0,0]
					,[0,0,0,0,0,0,0,0])),
		
		% Este predicado sirve a modo de flag para determinar cuándo se ha terminado el juego
		assertz(juegoTerminado(false)),
		
		% fichas(60): 	Son un total de 60 fichas, tantas como casillas vacías hay en el tablero
		% listaJug1:		En ella se llevará las fichas del jugador 1
		% listaJug2:		En ella se llevará las fichas del jugador 2
		assertz(fichas(60)), 
		% assertz(fichas(1)), 
		assertz(listaJug1([(4,4),(5,5)|[]])), 
		assertz(listaJug2([(4,5),(5,4)|[]])), 
		% assertz(listaJug1([	(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),
		% 					(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),
		% 					(3,2),(3,5),(3,6),(3,7),(3,8),
		% 					(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),
		%					(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),
		%					(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),
		%					(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),
		%					(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8)|[]])), 
		% assertz(listaJug2([(3,4)|[]])), 
				
		% bloqueado:		Indicará si el juego está bloqueado para algún jugador
		assertz(bloqueado(0)),
		
		dificultad(Dificultad),
		dibujaTablero,
		bucleJuego(Dificultad).
% ------------------------------------------------------------------------------------------------------------------------
		
% dificultad(+) Se encarga de guardar la dificultad del juego

dificultad(Dificultad):-
		limpia, 
		write('___________________REVERSI_________________________________'),nl,nl,
		write('Selecciona la dificultad {1. -> Facil | 2. -> Dificil}:'),nl,
		read(Respuesta),
		write('___________________________________________________________'),nl,nl,
		(
			Respuesta == 1 -> 
			Dificultad = 1
			;
			(
				Respuesta == 2 -> 
				Dificultad = 3
				;
				dificultad(Dificultad)
			)		
		).

% ------------------------------------------------------------------------------------------------------------------------		
		
% escribeFichasRestantes(+) Escribe por consola el número de fichas restantes

escribeFichasRestantes(NumFichas):-
	(NumFichas > 0 ->
		write('Quedan '),
		write(NumFichas), 
		write(' Fichas'), nl, nl
	;
		true
	).

% ------------------------------------------------------------------------------------------------------------------------				
	
% damePosicionesAdyacentes(+,-) Devuelve en una lista las posiciones adyacentes a una posición dada

damePosicionesAdyacentes((Fila,Columna),PosicionesAdyacentes):-
		
		tablero(A,B,C,D,E,F,G,H),
		maxFilasTablero([A,B,C,D,E,F,G,H],MF),
		maxColumnasTablero([A,B,C,D,E,F,G,H],MC),
		ColumnaAnterior is Columna - 1,
		ColumnaPosterior is Columna + 1,
		FilaAnterior is Fila - 1,
		FilaPosterior is Fila + 1,
		(
			Fila > MF, PosicionesAdyacentes = [];
			Columna > MC, PosicionesAdyacentes = [];
			
			Fila = 1, Columna = 1, 		PosicionesAdyacentes	= [(Fila,ColumnaPosterior),(FilaPosterior,Columna),(FilaPosterior,ColumnaPosterior)];
			Fila = 1, Columna = MC, 	PosicionesAdyacentes 	= [(FilaPosterior,ColumnaAnterior),(FilaPosterior,ColumnaAnterior),(FilaPosterior,Columna)];
			Fila = 1, Columna > 1, 		PosicionesAdyacentes 	= [(Fila,ColumnaAnterior),(FilaPosterior,ColumnaAnterior),(FilaPosterior,Columna),(FilaPosterior,ColumnaPosterior),(Fila,ColumnaPosterior)];
			
			Fila = MF, Columna = 1, 	PosicionesAdyacentes 	= [(FilaAnterior,Columna),(FilaAnterior,ColumnaPosterior),(Fila,ColumnaPosterior)];
			Fila = MF, Columna = MC, 	PosicionesAdyacentes 	= [(Fila,ColumnaAnterior),(FilaAnterior,ColumnaAnterior),(FilaAnterior,Columna)];
			Fila = MF, Columna > 1, 	PosicionesAdyacentes 	= [(Fila,ColumnaAnterior),(FilaAnterior,ColumnaAnterior),(FilaAnterior,Columna),(FilaAnterior,ColumnaPosterior),(FilaPosterior,ColumnaPosterior)];
			
			Fila > 1, Columna = 1, 		PosicionesAdyacentes	= [(FilaAnterior,Columna),(FilaAnterior,ColumnaPosterior),(Fila,ColumnaPosterior),(FilaPosterior,ColumnaPosterior),(FilaPosterior,Columna)];
			Fila > 1, Columna = MC,		PosicionesAdyacentes 	= [(FilaAnterior,Columna),(FilaAnterior,ColumnaAnterior),(Fila,ColumnaAnterior),(FilaPosterior,ColumnaAnterior),(FilaPosterior,Columna)];
			
			Fila > 1, Columna > 1, 		PosicionesAdyacentes 	= [(Fila,ColumnaAnterior),(FilaAnterior,ColumnaAnterior),(FilaAnterior,Columna),(FilaAnterior,ColumnaPosterior),(Fila,ColumnaPosterior),
																   (FilaPosterior,ColumnaPosterior),(FilaPosterior,Columna),(FilaPosterior,ColumnaAnterior)]		
		)	
		-> true
		;
		false.
	
% ------------------------------------------OPERACIONES SOBRE LISTAS-----------------------------------------------------------			

% contieneElemento(+,+) Acierta si en la lista pasada por parámetro existe el elemento pasado por parámetro.

contieneElemento(X):- false.
contieneElemento(X,[]):- false.
contieneElemento(X,[X|Xs]):- true.
contieneElemento(X,[Y|Ys]):- contieneElemento(X,Ys).

contieneElemento2(X,[]):- false.
contieneElemento2((A,B),[(A,B,_)|Xs]):- true.
contieneElemento2(X,[Y|Ys]):- contieneElemento2(X,Ys).

% existenComunes(+,+) Acierta si en la segunda lista pasada por parámetro existe algún elemento de la primera lista.

existenComunes([],_):-	false.
existenComunes([X|Xs],[X|Ys]):- true.
existenComunes([X|Xs],[Y|Ys]):- 
	(
		contieneElemento(X,Ys) ->
			true
		;
		existenComunes(Xs,[Y|Ys])
	).
	
% borrar(+,+,-) Borra todas las aparaciones de un determinado elemento en una lista 

borrar(_,[],[]).
borrar(X,[X|C],M):-!,borrar(X,C,M).
borrar(X,[Y|L1],[Y|L2]):- borrar(X,L1,L2).	

% eliminaElementos(+,+,-) Borra todas las aparaciones en una lista L2 de los elementos que existen en una lista L1
	
eliminaElementos([],Y,Y).
eliminaElementos([X|Xs],Ys,Zs):-member(X,Ys),borrar(X,Ys,Ts),eliminaElementos(Xs,Ts,Zs).
eliminaElementos([X|Xs],Ys,Zs):-eliminaElementos(Xs,Ys,Zs).
	
% eliminaRepetidos(+,-) Borra los elementos repetidos de una determinada lista
	
eliminaRepetidos([],[]).
eliminaRepetidos([H|T],S):-member(H,T),!,eliminaRepetidos(T,S).
eliminaRepetidos([H|T],[H|S]):-eliminaRepetidos(T,S). 

% insertaElemento(+,+,-) Inserta en una lista un elemento pasado por parámetro

insertaElemento(E,[],[E]).
insertaElemento(E,Xs,[E|Xs]).

% insertaElementos(+,+,-) Inserta en una lista L2 los elementos de una lista L1

insertaElementos([],Y,Y).
insertaElementos([X|Xs],Ys,Z):- insertaElementos(Xs,[X|Ys],Z).

% longitud(+,-) Devuelve la longitud de una lista 

longitud([],0).
longitud([_|L],T):-
longitud(L,T1),
T is T1+1. 	

% dameComunes(+,+,-) Devuelve los elementos comunes a las dos listas que se pasan por parámetro

% Relación auxiliar para establecer cuándo el primer argumento es miembro de la lista segundo argumento
elemento(X,[X|Ys]).
elemento(X,[Y|Ys]):-elemento(X,Ys).

dameComunes([],Xs,[]).
dameComunes([X|Xs],Ys,[X|Zs]):-
	elemento(X,Ys),
	% comprueba si X es elemento de Ys
	dameComunes(Xs,Ys,Zs).

dameComunes([X|Xs],Ys,Zs):-
	not(elemento(X,Ys)),
	% comprueba que X no es elemento de Ys
	dameComunes(Xs,Ys,Zs).
	
	
% --------------------------------------FIN OPERACIONES SOBRE LISTAS--------------------------------------------------------------------
% ------------------------------------------------------------------------------------------------------------------------

% existeFichaOponenteAdyacente(+,+) Comprueba si en las 8 posiciones adyacentes de una casilla existe una ficha del oponente (Los límites son casos particulares)

existeFichaOponenteAdyacente(W,Jug):-
				
		damePosicionesAdyacentes(W,PosicionesAdyacentes),
		
		 (	Jug = 1	->
			listaJug2(X),
			existenComunes(X,PosicionesAdyacentes) % Determina si en las posiciones adyacentes hay una ficha del oponente
			;
			( Jug = 2	->
				listaJug1(Y),
				existenComunes(Y,PosicionesAdyacentes) % Determina si en las posiciones adyacentes hay una ficha del jugador
				;
				false
			)
		).

% ------------------------------------------------------------------------------------------------------------------------

% estaOcupada(+) Acierta si la posición pasada por parámetro ya está ocupada por otra ficha

estaOcupada(W):-
	
	% write('Entra en estaOcupada'),nl,
	listaJug1(X),
	listaJug2(Y),
	( 
		contieneElemento(W,X);
		contieneElemento(W,Y)
	)
	->	
		% write('eO: Posicion ocupada'),nl,
		true
	;
		% write('eO: Posicion no ocupada'),nl,
		false.	
	
% ------------------------------------------------------------------------------------------------------------------------	

% dameFichasAdyacentesJugador(+,+,-) Devuelve la posición de las fichas adyacentes del jugador Jug a la posición (Fila,Columna) 

dameFichasAdyacentesJugador(X,Jug,PosicionesAdyacentes):-

	% write('Entra en dameFichasAdyacentesJugador'),nl,
	listaJug1(W),
	listaJug2(Y),
	(Jug = 1 ->
		damePosicionesAdyacentes(X,Adyacentes),  % Posiciones adyacentes a la posición (Fila,Columna)
		dameComunes(Y,Adyacentes,PosicionesAdyacentes)		% Devuelve las fichas del oponente que están en las posiciones adyacentes
		;
		(Jug = 2 ->
			damePosicionesAdyacentes(X,Adyacentes),
			dameComunes(W,Adyacentes,PosicionesAdyacentes)
			;
			PosicionesAdyacentes = []
		)	
	).
	
% ------------------------------------------------------------------------------------------------------------------------

% encuentraFinalFlanqueo(+,+,+,+)	Verifica si un posible recorrido de flanqueo es un flanqueo válido

encuentraFinalFlanqueo(0,_,_,_,Flanqueo,Flanqueo).
encuentraFinalFlanqueo(-1,_,_,_,_,[]).
encuentraFinalFlanqueo((F,C),Jug,NewDespF,NewDespC,FL,Flanqueo):-

	% write('Entra en encuentraFinalFlanqueo '),nl,
	
	listaJug1(X),
	listaJug2(Y),
	
	% (NewF, NewC) Siguiente posición en el recorrido del posible flanqueo
	NewF is F + NewDespF,
	NewC is C + NewDespC,
	(
		Jug = 1, 
				% Comprueba que la siguiente ficha pertenece al oponente
				contieneElemento((F,C),Y),				
				
				% Guarda la ficha
				insertaElemento((F,C),FL,NewFlanqueo),
				
				!,	% Evita la reevaluación de un estado previo cuando se realiza backtraking
				% Continua el recorrido
				encuentraFinalFlanqueo((NewF,NewC),Jug,NewDespF,NewDespC,NewFlanqueo,Flanqueo);
		
		Jug = 1, 
				% Comprueba que la siguiente ficha pertenece al jugador -> Final del flanqueo
				contieneElemento((F,C),X),
				encuentraFinalFlanqueo(0,Jug,NewDespF,NewDespC,FL,Flanqueo);
		
				% Se hacen las mismas verificaciones para los recorridos de flanqueos de la máquina
		
		Jug = 2, 
		
				contieneElemento((F,C),X), 
				
				% Guarda la ficha
				insertaElemento((F,C),FL,NewFlanqueo),
				
				!,	% Evita la reevaluación de un estado previo cuando se realiza backtraking
				% Continua el recorrido
				encuentraFinalFlanqueo((NewF,NewC),Jug,NewDespF,NewDespC,NewFlanqueo,Flanqueo);
		
		Jug = 2, 
				
				contieneElemento((F,C),Y),
				encuentraFinalFlanqueo(0,Jug,NewDespF,NewDespC,FL,Flanqueo)
		
	) 	-> 	true		
		;
		encuentraFinalFlanqueo(-1,Jug,NewDespF,NewDespC,FL,Flanqueo).
	
% ------------------------------------------------------------------------------------------------------------------------			

% comprobarFlanqueos(+,+,+) Mira si existe falqueo en todas las posibles direcciones dibujadas tomando como origen la ficha (Fila,Columna) 
%							y como sentido todas las fichas adyacentes del oponente

comprobarFlanqueos(X,[],Jug,FichasACambiar,FichasACambiar).
comprobarFlanqueos((Fila,Columna),[(F,C)|Xs],Jug,FAC,FichasACambiar):-
	
	% write('Entra en comprobarFlanqueos'),nl,nl,
		
	% Se recalcula el desplazamiento en cada llamada ya que puede cambiar el desplazamiento con un nuevo recorrido de flanqueo
	SumFila is (Fila - F),
	SumColumna is (Columna - C),	
	NewDespF is (SumFila * (-1)),
	NewDespC is (SumColumna * (-1)),
	
	% Si existe un flanqueo válido, se inserta el recorrido el lista de elementos a cambiar de propietario
	encuentraFinalFlanqueo((F,C),Jug,NewDespF,NewDespC,[],Flanqueo),
	(	not(Flanqueo = []) ->
			insertaElementos(Flanqueo,FAC,NewFAC),
			comprobarFlanqueos((Fila,Columna),Xs,Jug,NewFAC,FichasACambiar)
		;
		% Si no existe un flanqueo válido, se elimina de la lista auxiliar el recorrido guardado hasta el momento
			comprobarFlanqueos((Fila,Columna),Xs,Jug,FAC,FichasACambiar)
	).
	% ,
	% comprobarFlanqueos((Fila,Columna),Xs,Jug). 
	
% ------------------------------------------------------------------------------------------------------------------------			

% existeFlanqueo(+,+) Comprueba si al menos existe un flaqueo, es decir, si partiendo de la ficha (Fila,Columna) y siguiendo una
% 						de las posibles direcciones en las que hay adyacente al menos una ficha del oponente nos encontraremos con otra ficha del jugador.

existeFlanqueo(X,Jug,FichasACambiar):-

	% write('Entra en existeFlanqueo '),nl,

	dameFichasAdyacentesJugador(X,Jug,Adyacentes),
	% write('eF:	Posiciones adyacentes del oponente: '),muestraFichas(Adyacentes),nl,nl,	
		
	comprobarFlanqueos(X,Adyacentes,Jug,[],FichasACambiar), !,
	
	% Si listaCambiar está vacía indica que no existe flanqueo válido
	(	FichasACambiar = [] ->
			false
		;
			% write('eF:	Posiciones a cambiar de propietario: '),muestraFichas(Y),nl,nl,
			true
	).
		
% ------------------------------------------------------------------------------------------------------------------------			
	
% posicionValida(+,+,+) Comprueba si la posición (Fila,Columna) es válida y devuelve las fichas que cambian de propietario
	
posicionValida(X,Jug,FichasACambiar):-
		
	% Comprueba que la posición no esté ocupada por el oponente
	(estaOcupada(X)	->
		false
		;
		% Verifica que haya al menos una ficha del oponente cerca
		(existeFichaOponenteAdyacente(X,Jug)	->
			% Comprueba que exista al menos un flanqueo válido
			(existeFlanqueo(X,Jug,FichasACambiar) ->
				true
				;
				false			
			)
		)		
	).
	
 % ------------------------------------------------------------------------------------------------------------------------			

% ponFicha(+,+,+,-) Coloca la ficha del jugador en la posición de una fila determinada
 
ponFicha(N,Jug,X,Xc):-
 
		[X0,X1,X2,X3,X4,X5,X6,X7]     	= X,
		
		(
			N=1, Xc = [Jug,X1,X2,X3,X4,X5,X6,X7];
			N=2, Xc = [X0,Jug,X2,X3,X4,X5,X6,X7];
			N=3, Xc = [X0,X1,Jug,X3,X4,X5,X6,X7];
			N=4, Xc = [X0,X1,X2,Jug,X4,X5,X6,X7];
			N=5, Xc = [X0,X1,X2,X3,Jug,X5,X6,X7];
			N=6, Xc = [X0,X1,X2,X3,X4,Jug,X6,X7];
			N=7, Xc = [X0,X1,X2,X3,X4,X5,Jug,X7];
			N=8, Xc = [X0,X1,X2,X3,X4,X5,X6,Jug]
		)	->	true
		;
		false. 

		
% cambiaFicha(+,+,+,-) Cambia la ficha de la posicion (Fila,Columna) de propietario 

cambiaFicha([A,B,C,D,E,F,G,H],Fila,Columna,Jug,[Ac,Bc,Cc,Dc,Ec,Fc,Gc,Hc]):-
		 
	(	
		Fila = 1,ponFicha(Columna,Jug,A,Ac),Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
		Fila = 2,ponFicha(Columna,Jug,B,Bc),Ac = A, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
		Fila = 3,ponFicha(Columna,Jug,C,Cc),Ac = A, Bc = B, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
		Fila = 4,ponFicha(Columna,Jug,D,Dc),Ac = A, Bc = B, Cc = C, Ec = E, Fc = F, Gc = G, Hc = H;
		Fila = 5,ponFicha(Columna,Jug,E,Ec),Ac = A, Bc = B, Cc = C, Dc = D, Fc = F, Gc = G, Hc = H;
		Fila = 6,ponFicha(Columna,Jug,F,Fc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Gc = G, Hc = H;
		Fila = 7,ponFicha(Columna,Jug,G,Gc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Hc = H;
		Fila = 8,ponFicha(Columna,Jug,H,Hc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G
	)	-> true
	;
	false.

% cambiaFicha(+,+,+,-) Cambia las fichas de la lista pasada por parámetro de propietario
	
cambiaFichas(Tablero,[],Jug,Tablero).
cambiaFichas(Tablero,[(F,C)|Xs],Jug,NewTablero):- cambiaFicha(Tablero,F,C,Jug,AnotherTablero), cambiaFichas(AnotherTablero,Xs,Jug,NewTablero).

			
% ------------------------------------------------------------------------------------------------------------------------			
	
% colocaFicha(+,+,+,+,-) Coloca la ficha del jugador Jug en la posicion (Fila,Columna) y devuelve las fichas que cambian de propietario 

colocaFicha([A,B,C,D,E,F,G,H],(Fila,Columna),Jug,[Ac,Bc,Cc,Dc,Ec,Fc,Gc,Hc],FichasACambiar):-
		
		% Comprueba que la posición es válida
		(posicionValida((Fila,Columna),Jug,FichasACambiar) ->
			(	
				Fila = 1,ponFicha(Columna,Jug,A,Ac),Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
				Fila = 2,ponFicha(Columna,Jug,B,Bc),Ac = A, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
				Fila = 3,ponFicha(Columna,Jug,C,Cc),Ac = A, Bc = B, Dc = D, Ec = E, Fc = F, Gc = G, Hc = H;
				Fila = 4,ponFicha(Columna,Jug,D,Dc),Ac = A, Bc = B, Cc = C, Ec = E, Fc = F, Gc = G, Hc = H;
				Fila = 5,ponFicha(Columna,Jug,E,Ec),Ac = A, Bc = B, Cc = C, Dc = D, Fc = F, Gc = G, Hc = H;
				Fila = 6,ponFicha(Columna,Jug,F,Fc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Gc = G, Hc = H;
				Fila = 7,ponFicha(Columna,Jug,G,Gc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Hc = H;
				Fila = 8,ponFicha(Columna,Jug,H,Hc),Ac = A, Bc = B, Cc = C, Dc = D, Ec = E, Fc = F, Gc = G
			)	-> true
			;
			false
		;
			false
		).
		
% ------------------------------------------------------------------------------------------------------------------------	

% cambiarFichasDePropietario(+,+) Cambia de propietario las fichas de todos los flanqueos válidos, se pasa por parámetro las fichas a cambiar
		
cambiarFichasDePropietario(Jug1,Jug2,X,Jug,NewJug1,NewJug2):-

	( 	Jug = 1 ->
			insertaElementos(X,Jug1,NewJug1),			
			eliminaElementos(X,Jug2,NewJug2)
		;
		(Jug = 2 ->
			insertaElementos(X,Jug2,NewJug2),			
			eliminaElementos(X,Jug1,NewJug1)
		;		
			false
		)
	).
	
		
% ------------------------------------------------------------------------------------------------------------------------	
	
% colocaFichaJugador(+,+) Coloca la ficha del usuario en la posicion especificada por parámetros si es una posición válida.

colocaFichaJugador(NumFichas, X):-
	
	tablero(A,B,C,D,E,F,G,H),
	
	% Se comprueba que el movimiento del jugador es válido, es decir, la ficha añadida se ha colocado en una posición válida
	% da auerdo a las reglas del juego
	(colocaFicha([A,B,C,D,E,F,G,H],X,1,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar) ->	   
	   
		% Se registra el nuevo tablero
		retract(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
		assertz(tablero(Ar,Br,Cr,Dr,Er,Fr,Gr,Hr)),

		listaJug1(Y),									% Se "invoca" la lista del jugador para tratarla			
		insertaElemento(X,Y,NewFichas),					% Inserta la posición de la ficha colocada por el jugador a su lista de fichas

		listaJug2(Z),	
		
		cambiarFichasDePropietario(NewFichas,Z,FichasACambiar,1,NewJug1,NewJug2),	% Se cambia las fichas flanqueadas de propietario 

		% Se asertan las nuevas listas
		retract(listaJug1(_Y)),
		assert(listaJug1(NewJug1)),		
		retract(listaJug2(_Z)),
		assert(listaJug2(NewJug2)),
		
		% Se cambian las fichas en el tablero		
		cambiaFichas([Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar,1,[As,Bs,Cs,Ds,Es,Fs,Gs,Hs]),
		retract(tablero(_Ar,_Br,_Cr,_Dr,_Er,_Fr,_Gr,_Hr)),
		assertz(tablero(As,Bs,Cs,Ds,Es,Fs,Gs,Hs)),
		
		
		FichasRestantes is NumFichas - 1,
		retract(fichas(NumFichas)),
		assertz(fichas(FichasRestantes))

		;
		write('Posicion no valida'),nl,
		movimientoJugador(NumFichas)
	).	
		
% ------------------------------------------------------------------------------------------------------------------------			

% jugadorBloqueado(+) Acierta si existe al menos un movimiento válido para el jugador en su turno

jugadorBloqueado(Jug):-
	(	Jug = 1 ->
			AuxJug = min
			;
			AuxJug = max
	),
	tablero(A,B,C,D,E,F,G,H),
	% Devuelve los posibles movimientos que existen en el turno del jugador 
	moves([A,B,C,D,E,F,G,H],AuxJug,[],M),
	( M = [] ->
		true
		;
		!,false % El punto de corte evita los ciclos bloqueando la reevaluación de un estado previmente analizado
	).
		
% ------------------------------------------------------------------------------------------------------------------------	
	
% mueveJugador(+) Determina y válida el movimiento del jugador

movimientoJugador(NumFichas):-
	
	( jugadorBloqueado(1) ->
		bloqueado(X),
		(	X = 2 -> % El oponente también está bloqueado
			haTerminado
			;
			retract(bloqueado(_X)),
			assert(bloqueado(1))
		)		
		;
		write('Introduce la posicion:'),nl,
		write('Fila'),
		read(Fila),
		(Fila < 1 -> 
			haTerminado
			;
			write('Columna'),
			read(Columna),
			(Columna < 1 -> 
				haTerminado
				;
				nl,	write('___________________________________________________________'),nl,nl,
				colocaFichaJugador(NumFichas,(Fila,Columna))
			)
		)
	).

% ------------------------------------------------------------------------------------------------------------------------	
	
% mueveMaquina(+,+) Realiza el algoritmo minimax para determinar la jugada de la maquina.
mueveMaquina(Prof):-
		
	tablero(A,B,C,D,E,F,G,H),
	listaJug1(X1), listaJug2(X2),
	minimax([A,B,C,D,E,F,G,H],(Fila,Columna),_MejorValor,Prof,X1,X2), % Encuentra el mejor movimiento
	(colocaFicha([A,B,C,D,E,F,G,H],(Fila,Columna),2,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar) ->
		write('Movimiento del oponente: '),nl,
		write('Fila|: '),write(Fila),nl,
		write('Columna|: '), write(Columna),nl,nl,
		write('___________________________________________________________'),nl,nl,
		
		fichas(NumFichas),
		FichasRestantes is NumFichas - 1,
		retract(fichas(NumFichas)),
		assert(fichas(FichasRestantes)),
		
		retract(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
		assertz(tablero(Ar,Br,Cr,Dr,Er,Fr,Gr,Hr)),
		   
		listaJug2(X),										% Se "invoca" la lista del jugador para tratarla			
		insertaElemento((Fila,Columna),X,NewFichas),		% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		listaJug1(Y),
		
		cambiarFichasDePropietario(Y,NewFichas,FichasACambiar,2,NewJug1,NewJug2),	% Se cambia las fichas flanqueadas de propietario 
		
		% Se asertan las nuevas listas
		retract(listaJug1(_Y)),
		assert(listaJug1(NewJug1)),		
		retract(listaJug2(_X)),
		assert(listaJug2(NewJug2)),
		
		% Se cambian las fichas en el tablero		
		cambiaFichas([Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar,2,[As,Bs,Cs,Ds,Es,Fs,Gs,Hs]),
		retract(tablero(_Ar,_Br,_Cr,_Dr,_Er,_Fr,_Gr,_Hr)),
		assertz(tablero(As,Bs,Cs,Ds,Es,Fs,Gs,Hs)),
		
		dibujaTablero,
		escribeFichasRestantes(FichasRestantes)
		;
		write('Movimiento del oponente: NO EXISTE ALGUNO POSIBLE'),nl,nl,
		bloqueado(X),
		(	X = 1 ->
		 		haTerminado
		 	;
				retract(bloqueado(_X)),
		 		assert(bloqueado(2))
		)
	).

% ------------------------------------------------------------------------------------------------------------------------
	
% haTerminado Libera toda la memoria de la base de conocimeinto
haTerminado:-
	retractall(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
	retractall(fichas(_)),	
	listaJug1(L1),listaJug2(L2),
	longitud(L1,N1),longitud(L2,N2),
	( N1 < N2 -> 
		write('Has perdido (-___-)'),nl,nl
		;
		( N1 > N2 -> 
			write('Has ganado (^___^) '),nl,nl
			;
			nl,write('Empate (>___<) '),nl,nl
		) 
	),
	retractall(listaJug1(_)),
	retractall(listaJug2(_)),
	write('Otra partida?(y/n)'),nl,nl,
	read(Resp),
	( 
		Resp = y -> 
			reversi 
			;
			write('Bye bye!'),nl,false
	).	
	
% ------------------------------------------------------------------------------------------------------------------------
				
% Bucle principal del juego
		
bucleJuego(Dificultad):-
	fichas(NumFichas),
	movimientoJugador(NumFichas),
	dibujaTablero,
	fichas(NumFichas2),
	escribeFichasRestantes(NumFichas2),
	sleep(0.4),
	mueveMaquina(Dificultad),
	fichas(FichasRestantes),
	(FichasRestantes > 0 ->
		bucleJuego(Dificultad)			
		;
		haTerminado
	).
	
% ------------------------------------------------------------------------------------------------------------------------
	
% ---------------------------------------OPERACIONES ALGORITMO---------------------------------------------------------------------------------

% damePosicionesAdyacentesLibres(+,-):- Devuelve una lista con las posiciones libres adyacentes a una posición dada 

damePosicionesAdyacentesLibres((Fila,Columna), PosicionesAdyacentesLibres):-

	damePosicionesAdyacentes((Fila,Columna),PosicionesAdyacentes),
	
	% Se invocan las fichas de ambos jugadores
	listaJug1(X),
	listaJug2(Y),
	
	% Nos quedamos sólo con las posiciones libres
	eliminaElementos(X,PosicionesAdyacentes,PosicionesSinFichasJug1),
	eliminaElementos(Y,PosicionesSinFichasJug1,PosicionesAdyacentesLibres).

%	existenFlanqueos(+,+) Busca todos los posibles flaqueos existentes para el jugador Jug tomando como punto de partida cada elemento de la lista

buscaFlanqueos([],Jug,Movimientos,Movimientos).
buscaFlanqueos([(Fila,Columna)|Xs],Jug,M,Movimientos):-
			
	(existeFlanqueo((Fila,Columna),Jug,FichasACambiar) ->
		
		( not(contieneElemento((Fila,Columna),M)) ->
			insertaElemento((Fila,Columna),M,NewM),
			buscaFlanqueos(Xs,Jug,NewM,Movimientos)
			;
			buscaFlanqueos(Xs,Jug,M,Movimientos)
		)
	;
		buscaFlanqueos(Xs,Jug,M,Movimientos)
	).

% compruebaMovimientos(+,+) Comprueba todos los movimientos existentes para el jugador Jug, lo cual se determina con la ayuda de las fichas
% 							del oponente ya que debe flanquear estas

compruebaMovimientos([],Jug,Movimientos,Movimientos).
compruebaMovimientos([X|Xs],Jug,M,Movimientos):-

	% write('Entra en compruebaMovimientos'),nl,nl,

	% Se determina las posiciones libres adyacentes a cada ficha del jugador 
	damePosicionesAdyacentesLibres(X, PosicionesAdyacentesLibres),
	
	buscaFlanqueos(PosicionesAdyacentesLibres,Jug,[],MV),
	insertaElementos(MV, M, NM),
	
	compruebaMovimientos(Xs,Jug,NM,Movimientos).

% moves(+,+,-) Devuelve en una lista todos los posibles movimientos del jugador Jug

moves(Tablero,Jug,M,Movimientos):-
	
	( Jug = min ->
			listaJug2(X),
			compruebaMovimientos(X,1,M,Movimientos)
		;
			listaJug1(X),
			compruebaMovimientos(X,2,M,Movimientos)
	).
	

%  	ponFicha(+,+,+,+,+,-,-,-) Devuelve en NuevoTablero el movimiento de posicionar la ficha del Jugador en Pos, 
% 	X1 es la lista de las fichas del jugador 1 y X2 del jugador 2

put(Tablero,X,Jugador,X1,X2,NuevoTablero, NewNX1, NewNX2):-
	
	[A,B,C,D,E,F,G,H] = Tablero,
	colocaFicha([A,B,C,D,E,F,G,H],X,Jugador,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar),
		
	( Jugador  = 1 ->
		insertaElemento(X,X1,NX1),											% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		cambiarFichasDePropietario(NX1,X2,FichasACambiar,1,NewNX1,NewNX2),	% Se cambia las fichas flanqueadas de propietario
		cambiaFichas([Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar,1,[As,Bs,Cs,Ds,Es,Fs,Gs,Hs]) % Se cambia las fichas en el tablero
		;
		insertaElemento(X,X2,NX2),   										% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		cambiarFichasDePropietario(X1,NX2,FichasACambiar,2,NewNX1,NewNX2),	% Se cambia las fichas flanqueadas de propietario
		cambiaFichas([Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar,2,[As,Bs,Cs,Ds,Es,Fs,Gs,Hs]) % Se cambia las fichas flanqueadas de propietario 
	),
	NuevoTablero = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs].
	

% ------------------------------------------OPERACIONES HEURISTICA-----------------------------------------------------------------------

maxFilasTablero(Tablero,MF):-
	longitud(Tablero,MF).

maxColumnasTablero([T|Tablero],MC):-
	longitud(T,MC).

numFichas(NF):-
	fichas(NF).
	
	
% Devuelve el número de fichas del Jugador en una determinada lista
	
dameNumeroFichas([],Jugador,0):-!.
dameNumeroFichas([X|Xs],Jugador,R):-		
	( Jugador = 1 ->
		(	X = 1 ->
			dameNumeroFichas(Xs,Jugador,Raux),
			R is Raux + 1
			;
			dameNumeroFichas(Xs,Jugador,R)
		)
		;
		( 	X = 2 ->
			dameNumeroFichas(Xs,Jugador,Raux),
			R is Raux + 1
			;
			dameNumeroFichas(Xs,Jugador,R)
		)
	).
	
dameNumeroFichasConsecutivas([],Jugador,L,R):-
	longitud(L,R).

dameNumeroFichasConsecutivas([X|Xs],Jugador,L,R):-		
	( Jugador = 1 ->
		(	X = 1 ->
			insertaElemento(X,L,NewL),
			dameNumeroFichasConsecutivas(Xs,Jugador,NewL,R)
			;
			dameNumeroFichasConsecutivas([],Jugador,L,R)
		)
		;
		( 	X = 2 ->
			insertaElemento(X,L,NewL),
			dameNumeroFichasConsecutivas(Xs,Jugador,NewL,R)
			;
			dameNumeroFichasConsecutivas([],Jugador,L,R)
		)
	).
	
	
dameNumeroFichasAgrupadas([A0,A1,A2,A3,A4,A5,A6,A7],Jugador,R):-		
	
	((A0 = Jugador, A7 = Jugador) ->
		dameNumeroFichasConsecutivas([A0,A1,A2,A3,A4,A5,A6,A7],Jugador,[],R0),
		dameNumeroFichasConsecutivas([A7,A6,A5,A4,A3,A2,A1,A0],Jugador,[],R1),
		(	R0 = R1 ->
			R is R0
			;
			(	R0 > R1 ->
				R is R0
				;
				R is R1
			)		
		)
		;
		( A0 = Jugador ->
			dameNumeroFichasConsecutivas([A0,A1,A2,A3,A4,A5,A6,A7],Jugador,[],R)
			;
			( A7 = Jugador ->
				dameNumeroFichasConsecutivas([A7,A6,A5,A4,A3,A2,A1,A0],Jugador,[],R)
				;
				R is 0
			)
		)
	).
	
% Devuelve el número de fichas agrupadas del Jugador en todas las filas en las cuales las fichas tocan un límite del tablero
	
fichasAgrupadasPorFila([A,B,C,D,E,F,G,H],Jugador,0,R,R).
fichasAgrupadasPorFila([	[A0,A1,A2,A3,A4,A5,A6,A7],
							[B0,B1,B2,B3,B4,B5,B6,B7],
							[C0,C1,C2,C3,C4,C5,C6,C7],
							[D0,D1,D2,D3,D4,D5,D6,D7],
							[E0,E1,E2,E3,E4,E5,E6,E7],
							[F0,F1,F2,F3,F4,F5,F6,F7],
							[G0,G1,G2,G3,G4,G5,G6,G7],
							[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,Fila,R,RFinal):-	
	(
		Fila = 1, dameNumeroFichasAgrupadas([A0,A1,A2,A3,A4,A5,A6,A7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 2, dameNumeroFichasAgrupadas([B0,B1,B2,B3,B4,B5,B6,B7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 3, dameNumeroFichasAgrupadas([C0,C1,C2,C3,C4,C5,C6,C7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 4, dameNumeroFichasAgrupadas([D0,D1,D2,D3,D4,D5,D6,D7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 5, dameNumeroFichasAgrupadas([E0,E1,E2,E3,E4,E5,E6,E7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 6, dameNumeroFichasAgrupadas([F0,F1,F2,F3,F4,F5,F6,F7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 7, dameNumeroFichasAgrupadas([G0,G1,G2,G3,G4,G5,G6,G7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Fila = 8, dameNumeroFichasAgrupadas([H0,H1,H2,H3,H4,H5,H6,H7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas
	),
	NewFila is Fila - 1,
	fichasAgrupadasPorFila([[A0,A1,A2,A3,A4,A5,A6,A7],
							[B0,B1,B2,B3,B4,B5,B6,B7],
							[C0,C1,C2,C3,C4,C5,C6,C7],
							[D0,D1,D2,D3,D4,D5,D6,D7],
							[E0,E1,E2,E3,E4,E5,E6,E7],
							[F0,F1,F2,F3,F4,F5,F6,F7],
							[G0,G1,G2,G3,G4,G5,G6,G7],
							[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,NewFila,RAux,RFinal).
	

% Devuelve el número de fichas del Jugador en todas las columnas en las cuales las fichas tocan un límite del tablero
	
fichasAgrupadasPorColumna([A,B,C,D,E,F,G,H],Jugador,0,R,R).
fichasAgrupadasPorColumna([	[A0,A1,A2,A3,A4,A5,A6,A7],
							[B0,B1,B2,B3,B4,B5,B6,B7],
							[C0,C1,C2,C3,C4,C5,C6,C7],
							[D0,D1,D2,D3,D4,D5,D6,D7],
							[E0,E1,E2,E3,E4,E5,E6,E7],
							[F0,F1,F2,F3,F4,F5,F6,F7],
							[G0,G1,G2,G3,G4,G5,G6,G7],
							[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,Columna,R,RFinal):-	
							
	(
		Columna = 1, dameNumeroFichasAgrupadas([A0,B0,C0,D0,E0,F0,G0,H0],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 2, dameNumeroFichasAgrupadas([A1,B1,C1,D1,E1,F1,G1,H1],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 3, dameNumeroFichasAgrupadas([A2,B2,C2,D2,E2,F2,G2,H2],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 4, dameNumeroFichasAgrupadas([A3,B3,C3,D3,E3,F3,G3,H3],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 5, dameNumeroFichasAgrupadas([A4,B4,C4,D4,E4,F4,G4,H4],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 6, dameNumeroFichasAgrupadas([A5,B5,C5,D5,E5,F5,G5,H5],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 7, dameNumeroFichasAgrupadas([A6,B6,C6,D6,E6,F6,G6,H6],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas;
		Columna = 8, dameNumeroFichasAgrupadas([A7,B7,C7,D7,E7,F7,G7,H7],Jugador,NumeroFichasAgrupadas), RAux is R + NumeroFichasAgrupadas
	),
	NewColumna is Columna - 1,
	fichasAgrupadasPorColumna([	[A0,A1,A2,A3,A4,A5,A6,A7],
								[B0,B1,B2,B3,B4,B5,B6,B7],
								[C0,C1,C2,C3,C4,C5,C6,C7],
								[D0,D1,D2,D3,D4,D5,D6,D7],
								[E0,E1,E2,E3,E4,E5,E6,E7],
								[F0,F1,F2,F3,F4,F5,F6,F7],
								[G0,G1,G2,G3,G4,G5,G6,G7],
								[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,NewColumna,RAux,RFinal).	

																
fichasEnEsquinas([	[A0,A1,A2,A3,A4,A5,A6,A7],
					[B0,B1,B2,B3,B4,B5,B6,B7],
					[C0,C1,C2,C3,C4,C5,C6,C7],
					[D0,D1,D2,D3,D4,D5,D6,D7],
					[E0,E1,E2,E3,E4,E5,E6,E7],
					[F0,F1,F2,F3,F4,F5,F6,F7],
					[G0,G1,G2,G3,G4,G5,G6,G7],
					[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,R):-
					
		dameNumeroFichas([A0,A7,H0,H7],Jugador,R).
		

fichasEnLaterales([	[A0,A1,A2,A3,A4,A5,A6,A7],
					[B0,B1,B2,B3,B4,B5,B6,B7],
					[C0,C1,C2,C3,C4,C5,C6,C7],
					[D0,D1,D2,D3,D4,D5,D6,D7],
					[E0,E1,E2,E3,E4,E5,E6,E7],
					[F0,F1,F2,F3,F4,F5,F6,F7],
					[G0,G1,G2,G3,G4,G5,G6,G7],
					[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,R):-
		
		dameNumeroFichas([A0],Jugador,A),
		dameNumeroFichas([A7],Jugador,B),
		dameNumeroFichas([H0],Jugador,C),
		dameNumeroFichas([H7],Jugador,D),
		( A = 1 ->
			dameNumeroFichas([B0,A1,C0,B1,A2],Jugador,RA)
			;
			RA is 0
		),
		( B = 1 ->
			dameNumeroFichas([B7,A6,C7,B6,A5],Jugador,RB)
			;
			RB is 0
		),
		( C = 1 ->
			dameNumeroFichas([G0,H1,F0,G1,H2],Jugador,RC)
			;
			RC is 0
		),
		( D = 1 ->
			dameNumeroFichas([G7,H6,F7,G6,H5],Jugador,RD)
			;
			RD is 0
		),
		R is (RA+RB+RC+RD).


fichasEnCasillasX([	[A0,A1,A2,A3,A4,A5,A6,A7],
					[B0,B1,B2,B3,B4,B5,B6,B7],
					[C0,C1,C2,C3,C4,C5,C6,C7],
					[D0,D1,D2,D3,D4,D5,D6,D7],
					[E0,E1,E2,E3,E4,E5,E6,E7],
					[F0,F1,F2,F3,F4,F5,F6,F7],
					[G0,G1,G2,G3,G4,G5,G6,G7],
					[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,R):-
		
		dameNumeroFichas([B1],Jugador,A),
		dameNumeroFichas([B6],Jugador,B),
		dameNumeroFichas([G1],Jugador,C),
		dameNumeroFichas([G6],Jugador,D),
		R is (A+B+C+D).
		

dameNumeroFronteras([X|Xs],0):-!.		
dameNumeroFronteras([X|Xs],R):-

	dameNumeroFronteras(Xs,R),
	damePosicionesAdyacentesLibres(X,PA),
	Raux is longitud(PA).
		
		
fichasEnFronteras(Jug1,Jug2,Jugador,R):-
	
	(	Jugador = 1 ->
		dameNumeroFronteras(Jug1,R)
		;
		dameNumeroFronteras(Jug2,R)
	).
	
	
	
	
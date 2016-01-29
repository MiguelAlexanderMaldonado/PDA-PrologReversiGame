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
				moves/3,
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

:- dynamic 	tablero/8,fase/1,bloqueado/1,listaJug1/1,listaJug2/1,maxFila/1,maxColumna/1,
			listaCambiar/1,listaAuxiliar/1,listaMovimientos/1.

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
		write('___________________________________________________________'),nl,nl.
		
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
		
		% listaCambiar:		En ella se llevará las fichas que deben cambiar de propietario
		assertz(listaCambiar([])), 
		
		% listaAuxiliar:	En ella se llevarán elementos que no se quieran perder en algunas operaciones
		assertz(listaAuxiliar([])),
	
		% listaMovimientos:	En ella se llevarán todas las posibles posiciones en la que se puede poner una ficha donde existe al menos un flanqueo válido
		assertz(listaMovimientos([])),	
		
		% MaxFila x MaxColumna son las dimensiones del tablero
		assertz(maxFila(8)),
		assertz(maxColumna(8)),
		
		% bloqueado:		Indicará si el juego está bloqueado para algún jugador
		assertz(bloqueado(0)),
		
		% juegoAcabado:		Indicará si se ha terminado el juego
		assertz(juegoAcabado(false)),
		
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
				Dificultad = 2
				;
				dificultad(Dificultad)
			)		
		).

% ------------------------------------------------------------------------------------------------------------------------		
		
% escribeFichasRestantes(+) Escribe por consola el número de fichas restantes

escribeFichasRestantes(NumPiezas):-
	(NumPiezas > 0 ->
		write('Quedan '),
		write(NumPiezas), 
		write(' Fichas'), nl, nl
	;
		true
	).

% ------------------------------------------------------------------------------------------------------------------------				
	
% damePosicionesAdyacentes(+,+,-) Devuelve las posiciones adyacentes a una posición dada

damePosicionesAdyacentes(Fila,Columna,PosicionesAdyacentes):-
		maxFila(MF),
		maxColumna(MC),
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
	
% ------------------------------------------------------------------------------------------------------------------------			

% contieneElemento(+,+) Mira si en la lista pasada por parámetro contiene el elemento pasado por parámetro.

contieneElemento(X,[]):- false.
contieneElemento(X,[X|Xs]):- true.
contieneElemento(X,[Y|Ys]):- contieneElemento(X,Ys).

contieneElemento2(X,[]):- false.
contieneElemento2((A,B),[(A,B,_)|Xs]):- true.
contieneElemento2(X,[Y|Ys]):- contieneElemento2(X,Ys).

% existenComunes(+,+) Mira si en la segunda lista pasada por parámetro contiene algún elemento de la primera lista.

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

% eliminaElementos(+,+,-) Borra todas las aparaciones en una lista L2 de los elementos de una lista L1
	
eliminaElementos([],Y,Y).
eliminaElementos([X|Xs],Ys,Zs):-member(X,Ys),borrar(X,Ys,Ts),eliminaElementos(Xs,Ts,Zs).
eliminaElementos([X|Xs],Ys,Zs):-eliminaElementos(Xs,Ys,Zs).
	
% eliminaRepetidos(+,-) Borra los elementos repetidos de una determinada lista
	
eliminaRepetidos([],[]).
eliminaRepetidos([H|T],S):-member(H,T),!,eliminaRepetidos(T,S).
eliminaRepetidos([H|T],[H|S]):-eliminaRepetidos(T,S). 

% insertaElemento((+Fila,+Columna),+Lista,-NuevaLista)

insertaElemento(E,[],[E]).
insertaElemento(E,Xs,[E|Xs]).

% insertaElementos((+ListaX,+ListaY,-NuevaLista)

insertaElementos([],Y,Y).
insertaElementos([X|Xs],Ys,Z):- insertaElementos(Xs,[X|Ys],Z).

% damePosElemento(+,+,-) Devuelve la posición en que se encuentra un elemento X en una lista

damePosElemento((A,B),[(A,B,_)|_],0).
damePosElemento(_,[],_):-
!,fail.
damePosElemento(X,[_|R],Pos):-
damePosElemento(X,R,Pos1),
Pos is Pos1+1. 

% sobreescribirEn(+,+,+,-) Sustituye el elemento X en la posición N de una lista por el elemento pasado por parámetro 

sobreescribirEn(_,_,[],[]).
sobreescribirEn((A,B,C),0,[_|R],[(A,B,C)|R1]):-
sobreescribirEn(Elem,-1,R,R1),!.
sobreescribirEn(Elem,Pos,[C|R],[C|R1]):-
ColTemp is Pos -1,
sobreescribirEn(Elem,ColTemp,R,R1). 

% dameValor(+,+,-) Devuelve el valor V de un lemento X siendo; X = (A,B,V) y valor = V 

dameValor((A,B),[(A,B,C)|_],C).
dameValor(_,[],_):-
!,fail.
dameValor(X,[_|R],P):-
dameValor(X,R,P). 
	

% longitud(+,-) Devuelve la longitud de una lista 

longitud([],0).
longitud([_|L],T):-
longitud(L,T1),
T is T1+1. 	
	
% ------------------------------------------------------------------------------------------------------------------------			

% existeFichaOponenteCerca(+,+,+) Comprueba si en las 8 posiciones adyacentes de una casilla existe una ficha del oponente (Los límites son casos particulares)

existeFichaOponenteCerca(Fila,Columna,Jug):-
		
		% write('Entra en existeFichaOponenteCerca'),nl,
		
		damePosicionesAdyacentes(Fila,Columna,PosicionesAdyacentes),
		
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

% estaOcupada(+,+) Mira si la posición (Fila,Columna) ya está ocupada por otra ficha

estaOcupada(Fila,Columna):-
	
	% write('Entra en estaOcupada'),nl,
	listaJug1(X),
	listaJug2(Y),
	( 
		contieneElemento((Fila,Columna),X);
		contieneElemento((Fila,Columna),Y)
	)
	->	
		% write('eO: Posicion ocupada'),nl,
		true
	;
		% write('eO: Posicion no ocupada'),nl,
		false.	
	
% ------------------------------------------------------------------------------------------------------------------------		

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
	
	
% ------------------------------------------------------------------------------------------------------------------------	

% dameFichasAdyacentesJugador(+,+,+,-) Devuelve la posición de las fichas adyacentes del jugador Jug a la posición (Fila,Columna) 

dameFichasAdyacentesJugador(Fila,Columna,Jug,PosicionesAdyacentes):-

	% write('Entra en dameFichasAdyacentesJugador'),nl,
	listaJug1(X),
	listaJug2(Y),
	(Jug = 1 ->
		damePosicionesAdyacentes(Fila,Columna,Adyacentes),  % Posiciones adyacentes a la posición (Fila,Columna)
		dameComunes(Y,Adyacentes,PosicionesAdyacentes)		% Devuelve las fichas del oponente que están en las posiciones adyacentes
		;
		(Jug = 2 ->
			damePosicionesAdyacentes(Fila,Columna,Adyacentes),
			dameComunes(X,Adyacentes,PosicionesAdyacentes)
			;
			PosicionesAdyacentes = []
		)	
	).
	
% ------------------------------------------------------------------------------------------------------------------------

% encuentraFinalFlanqueo(+)	Verifica si un posible recorrido de flanqueo es un flanqueo válido

encuentraFinalFlanqueo(F,C,Jug,NewDespF,NewDespC):-

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
				listaAuxiliar(M), 						
				insertaElemento((F,C),M,NewFlanqueo), 
				retract(listaAuxiliar(_M)), 
				assertz(listaAuxiliar(NewFlanqueo)), 
				
				!,	% Evita la reevaluación de un estado previo cuando se realiza backtraking
				% Continua el recorrido
				encuentraFinalFlanqueo(NewF,NewC,Jug,NewDespF,NewDespC);
		
		Jug = 1, 
				% Comprueba que la siguiente ficha pertenece al jugador -> Final del flanqueo
				contieneElemento((F,C),X);
		
				% Se hacen las mismas verificaciones para los recorridos de flanqueos de la máquina
		
		Jug = 2, 
		
				contieneElemento((F,C),X), 
				
				listaAuxiliar(N), 
				insertaElemento((F,C),N,NewFlanqueo), 
				retract(listaAuxiliar(_N)), 
				assertz(listaAuxiliar(NewFlanqueo)),				
				!,
				% write('eFF:	Sigue la busqueda '),nl,
				encuentraFinalFlanqueo(NewF,NewC,Jug,NewDespF,NewDespC);
		
		Jug = 2, 
				
				contieneElemento((F,C),Y)
		
	) -> true
	;
	% write('eFF:	No existe flanqueo'),nl,
	false.
	
% ------------------------------------------------------------------------------------------------------------------------			

% comprobarFlanqueos(Fila,Columna,Adyacentes,Jug) Mira si existe falqueo en todas las posibles direcciones dibujadas tomando como origen la ficha (Fila,Columna) 
%												y como sentido todas las fichas adyacentes del oponente

comprobarFlanqueos(Fila,Columna,[],Jug).
comprobarFlanqueos(Fila,Columna,[(F,C)|Xs],Jug):-
	
	% write('Entra en comprobarFlanqueos'),nl,nl,
		
	% Se recalcula el desplazamiento en cada llamada ya que puede cambiar el desplazamiento con un nuevo recorrido de flanqueo
	SumFila is (Fila - F),
	SumColumna is (Columna - C),	
	NewDespF is (SumFila * (-1)),
	NewDespC is (SumColumna * (-1)),
	
	(	% Si existe un flanqueo válido, se inserta el recorrido el lista de elementos a cambiar de propietario
		encuentraFinalFlanqueo(F,C,Jug,NewDespF,NewDespC) ->
			listaAuxiliar(O),			
			listaCambiar(P),
			insertaElementos(O,P,AuxCambiar),
			eliminaRepetidos(AuxCambiar,NewCambiar),
			retract(listaAuxiliar(_O)),
			retract(listaCambiar(_P)),
			assertz(listaAuxiliar([])),
			assertz(listaCambiar(NewCambiar))
		;
		% Si no existe un flanqueo válido, se elimina de la lista auxiliar el recorrido guardado hasta el momento
			listaAuxiliar(N), 
			retract(listaAuxiliar(_N)), 
			assertz(listaAuxiliar([]))
	),
	comprobarFlanqueos(Fila,Columna,Xs,Jug). 
	
% ------------------------------------------------------------------------------------------------------------------------			

% existeFlanqueo(+,+,+) Comprueba si al menos existe un flaqueo, es decir, si partiendo de la ficha (Fila,Columna) y siguiendo una
% 						de las posibles direcciones en las que hay adyacente al menos una ficha del oponente nos encontraremos con otra ficha del jugador.

existeFlanqueo(Fila,Columna,Jug):-

	% write('Entra en existeFlanqueo '),nl,

	dameFichasAdyacentesJugador(Fila,Columna,Jug,Adyacentes),
	% write('eF:	Posiciones adyacentes del oponente: '),muestraFichas(Adyacentes),nl,nl,	
		
	% calculaDesplazamientos(Fila,Columna,Adyacentes),	
		
	listaAuxiliar(X),
	retract(listaAuxiliar(_X)),
	assertz(listaAuxiliar([])),
	
	comprobarFlanqueos(Fila,Columna,Adyacentes,Jug), !,
	
	listaCambiar(Z),
	
	% Si listaCambiar está vacía indica que no existe flanqueo válido
	(	Z = [] ->
			false
		;
			% write('eF:	Posiciones a cambiar de propietario: '),muestraFichas(Y),nl,nl,
			true
	).
		
% ------------------------------------------------------------------------------------------------------------------------			
	
% posicionValida(+,+,+) Comprueba si la posición (Fila,Columna) es válida y devuelve las fichas que cambian de propietario
	
posicionValida(Fila,Columna,Jug):-
		
	(estaOcupada(Fila,Columna)	->
		false
		;		
		(existeFichaOponenteCerca(Fila,Columna,Jug)	->
			(existeFlanqueo(Fila,Columna,Jug) ->
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

% cambiaFicha(+,+,+,-) Cambia las fichas de las posiciones de la lista pasado por parámetro de propietario
	
cambiaFichas(Tablero,[],Jug,Tablero).
cambiaFichas(Tablero,[(F,C)|Xs],Jug,NewTablero):- cambiaFicha(Tablero,F,C,Jug,AnotherTablero), cambiaFichas(AnotherTablero,Xs,Jug,NewTablero).

			
% ------------------------------------------------------------------------------------------------------------------------			
	
% colocaFicha(+,+,+,-) Coloca la ficha del jugador Jug en la posicion (Fila,Columna) y devuelve las fichas que cambian de propietario 

colocaFicha([A,B,C,D,E,F,G,H],Fila,Columna,Jug,[Ac,Bc,Cc,Dc,Ec,Fc,Gc,Hc],X):-
		
		listaCambiar(Y),
		retract(listaCambiar(_Y)),
		assertz(listaCambiar([])),
		
		(posicionValida(Fila,Columna,Jug) ->
			listaCambiar(X),
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
		
cambiarFichasDePropietario(X,Jug):-

	tablero(A,B,C,D,E,F,G,H),
	listaJug1(M),
	listaJug2(N),
	
	( 	Jug = 1 ->
			insertaElementos(X,M,NewJug1),
			retract(listaJug1(_M)),
			assert(listaJug1(NewJug1)),
			
			eliminaElementos(X,N,NewJug2),
			retract(listaJug2(_N)),
			assert(listaJug2(NewJug2)),
			
			cambiaFichas([A,B,C,D,E,F,G,H],X,1,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr]),
			retract(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
			assertz(tablero(Ar,Br,Cr,Dr,Er,Fr,Gr,Hr)),
	   
			retract(listaCambiar(_X)),
			assert(listaCambiar([]))
		;
		(Jug = 2 ->
			insertaElementos(X,N,NewJug2),
			retract(listaJug2(_N)),
			assert(listaJug2(NewJug2)),
			
			eliminaElementos(X,M,NewJug1),
			retract(listaJug1(_M)),
			assert(listaJug1(NewJug1)),
			
			cambiaFichas([A,B,C,D,E,F,G,H],X,2,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr]),
			retract(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
			assertz(tablero(Ar,Br,Cr,Dr,Er,Fr,Gr,Hr)),
			
			retract(listaCambiar(_X)),
			assert(listaCambiar([]))
		;		
			false
		)
	).
	
		
% ------------------------------------------------------------------------------------------------------------------------	
	
% colocaFichaJugador(+,+,+) Coloca la ficha del usuario en la posicion especificada por parámetros si es una posición válida.

colocaFichaJugador(NumFichas, Fila, Columna):-
	
	tablero(A,B,C,D,E,F,G,H),
	
	(colocaFicha([A,B,C,D,E,F,G,H],Fila,Columna,1,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar) ->	   
	   
		% Se registra el nuevo tablero
		retract(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
		assertz(tablero(Ar,Br,Cr,Dr,Er,Fr,Gr,Hr)),

		listaJug1(X),									% Se "invoca" la lista del jugador para tratarla			
		insertaElemento((Fila,Columna),X,NewFichas),		% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		retract(listaJug1(_X)),							% Se elimina la lista de fichas actual del jugador
		assertz(listaJug1(NewFichas)),	   				% Se inserta la nueva lista de fichas del jugador

		cambiarFichasDePropietario(FichasACambiar,1),	% Se cambia las fichas flanqueadas de propietario 

		FichasRestantes is NumFichas - 1,
		retract(fichas(NumFichas)),
		assertz(fichas(FichasRestantes))

		;
		write('Posicion no valida'),nl,
		movimientoJugador(NumFichas)
	).	
		
% ------------------------------------------------------------------------------------------------------------------------			
jugadorBloqueado(Jug):-
	(	Jug = 1 ->
			AuxJug = min
			;
			AuxJug = max
	),
	tablero(A,B,C,D,E,F,G,H),
	listaCambiar(X),
	retract(listaCambiar(_X)),
	assert(listaCambiar([])),
	moves([A,B,C,D,E,F,G,H],AuxJug,M),
	( M = [] ->
		true
		;
		!,false % El punto de corte evita los ciclos bloqueando la reevaluación de un estado previmente analizado
	).
		
% ------------------------------------------------------------------------------------------------------------------------	
	
% mueveJugador(+) Determina si el jugador está moviendo según la primera o la segunda fase del juego.

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
				colocaFichaJugador(NumFichas,Fila,Columna)
			)
		)
	).

% ------------------------------------------------------------------------------------------------------------------------	
	
% mueveMaquina(+,+) Realiza el algoritmo minimax para determinar la jugada de la maquina.
mueveMaquina(Prof):-
		
	tablero(A,B,C,D,E,F,G,H),
	listaJug1(X1), listaJug2(X2),
	minimax([A,B,C,D,E,F,G,H],(Fila,Columna,K),_MejorValor,Prof,X1,X2), % Encuentra el mejor movimiento
	(colocaFicha([A,B,C,D,E,F,G,H],Fila,Columna,2,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar) ->
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
		   
		listaJug2(X),									% Se "invoca" la lista del jugador para tratarla			
		insertaElemento((Fila,Columna),X,NewFichas),		% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		retract(listaJug2(_X)),							% Se elimina la lista de fichas actual del jugador
		assertz(listaJug2(NewFichas)),	   				% Se inserta la nueva lista de fichas del jugador

		cambiarFichasDePropietario(FichasACambiar,2),	% Se cambia las fichas flanqueadas de propietario 
		dibujaTablero,
		% fichas(FichasRestantes),
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
	retractall(juegoAcabado(_)),
	retractall(tablero(_A,_B,_C,_D,_E,_F,_G,_H)),
	retractall(fichas(_)),
	retractall(listaAuxiliar(_)),
	retractall(listaMovimientos(_)),	
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
	retractall(phase(_)),
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
	% escribeFichasJugadores,
	sleep(0.4),
	( not(juegoAcabado(true)) ->
		mueveMaquina(Dificultad),
		fichas(FichasRestantes),
		(FichasRestantes > 0 ->
			bucleJuego(Dificultad)			
			;
			haTerminado
		)
		;
		haTerminado
	).
	
% ------------------------------------------------------------------------------------------------------------------------

% damePosicionesAdyacentesLibres(+,-):- Devuelve una lista con las posiciones libres adyacentes a una posición dada 

damePosicionesAdyacentesLibres((Fila,Columna), PosicionesAdyacentesLibres):-
	
	% write('Entra en damePosicionesAdyacentesLibres'),nl,nl,
	damePosicionesAdyacentes(Fila,Columna,PosicionesAdyacentes),
	
	% write('dPAL:	Para la posicion: '),write(Fila),write(' , '),write(Columna),nl,nl,
	
	% Se invocan las fichas de ambos jugadores
	listaJug1(X),
	listaJug2(Y),
	
	% Nos quedamos sólo con las posiciones libres
	eliminaElementos(X,PosicionesAdyacentes,PosicionesSinFichasJug1),
	eliminaElementos(Y,PosicionesSinFichasJug1,PosicionesAdyacentesLibres).
	
	% write('dPAL:	Posiciones adyacentes libres: '),
	% muestraFichas(PosicionesAdyacentesLibres),nl,nl.

% ------------------------------------------------------------------------------------------------------------------------

%	existenFlanqueos(PosicionesAdyacentes,Jug)

buscaFlanqueos([],Jug).
buscaFlanqueos([(Fila,Columna)|Xs],Jug):-
	
	% write('Entra en buscaFlanqueos'),nl,nl,
	% write('eF:	Posicion libre: '),write('(' ),write(Fila ),write(',' ),write(Columna),write(')'),nl,nl,
			
	listaCambiar(L), 
	retract(listaCambiar(_L)), 
	assertz(listaCambiar([])),
	
	listaMovimientos(M),
	
	(	existeFlanqueo(Fila,Columna,Jug) ->
			
			( contieneElemento2((Fila,Columna),M) ->
				
				dameValor((Fila,Columna),M,Valor),
				NewValor is Valor + 1,
				damePosElemento((Fila,Columna),M,Pos),
				sobreescribirEn((Fila,Columna,NewValor),Pos,M,NewM),
				retract(listaMovimientos(_M)),
				assertz(listaMovimientos(NewM))
				;
				insertaElemento((Fila,Columna,1),M,NewM),
				retract(listaMovimientos(_M)),
				assertz(listaMovimientos(NewM))
			),			
			buscaFlanqueos(Xs,Jug)
		;
			buscaFlanqueos(Xs,Jug)
	).
	
	% buscaFlanqueos(Xs,Jug).
	
% ------------------------------------------------------------------------------------------------------------------------

compruebaMovimientos([],Jug).
compruebaMovimientos([X|Xs],Jug):-

	% write('Entra en compruebaMovimientos'),nl,nl,

	listaAuxiliar(N), 
	insertaElemento(X,N,NewFlanqueo), 
	retractall(listaAuxiliar(_)), 
	assertz(listaAuxiliar(NewFlanqueo)),
	
	% Se determina las posiciones libres adyacentes a cada ficha del jugador 
	damePosicionesAdyacentesLibres(X, PosicionesAdyacentesLibres),
	
	buscaFlanqueos(PosicionesAdyacentesLibres,Jug), 
	
	% listaCambiar(Y),
	% write('cM:	Posiciones a cambiar de propietario: '),muestraFichas(Y),nl,nl
	
	compruebaMovimientos(Xs,Jug).
	
% ------------------------------------------------------------------------------------------------------------------------

maxFilasTablero(Tablero,MF):-
	longitud(Tablero,MF).

maxColumnasTablero([T|Tablero],MC):-
	longitud(T,MC).

% jugadorSinFichas(+,+) Acierta si el jugador pasado por parametro está bloqueado
jugadorSinFichas(Tablero,Jug):-
	(Jug = 1 ->
	 listaJug1(Lista)
	 ;
	 listaJug2(Lista)
	),
	longitud(Lista, Long),
	maxFilasTablero(Tablero,MF),
	maxColumnasTablero(Tablero,MC),
	numFichas is ((MF * MC)/2),
	(Long = numFichas ->
		true
	;
		false
	).

% rival(+,-) Saca por el segundo parametro quien es el rival del primero
rival(Jugador,Rival):-
	(Jugador = 2 -> Rival = 1
		;
		Rival = 2
	).
	
% rivalSinFichas(+,+) Acierta si el rival esta bloqueado
rivalSinFichas(Tablero, Jugador):-
	rival(Jugador,Rival),
	jugadorSinFichas(Tablero,Rival).
	
% ------------------------------------------------------------------------------------------------------------------------
	
moves(Tablero,Jug,M):-
	
	listaCambiar(P),
	retract(listaCambiar(_P)),
	assertz(listaCambiar([])),
	
	listaMovimientos(Q),
	retract(listaMovimientos(_Q)),
	assertz(listaMovimientos([])),
	
	( Jug = min ->
			listaJug2(X),
			compruebaMovimientos(X,1)
		;
			listaJug1(X),
			compruebaMovimientos(X,2)
	),
	
	listaMovimientos(M),
	listaCambiar(Y).
	
	% write('cM:	Posiciones a cambiar de propietario: '),muestraFichas(Y),nl,nl,
	% write('cM:	Movimientos posibles: '),muestraFichas(M),nl,nl,
	

%  ponFicha(+,+,+,+,+,-,-,-) Devuelve en NuevoTablero el movimiento de posicionar la ficha del Jugador en Pos, 
% X1 es la lista de las fichas del jugador 1 y X2 del jugador 2

put(Tablero,(Fila,Columna,K),Jugador,X1,X2,NuevoTablero, NX1, NX2):-
	
	[A,B,C,D,E,F,G,H] = Tablero,
	colocaFicha([A,B,C,D,E,F,G,H],Fila,Columna,Jugador,[Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],FichasACambiar),
	NuevoTablero = [Ar,Br,Cr,Dr,Er,Fr,Gr,Hr],
	
	( Jugador  = 1 ->
		insertaElemento((Fila,Columna),X1,NX1)	     		% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
		;
		insertaElemento((Fila,Columna),X2,NX2)   			% Inserta la posición de la ficha colocada por el jugador a su lista de fichas
	).	

	
dameNumGruposFichasAdyacentes([],Jugador,ListaJug1,ListaJug2,Num,L,LongL):-
	( Num > 1 ->
		insertaElemento(Num,L,NewL),
		longitud(NewL,NewLongL),
		LongL is NewLongL
		;
		longitud(L,NewLongL),
		LongL is NewLongL
	).

dameNumGruposFichasAdyacentes([X|Xs],Jugador,ListaJug1,ListaJug2,Num,L,LongL):-
	
	(Jugador = 1 ->
		(contieneElemento(X,ListaJug1) ->
			NewNum is Num + 1,
			dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,NewNum,L,LongL)
			;
			( Num > 1 ->
				insertaElemento(Num,L,NewL),
				NewNum is 0,
				dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,NewNum,NewL,LongL)
				;
				dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,0,L,LongL)
			)
			
		)
		;
		(contieneElemento(X,ListaJug2) ->
			NewNum is Num + 1,
			dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,NewNum,L,LongL)
			;
			( Num > 1 ->
				insertaElemento(Num,L,NewL),
				NewNum is 0,
				dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,NewNum,NewL,LongL)
				;
				dameNumGruposFichasAdyacentes(Xs,Jugador,ListaJug1,ListaJug2,0,L,LongL)
			)
			
		)
	).
	
	
fichasAgrupadasPorFila([A,B,C,D,E,F,G,H],Jugador,0,ListaJug1,ListaJug2,R,R).
fichasAgrupadasPorFila([A,B,C,D,E,F,G,H],Jugador,Fila,ListaJug1,ListaJug2,R,RFinal):-	
	
	(
		Fila = 1, dameNumGruposFichasAdyacentes(A,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 2, dameNumGruposFichasAdyacentes(B,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 3, dameNumGruposFichasAdyacentes(C,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 4, dameNumGruposFichasAdyacentes(D,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 5, dameNumGruposFichasAdyacentes(E,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 6, dameNumGruposFichasAdyacentes(F,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 7, dameNumGruposFichasAdyacentes(G,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Fila = 8, dameNumGruposFichasAdyacentes(H,Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas)
	),
	NewFila is Fila - 1,
	fichasAgrupadasPorFila([A,B,C,D,E,F,G,H],Jugador,NewFila,ListaJug1,ListaJug2,RAux,RFinal).
	
	
fichasAgrupadasPorColumna([A,B,C,D,E,F,G,H],Jugador,0,ListaJug1,ListaJug2,R,R).
fichasAgrupadasPorColumna([	[A0,A1,A2,A3,A4,A5,A6,A7],
							[B0,B1,B2,B3,B4,B5,B6,B7],
							[C0,C1,C2,C3,C4,C5,C6,C7],
							[D0,D1,D2,D3,D4,D5,D6,D7],
							[E0,E1,E2,E3,E4,E5,E6,E7],
							[F0,F1,F2,F3,F4,F5,F6,F7],
							[G0,G1,G2,G3,G4,G5,G6,G7],
							[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,Columna,ListaJug1,ListaJug2,R,RFinal):-	
							
	(
		Columna = 1, dameNumGruposFichasAdyacentes([A0,B0,C0,D0,E0,F0,G0,H0],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 2, dameNumGruposFichasAdyacentes([A1,B1,C1,D1,E1,F1,G1,H1],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 3, dameNumGruposFichasAdyacentes([A2,B2,C2,D2,E2,F2,G2,H2],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 4, dameNumGruposFichasAdyacentes([A3,B3,C3,D3,E3,F3,G3,H3],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 5, dameNumGruposFichasAdyacentes([A4,B4,C4,D4,E4,F4,G4,H4],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 6, dameNumGruposFichasAdyacentes([A5,B5,C5,D5,E5,F5,G5,H5],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 7, dameNumGruposFichasAdyacentes([A6,B6,C6,D6,E6,F6,G6,H6],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas);
		Columna = 8, dameNumGruposFichasAdyacentes([A7,B7,C7,D7,E7,F7,G7,H7],Jugador,ListaJug1,ListaJug2,0,[],NumeroFichasAgrupadas), RAux is R + ( 3 * NumeroFichasAgrupadas)
	),
	NewColumna is Columna - 1,
	fichasAgrupadasPorColumna([	[A0,A1,A2,A3,A4,A5,A6,A7],
								[B0,B1,B2,B3,B4,B5,B6,B7],
								[C0,C1,C2,C3,C4,C5,C6,C7],
								[D0,D1,D2,D3,D4,D5,D6,D7],
								[E0,E1,E2,E3,E4,E5,E6,E7],
								[F0,F1,F2,F3,F4,F5,F6,F7],
								[G0,G1,G2,G3,G4,G5,G6,G7],
								[H0,H1,H2,H3,H4,H5,H6,H7]],Jugador,NewColumna,ListaJug1,ListaJug2,RAux,RFinal).	
	

	
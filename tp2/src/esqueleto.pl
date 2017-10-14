%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(Tablero, Fila, Columna, Contenido) :- nth1(Fila, Tablero, ListaFilas), nth1(Columna, ListaFilas, Contenido).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T, F, C) :- contenido(T, F, C, Cont), var(Cont), forall( adyacenteEnRango(T, F, C, FA, CA), ( contenido(T, FA, CA, ContA), var(ContA) )).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(1, horizontal, Tablero, Fila, Columna) :- disponible(Tablero,Fila,Columna) .

puedoColocar(CantPiezas, horizontal, Tablero, Fila, Columna) :- setof(Var,(disponible(Tablero,Fila,Columna),
																CantPiezasMenosUno is CantPiezas - 1 , 
																CMasUno is Columna + 1, 
																puedoColocar(CantPiezasMenosUno,horizontal,Tablero,Fila,CMasUno)),Set).

puedoColocar(1, vertical, Tablero, Fila, Columna) :- disponible(Tablero,Fila,Columna) .

puedoColocar(CantPiezas, vertical, Tablero, Fila, Columna) :- setof(Var,(disponible(Tablero,Fila,Columna),
																CantPiezasMenosUno is CantPiezas - 1 , 
																FMasUno is Fila + 1, 
																puedoColocar(CantPiezasMenosUno,vertical,Tablero,FMasUno,Coulmna)),Set).

%insertarBarco(?+T,+X,+Y,+Len, Dir)
insertarBarco(T,X,Y,1,Dir) :- contenido(T,X,Y,o).
insertarBarco(T,X,Y,Len, horizontal) :- contenido(T,X,Y,o) , YMasUno is Y+1 , LenMenosUno is Len -1, insertarBarco(T,X,YMasUno,LenMenosUno,horizontal).
insertarBarco(T,X,Y,Len, vertical) :- contenido(T,X,Y,o) , XMasUno is X+1 ,  LenMenosUno is Len -1, insertarBarco(T,XMasUno,Y,LenMenosUno,vertical).


%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([],Tablero).
ubicarBarcos([Barco|Barcos],Tablero) :- setof(Var, (puedoColocar(Barco,Dir,Tablero,Fila,Columna), insertarBarco(Tablero,Fila,Columna,Barco,Dir), ubicarBarcos(Barcos,Tablero)), Set).

%completarConAgua(+?Tablero)



completarConAgua(Tablero) :-  flatten(Tablero,TableroFlat), maplist(nonvar,TableroFlat).

completarConAgua(Tablero) :-  contenido(Tablero,X,Y,C), var(C), contenido(Tablero,X,Y,~),completarConAgua(Tablero) ,!.


%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(Tablero, NumFila, NumColumna, NuevoTab) :- forall(contenido(Tablero, Fila, Columna, Cont), 
	contenido(NuevoTab, Fila, Columna, Cont); (Fila =:= NumFila, Columna =:= NumColumna)),
	contenido(NuevoTab, NumFila, NumColumna, ~).

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

% Ejercicio 1
test_contenido :- contenido([[1, 2], [3, 4], [5, 6]], 1, 1, 1).
test_contenido_2 :- contenido([[1, 2], [3, 4], [5, 6]], 2, 1, 3).

% Ejercicio 2
test_disp_1:- matriz(M,2,3),disponible(M, 1, 1).
test_disp_2:- not(disponible([[1, 2], [3, 4], [5, 6]], 1, 1)).
test_disp_3:- not(disponible([[1, 2], [_, _], [5, 6]], 2, 1)).
test_disp_4:- not(disponible([[1, 2], [3, 4], [5, 6]], 3, 1)).
test_disp_5:- disponible([[_, _, _, 1], [_, _, _, 1], [_, _, _, 1]], 2, 2).

% Ejercicio 3

% Ejercicio 4

% Ejercicio 5

test_golpear_1 :- golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[~, 2], [3, 4], [5, 6]]).
test_golpear_2 :- not(golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[1, 2], [3, 4], [5, 6]])).
% Ejercicio 6

% Ejercicio 7

test_puedoColocar :- puedoColocar(2,horizontal,[ [X, E ,Y], [Y, R, U], [Z, T, I] ],1,1).

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- test_disp_1, test_disp_2, test_disp_3, test_disp_4, test_disp_5.
test(4) :- test_golpear_1, test_golpear_2.

test_puedoColocar_1 :- matriz(M, 2, 4), puedoColocar(3, horizontal, M, 1, 1).

tests :- forall(between(1,4,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.


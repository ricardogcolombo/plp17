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
disponible(T, F, C) :- contenido(T, F, C, Cont), var(Cont), adyacenteEnRango(T, F, C, FA, CA), contenido(T, FA, CA, ContA), var(ContA).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(0, horizontal, Tablero, Fila, Columna) :- disponible(Tablero,Fila,Columna) .
puedoColocar(CantPiezas, horizontal, Tablero, Fila, Columna) :- setof(Var,(disponible(Tablero,Fila,Columna),
																CantPiezasMenosUno is CantPiezas - 1 , 
																CMasUno is Columna + 1, 
																puedoColocar(CantPiezasMenosUno,horizontal,Tablero,Fila,CMasUno)),Set).

%ubicarBarcos(+Barcos, +?Tablero)

%completarConAgua(+?Tablero)

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.

test_contenido :- contenido([[1, 2], [3, 4], [5, 6]], 1, 1, 1).

test_contenido_2 :- contenido([[1, 2], [3, 4], [5, 6]], 2, 1, 3).

test_disponible :- matriz(M,3,3) , disponible(M,1,1).

test_disponible_2 :- not(disponible([[1, 2], [3, 4], [5, 6]],1,1)).

test_puedoColocar :- puedoColocar(2,horizontal,[ [X, E ,Y], [Y, R, U], [Z, T, I] ],1,1).


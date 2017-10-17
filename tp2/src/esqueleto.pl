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


replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L). 

golpear(Tablero,Fila,Columna,NuevoTab) :- NuevoTabAux = Tablero, lists:nth1(Fila,NuevoTabAux,L), replace(L ,Columna,~,NuevaCol), replace(NuevoTabAux,Fila,NuevaCol,NuevoTab), !.  

% Completar instanciación soportada y justificar.


%atacar(+Tablero, ?Fila, ?Columna, ?Resultado, ?NuevoTab)
%Todos los parametros son reversibles, excepto Tablero, pues la implementacion usa fuertemente la precondicion que dice que lo que entra en este parametro esta correctamente instanciado.

aguaOborde(Tablero,Fila,Columna) :- not(enRango(Tablero,Fila,Columna) ).

aguaOborde(Tablero,Fila,Columna) :- contenido(Tablero,Fila,Columna,~).
hundido(Tablero,Fila,Columna) :- FMasUno is Fila + 1 , FMenosUno is Fila - 1, CMasUno is Columna + 1, CMenosUno is Columna - 1, aguaOborde(Tablero,Fila,Columna) , aguaOborde(Tablero,FMasUno,Columna),aguaOborde(Tablero,FMenosUno,Columna), aguaOborde(Tablero,Fila,CMenosUno), aguaOborde(Tablero,Fila,CMasUno).

atacar(Tablero,Fila,Columna,Resultado,NuevoTab) :- contenido(Tablero,Fila,Columna,Cont) ,  contenido(Tablero,Fila,Columna,o), golpear(Tablero,Fila,Columna,NuevoTab), hundido(NuevoTab,Fila,Columna), Resultado = hundido.
atacar(Tablero,Fila,Columna,Resultado,NuevoTab) :- contenido(Tablero,Fila,Columna,Cont) ,  contenido(Tablero,Fila,Columna,o), golpear(Tablero,Fila,Columna,NuevoTab), not( hundido(NuevoTab,Fila,Columna) ), Resultado = tocado.
atacar(Tablero,Fila,Columna,Resultado,NuevoTab) :- contenido(Tablero,Fila,Columna,Cont) ,  contenido(Tablero,Fila,Columna,~), Resultado = agua, NuevoTab = Tablero.

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
test_puedoColocar_1:- matriz(M,2,4), puedoColocar(3,Dir,M,F,C).
test_puedoColocar_2:- matriz(M,2,3), contenido(M,2,1,o), puedoColocar(2,Dir,M,F,C).
test_puedoColocar_3:- not(puedoColocar(2,[[_, _, _, 1], [_, _, _, 1],[_, _, _, 1]], horizontal,2, 2)).
test_puedoColocar_4:- not(puedoColocar(5,[[_, _, _, 1], [_, _, _, 1], [_, _, _, 1]], vertical,3,3)).

% Ejercicio 4
test_ubicar_1:- matriz(M,3,2), ubicarBarcos([2,1],M).
test_ubicar_2:- matriz(M,3,2), ubicarBarcos([1,1],M).
test_ubicar_3:- matriz(M,3,2), not(ubicarBarcos([3,1],M)).
test_ubicar_4:- matriz(M,10,10), ubicarBarcos([1,1,1,2],M).

% Ejercicio 5
test_completar_1:- matriz(M,3,2),ubicarBarcos([2,1],M), completarConAgua(M),compare((=),M,[[~, ~], [~, ~], [~, ~]]).
test_completar_2:- matriz(M,3,2), completarConAgua(M),compare((=),M,[[~, ~], [~, ~], [~, ~]]).

% Este test compara solo com el primero de los elementos en el set
test_completar_3:- matriz(M,3,2), setof(Var,ubicarBarcos([1,1],M),Set), completarConAgua(M),compare((=),M,[[~, o], [~, ~], [~, o]]).
test_completar_4:- matriz(M,3,2), ubicarBarcos([2,1],M),completarConAgua(M),compare((=),M,[[~, o], [~, ~], [o, o]]).

% Ejercicio 6
test_golpear_1 :- golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[~, 2], [3, 4], [5, 6]]).
test_golpear_2 :- golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[1, 2], [3, 4], [5, 6]]).

% Ejercicio 7
test_atacar_1 :- atacar([[o, o], [⇠, ⇠], [⇠, o]],1,1,Res,T).

test_puedoColocar :- puedoColocar(2,horizontal,[ [X, E ,Y], [Y, R, U], [Z, T, I] ],1,1).

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- test_disp_1, test_disp_2, test_disp_3, test_disp_4, test_disp_5.
test(4) :- test_golpear_1, test_golpear_2.
test(5) :- test_completar_1,test_completar_2,test_completar_3,test_completar_4.
test(6) :- test_ubicar_1,test_ubicar_2,test_ubicar_3,test_ubicar_4.
test(7) :- test_puedoColocar_1,test_puedoColocar_2,test_puedoColocar_3,test_puedoColocar_4.

test_puedoColocar_1 :- matriz(M, 2, 4), puedoColocar(3, horizontal, M, 1, 1).

tests :- forall(between(1,4,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.


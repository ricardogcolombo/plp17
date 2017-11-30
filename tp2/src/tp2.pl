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
contenido(Tablero, Fila, Columna, Contenido) :- 
    nth1(Fila, Tablero, ListaFilas), 
    nth1(Columna, ListaFilas, Contenido).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(Tablero, Fila, Columna) :- 
    contenido(Tablero, Fila, Columna, Contenido), 
    var(Contenido), 
    forall(
        adyacenteEnRango(Tablero, Fila, Columna, FilaAdyacente, ColumnaAdyacente), 
        (
        	contenido(Tablero, FilaAdyacente, ColumnaAdyacente, ContenidoAdyacente), 
        	var(ContenidoAdyacente) 
        )
    ).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(0, _, _, _, _).
puedoColocar(CantPiezas, Direccion, Tablero, Fila, Columna) :-
	disponible(Tablero, Fila, Columna),
    siguienteCelda(Direccion, Fila, Columna, SiguienteFila, SiguienteColumna),
    NuevaCantPiezas is CantPiezas - 1,
    puedoColocar(NuevaCantPiezas, Direccion, Tablero, SiguienteFila, SiguienteColumna).
    
siguienteCelda(horizontal, Fila, Columna, SiguienteFila, SiguienteColumna) :- 
    SiguienteFila is Fila,
    SiguienteColumna is Columna + 1.

siguienteCelda(vertical, Fila, Columna, SiguienteFila, SiguienteColumna) :- 
    SiguienteFila is Fila + 1,
    SiguienteColumna is Columna.

%%% matriz(Tablero, 2, 4), puedoColocar(20, Direccion, Tablero, Fila, Columna)

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([Longitud| Longitudes], Tablero) :- 
    puedoColocar(Longitud, Direccion, Tablero, Fila, Columna),
    not((Direccion = horizontal, Longitud = 1)), % Esto es solamente para evitar contar los barcos de 1 elemento dos veces (una como horizontal y otra como vertical)
    colocarBarco(Tablero, Longitud, Direccion, Fila, Columna),
    ubicarBarcos(Longitudes, Tablero).

%colocarBarco(?+Tablero, +Longitud, +Direccion, +Fila, +Columna)
colocarBarco(_, 0, _, _, _).
colocarBarco(Tablero, Longitud, Direccion, Fila, Columna) :-
    contenido(Tablero, Fila, Columna, o),
	siguienteCelda(Direccion, Fila, Columna, SiguienteFila, SiguienteColumna),
	NuevaLongitud is Longitud - 1,
	colocarBarco(Tablero, NuevaLongitud, Direccion, SiguienteFila, SiguienteColumna).


%completarConAgua(+?Tablero)
% TODO: hacerlo con maplist/2
completarConAgua(Tablero) :- maplist(completarFilaConAgua, Tablero).
completarFilaConAgua(Fila) :- maplist(completarCeldaConAgua, Fila).
completarCeldaConAgua(Celda) :- nonvar(Celda).
completarCeldaConAgua(~).

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
test_puedoColocar_5 :- matriz(M, 2, 4), puedoColocar(3, horizontal, M, 1, 1).

% Ejercicio 4
test_ubicar_1:- matriz(M,3,2), ubicarBarcos([2,1],M).
test_ubicar_2:- matriz(M,3,2), ubicarBarcos([1,1],M).
test_ubicar_3:- matriz(M,3,2), not(ubicarBarcos([3,1],M)).
test_ubicar_4:- matriz(M,10,10), ubicarBarcos([1,1,1,2],M).

% Ejercicio 5
test_completar_1:- matriz(M,3,2),ubicarBarcos([2,1],M), completarConAgua(M), member(M,[ [[~, o], [~, ~], [o, o]] , [[o, ~], [~, ~], [o, o]] , [[o, o], [~, ~], [~, o]], [[o, o], [~, ~], [o, ~]]]).
test_completar_2:- matriz(M,3,2), completarConAgua(M),compare((=),M,[[~, ~], [~, ~], [~, ~]]).

% Este test compara solo com el primero de los elementos en el set
test_completar_3:- matriz(M,3,2), ubicarBarcos([1,1],M),completarConAgua(M), member(M,[[[~, o], [~, ~], [~, o]], [[~, o], [~, ~], [o, ~]], [[o, ~], [~, ~], [~, o]], [[o, ~], [~, ~], [o, ~]]]).

test_completar_4:- matriz(M,3,2), ubicarBarcos([2,1],M),completarConAgua(M), member(M,[ [[~, o], [~, ~], [o, o]] , [[o, ~], [~, ~], [o, o]] , [[o, o], [~, ~], [~, o]] , [[o, o], [~, ~], [o, ~]]]).
% Ejercicio 6
test_golpear_1 :- golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[~, 2], [3, 4], [5, 6]]).
test_golpear_2 :- golpear([[1, 2], [3, 4], [5, 6]], 1, 1, [[1, 2], [3, 4], [5, 6]]).

% Ejercicio 7
test_atacar_1 :- atacar([[o, o], [⇠, ⇠], [⇠, o]],1,1,Res,T),Res=tocado.
test_atacar_2:- not(atacar([[o, o], [⇠, ⇠], [⇠, o]],3,1,Res,T)).
test_atacar_3 :- atacar([[o, o], [⇠, ⇠], [⇠, o]],3,2,Res,T),Res=tocado.

test_puedoColocar :- puedoColocar(2,horizontal,[ [X, E ,Y], [Y, R, U], [Z, T, I] ],1,1).

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- test_disp_1, test_disp_2, test_disp_3, test_disp_4, test_disp_5.
test(4) :- test_golpear_1, test_golpear_2.
test(5) :- test_completar_1,test_completar_2,test_completar_3,test_completar_4.
test(6) :- test_ubicar_1,test_ubicar_2,test_ubicar_3,test_ubicar_4.
test(7) :- test_puedoColocar_1,test_puedoColocar_2,test_puedoColocar_3,test_puedoColocar_4.
test(8) :- test_atacar_1,test_atacar_2,test_atacar_3.


tests :- forall(between(1,4,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.


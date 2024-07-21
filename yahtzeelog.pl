:- use_module(library(random)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).

% Lanza un dado
tiro_dado(X):-
    random(1,7,X).


% puntaje(+Dados, +Cat,-Puntos)
% Dados es una lista con los valores de los dados, y Cat indica una de las categorías del juego. 
% El argumento Puntos indica la cantidad de puntos para esa combinación de dados y categoría.

% Verifica si esta libre (con nil) el slot de la categoria dada en el tablero actual
slot_libre([Elemento|_], Categoria) :-
    Elemento =.. [_|Args],
    nth1(1, Args, Categoria),
    nth1(2, Args, nil).
slot_libre([Elemento|Tablero], Cat) :-
    Elemento =.. [_|Args],
    nth1(1, Args, Categoria),
    Categoria \= Cat,
    slot_libre(Tablero, Cat).

% Verifica si, en un conjunto de categorias, hay una que esta libre en el tablero
una_libre(Tablero, [Cat|_]) :- slot_libre(Tablero, Cat).
una_libre(Tablero, [Cat|Cats]) :- not(slot_libre(Tablero, Cat)) -> una_libre(Tablero, Cats).

% Crea un arreglo donde el valor en la posicion i dice la cantidad de dados con valor i en la configuracion de dados
revisar_dados(_, [], 7).
revisar_dados(Dados, [Elemento|Resto], Index) :-
    Index < 7,
    findall(X, select(Index, Dados, X), L),
    length(L, Elemento),
    IndexNuevo is Index + 1,
    revisar_dados(Dados, Resto, IndexNuevo).
 
% ------------ Secciones ------------
seccion_inferior(three_of_a_kind).
seccion_inferior(four_of_a_kind).
seccion_inferior(full_house).
seccion_inferior(small_straight).
seccion_inferior(large_straight).
seccion_inferior(yahtzee).
seccion_inferior(chance).

seccion_superior(aces, 1). 
seccion_superior(twos, 2).
seccion_superior(threes, 3).
seccion_superior(fours, 4).
seccion_superior(fives, 5).
seccion_superior(sixes, 6).
% -----------------------------------

% ------------ Seccion Superior ------------
elegir([Elemento|_], 1, Elemento).
elegir([_|Resto], Index, Resultado) :-
    Index \= 1,
    IndexNuevo is Index - 1,
    elegir(Resto, IndexNuevo, Resultado).
% ------------------------------------------

% ------------ Small y Large Straight ------------
ver_puntaje_straight(_, 30, 4, 4).
ver_puntaje_straight(_, 40, 5, 5).
ver_puntaje_straight([], 0, Cantidad, Bound) :- Cantidad < Bound.
ver_puntaje_straight([0|Resto], Resultado, Cantidad, Bound) :-
    Cantidad < Bound,
    ver_puntaje_straight(Resto, Resultado, 0, Bound).
ver_puntaje_straight([Elemento|Resto], Resultado, Cantidad, Bound) :-
    Elemento \= 0, 
    Cantidad < Bound,
    CantidadNuevo is Cantidad + 1,
    ver_puntaje_straight(Resto, Resultado, CantidadNuevo, Bound).
% ------------------------------------------------

% ------------ Full house ------------
ver_puntaje_fh([], Resultado, Existen2, Existen3) :- Resultado is 25 * Existen2 * Existen3.
ver_puntaje_fh([3|B], Resultado, Existen2, _) :-
  ver_puntaje_fh(B, Resultado, Existen2, 1).
ver_puntaje_fh([2|B], Resultado, _, Existen3) :-
  ver_puntaje_fh(B, Resultado, 1, Existen3).
ver_puntaje_fh([A|B], Resultado, Existen2, Existen3) :-
  A \= 2,
  A \= 3,
  ver_puntaje_fh(B, Resultado, Existen2, Existen3).
% ------------------------------------

% ------------ Yahtzee ------------
ver_puntaje_yah([Elemento|_], 50) :- Elemento >= 5.
ver_puntaje_yah([], 0).
ver_puntaje_yah([Elemento|Resto], Resultado) :-
    Elemento < 5,
    ver_puntaje_yah(Resto, Resultado).
% ---------------------------------

% ------------ Three o Four of a Kind ------------
calcular_total(_, 0, 7).
calcular_total([Elemento|Resto], Resultado, Index) :-
    IndexNuevo is Index + 1,
    calcular_total(Resto, Res, IndexNuevo),
    Resultado is Elemento*Index + Res.

ver_puntaje_detectar([], _, 0).
ver_puntaje_detectar([Elemento|_], Necesarios, 1) :-
    Elemento >= Necesarios.
ver_puntaje_detectar([Elemento|Resto], Necesarios, Resultado) :-
    Elemento < Necesarios,
    ver_puntaje_detectar(Resto, Necesarios, Resultado).

ver_puntaje_tof(_, 0, 0).
ver_puntaje_tof(Elemento, Resultado, 1) :-
    calcular_total(Elemento, Resultado, 1).
% ------------------------------------------------

% ------------ Chance ------------
ver_puntaje_ch([], Resultado, _, Resultado).
ver_puntaje_ch([Elemento|Resto], P, Index, Resultado) :-
    ResultadoNuevo is Index * Elemento + Resultado,
    IndexNuevo is Index + 1,
    ver_puntaje_ch(Resto, P, IndexNuevo, ResultadoNuevo).
% --------------------------------

ver_puntaje(A, full_house, P) :-
    ver_puntaje_fh(A, P, 0, 0).
ver_puntaje(A, large_straight, P) :-
    ver_puntaje_straight(A, P, 0, 5).
ver_puntaje(A, small_straight, P) :-
    ver_puntaje_straight(A, P, 0, 4).
ver_puntaje(A, chance, P) :-
    ver_puntaje_ch(A, P, 1, 0).
ver_puntaje(A, four_of_a_kind, P) :-
    ver_puntaje_detectar(A, 4, R),
    ver_puntaje_tof(A, P, R).
ver_puntaje(A, three_of_a_kind, P) :-
    ver_puntaje_detectar(A, 3, R),
    ver_puntaje_tof(A, P, R).
ver_puntaje(A, yahtzee, P) :-
    ver_puntaje_yah(A, P).
ver_puntaje(A,C,P) :-
    seccion_superior(C, N),
    elegir(A, N, R),
    P is R * N.

puntaje(D,C,P) :- 
    revisar_dados(D, Arreglo, 1),
    ver_puntaje(Arreglo,C,P). 


% puntaje_tablero(+Tablero, -Puntaje)
% Tablero contiene un tablero con todos los slots completos. 
% El segundo argumento contiene el puntaje para ese tablero

puntajes_total(Puntos, SeccionSuperior, Resultado) :- 
    SeccionSuperior > 62,
    Resultado is Puntos + 35.
puntajes_total(Puntos, SeccionSuperior, Puntos) :- 
    SeccionSuperior < 63.

seccion(Categoria, Puntos, SeccionSuperior, SeccionSuperiorNuevo) :-
    seccion_superior(Categoria, _),
    SeccionSuperiorNuevo is SeccionSuperior + Puntos.
seccion(Categoria, _, SeccionSuperior, SeccionSuperior) :-
    seccion_inferior(Categoria).

calcular_puntaje([], 0, 0).
calcular_puntaje([Elemento|Tablero], Puntaje, SeccionSuperior) :-
    calcular_puntaje(Tablero, PuntajeAnterior, SeccionSuperiorAnterior),
    Elemento =.. [_|Args],
    nth1(1, Args, Categoria),
    nth1(2, Args, Puntos),
    seccion(Categoria, Puntos, SeccionSuperiorAnterior, SeccionSuperior),
    Puntaje is PuntajeAnterior + Puntos.

puntaje_tablero(Tablero, Puntaje) :- 
    calcular_puntaje(Tablero, Puntos, SeccionSuperior),
    puntajes_total(Puntos, SeccionSuperior, Puntaje).
    

% ajustar_tablero(+Tablero,+Categoria,+Puntaje,-TableroSalida)
% TableroSalida es igual a Tablero, pero en el slot correspondiente a Categoria contiene Puntaje.

ajustar_tablero([],_,_,[]).
ajustar_tablero([Elemento|Tablero],Categoria,Puntaje,[Elemento|TableroSalida]) :-
    Elemento =.. [_|Args],
    nth1(1, Args, CategoriaActual),
    Categoria \= CategoriaActual,
    ajustar_tablero(Tablero,Categoria,Puntaje,TableroSalida).
ajustar_tablero([Elemento|Tablero],Categoria,Puntaje,[Elemento1|TableroSalida]) :-
    Elemento = s(Categoria,_),
    Elemento1 = s(Categoria,Puntaje),
    ajustar_tablero(Tablero,Categoria,Puntaje,TableroSalida).

% Recibe una configuracion de dados y un numero, y devolvera el patron que mantiene
% todos los dados cuyo valor es igual a numero
generar_patron_numero([], _, []).
generar_patron_numero([Dado|Resto], Numero, Patron) :-
    Numero \= Dado,
    generar_patron_numero(Resto, Numero, PatronAnterior),
    Patron = [1|PatronAnterior].
generar_patron_numero([Dado|Resto], Numero, Patron) :-
    Numero = Dado,
    generar_patron_numero(Resto, Numero, PatronAnterior),
    Patron = [0|PatronAnterior].

% Recibe una configuracion de dados y una lista de valores, y devolvera un patron que mantiene los dados  
% que tienen la primera aparicion de cada uno de los valores en lista.
generar_patron_lista([], [], []).
generar_patron_lista([Dado|Resto], Lista, Patron) :-
    not(member(Dado, Lista)),
    generar_patron_lista(Resto, Lista, PatronAnterior),
    Patron = [1|PatronAnterior].
generar_patron_lista([Dado|Resto], Lista, Patron) :-
    selectchk(Dado, Lista, NuevaLista),
    generar_patron_lista(Resto, NuevaLista, PatronAnterior),
    Patron = [0|PatronAnterior].

% ----------------------- Condiciones cambio de dado ia_det -----------------------    

% Condicion 1: Si se tiene 2,3,4,5 y esta libre large_straight los mantiene.
% Ademas si el quinto dado ya es 1 o 6, se mantienen todos.
cambio_dados_condicion1(Dados, Tablero, Patron) :-
    slot_libre(Tablero, large_straight),
    (
	generar_patron_lista(Dados, [1,2,3,4,5], Patron1) -> Patron = Patron1;   
	generar_patron_lista(Dados, [2,3,4,5,6], Patron2) -> Patron = Patron2;   
	generar_patron_lista(Dados, [2,3,4,5], Patron3) -> Patron = Patron3
    ).

% Condicion 2: Si yahtzee esta libre y todos los dados son iguales, los mantiene.
cambio_dados_condicion2(Dados, Tablero, Patron) :- 
    slot_libre(Tablero, yahtzee),
    generar_patron_lista(Dados, [X,X,X,X,X], Patron).

% Condicion 3: Si se tiene Full House, mantenerlo amenos que los 3 dados repetidos sean de valor 6.
cambio_dados_condicion3(Dados, Tablero, Patron) :- 
    slot_libre(Tablero, full_house),
    (
	generar_patron_lista(Dados, [X,X,Y,Y,Y], Patron1), Y \= 6 -> Patron = Patron1;
	generar_patron_lista(Dados, [X,X,X,Y,Y], Patron2), X \= 6 -> Patron = Patron2
    ),
    X \= Y.

% Condicion 4: Si se tiene 3, 4, 5 o 2, 3, 4 en el tiro, y esta libre small_straight mantenerlos.
cambio_dados_condicion4(Dados, Tablero, Patron) :- 
    slot_libre(Tablero, small_straight),
    (
        generar_patron_lista(Dados, [3,4,5], Patron);
        generar_patron_lista(Dados, [2,3,4], Patron)
    ). 

% Condicion 5: Si estan libres sixes, three_of_a_king o four_of_a_kind, mantener los dados con valor 6.
cambio_dados_condicion5(Dados, Tablero, Patron) :- 
    (
        slot_libre(Tablero, sixes);
        slot_libre(Tablero, three_of_a_kind);
        slot_libre(Tablero, four_of_a_kind)
    ),
    generar_patron_numero(Dados, 6, Patron).

% Dada una configuracion de dados devuelve una lista de elementos de la forma [Cantidad,Valor], que da la cantidad de dados de ese valor en la configuracion de dados.
calcular_repeticiones(_, [], 7).
calcular_repeticiones(Dados, [Elemento|Resto], Index) :-
    Index < 7,
    findall(X, select(Index, Dados, X), L),
    length(L, Len),
    Elemento = [Len, Index],
    IndexNuevo is Index + 1,
    calcular_repeticiones(Dados, Resto, IndexNuevo).

% Para cada valor que aparece en la configuracion de dados, verifica si esta libre su categoria en la upper section.
% Si lo esta genera un patron que mantiene todos los dados de ese valor.
cambio_dados_condicion6(Dados, Tablero, [Elemento|Arreglo], Patron) :- 
    Elemento = [Cantidad, Resultado],
    Cantidad > 0,
    seccion_superior(Cat, Resultado),
    (
        (
            slot_libre(Tablero, Cat),
            generar_patron_numero(Dados, Resultado, Patron)
        );
        (
            cambio_dados_condicion6(Dados, Tablero, Arreglo, Patron)
        )
    ).

% Condicion 6: Mantener los dados cuyo valor tiene la mayor cantidad de repeticiones en la configuracion, y ademas esta libre en la Upper Section
cambio_dados_condicion6(Dados, Tablero, Patron) :-
    calcular_repeticiones(Dados, Arreglo, 1),
    sort(1, @>=, Arreglo, ArregloSort),
    cambio_dados_condicion6(Dados, Tablero, ArregloSort, Patron).

% Busca en una lista el indice donde se encuentra el elemento de mayor valor
indice_maximo([], _, 0, 0).
indice_maximo([Maximo|_], Maximo, Indice, Indice).
indice_maximo([Elemento|Lista], Maximo, Indice, Resultado) :-
    Elemento \= Maximo,
    IndiceNuevo is Indice - 1,
    indice_maximo(Lista, Maximo, IndiceNuevo, Resultado).

% Condicion 7: Si estan libres three_of_a_king o four_of_a_kind, mantener los dados cuyos valores se presenten en mayor cantidad. 
% Si hay empates va por los de mayor valor.
cambio_dados_condicion7(Dados, Tablero, Patron) :- 
    (
        slot_libre(Tablero, four_of_a_kind);
        slot_libre(Tablero, three_of_a_kind)
    ),
    revisar_dados(Dados, Lista, 1),
    reverse(Lista, ListaReversa),
    max_list(Lista, Maximo),
    indice_maximo(ListaReversa, Maximo, 6, Resultado),
    generar_patron_numero(Dados, Resultado, Patron).

% Condicion 8: Cambia todos los dados
cambio_dados_condicion8(_, _, [1,1,1,1,1]).

% ---------------------------------------------------------------------------------    
    
% Chequea de a una las condiciones de cambio de dados para ia_det y elige el primer patron valido
cambio_dados(Dados, Tablero, ia_det, Patron) :- (   
    cambio_dados_condicion1(Dados, Tablero, Patron1) ->  Patron = Patron1;   
    cambio_dados_condicion2(Dados, Tablero, Patron2) ->  Patron = Patron2;   
    cambio_dados_condicion3(Dados, Tablero, Patron3) ->  Patron = Patron3;   
    cambio_dados_condicion4(Dados, Tablero, Patron4) ->  Patron = Patron4;
    cambio_dados_condicion6(Dados, Tablero, Patron6) ->  Patron = Patron6;   
    cambio_dados_condicion7(Dados, Tablero, Patron7) ->  Patron = Patron7;
    cambio_dados_condicion8(Dados, Tablero, Patron8) ->  Patron = Patron8   
).

% Permite a un jugador elegir el patron de cambio de dados.
cambio_dados(_, _, humano, Patron) :-
    writeln('¿Que dados desdea mantener? Ingresar lista con: 0 - mantener | 1 - volver a tirar'),
    read(Patron).

% 
cambio_dados(Dados, Tablero, ia_prob, Patron) :- 
    Final1 = [],
    Categorias = [full_house,large_straight,small_straight,aces,twos,threes,fours,fives,sixes], 
    % Primer peticion a Problog
    peticion_problog([["large_straight", "40"], ["full_house", "25"], ["yahtzee", "50"]], Dados, Tablero, Final2, Final1),

    % Segunda peticion a Problog
    (
        (
            elegir_dados_repetidos(Dados, Tablero, N, C),
            N = [Cant, Nume],
            lista_elementos_repetidos(Cant, Nume, DadosAProbar2),
            CategoriasAProbar2 = [C, ["full_house", "25"]],
            peticion_problog(CategoriasAProbar2, DadosAProbar2, Tablero, Final3, Final2)
        );
        (
            random(3, 5, CantidadDados2),
            elegir_N_dados_random(Dados, CantidadDados2, DadosAProbar2),
            random(3, 5, CantidadCategorias2),
            elegir_N_categorias_random(Categorias, CantidadCategorias2, Tablero, CategoriasAProbar2),
            peticion_problog(CategoriasAProbar2, DadosAProbar2, Tablero, Final3, Final2)
        )
    ),  
    % Tercer peticion a Problog
    random(3, 5, CantidadDados3),
    elegir_N_dados_random(Dados, CantidadDados3, DadosAProbar3),
    random(3, 5, CantidadCategorias3),
    elegir_N_categorias_random(Categorias, CantidadCategorias3, Tablero, CategoriasAProbar3),
    peticion_problog(CategoriasAProbar3, DadosAProbar3, Tablero, Final4, Final3),
    % Elige el mejor candidato para realizar las pruebas
    max_member(max_func, Maximo, Final4),
    Maximo = [_, _, D1],
    generar_patron_lista(Dados, D1, Patron).

elegir_dados_repetidos_aux(Tablero, [Elemento|_], Elemento, [C, P]) :- 
    Elemento = [Cantidad, Resultado],
    Cantidad > 0,
    seccion_superior(Cat, Resultado),
    slot_libre(Tablero, Cat), 
    datos_categoria(Cat, Datos),
    Datos = [C, P, _].
elegir_dados_repetidos_aux(Tablero, [Elemento|Arreglo], Numero, Categoria) :- 
    Elemento = [Cantidad, Resultado],
    Cantidad > 0,
    seccion_superior(Cat, Resultado),
    not(slot_libre(Tablero, Cat)),
    elegir_dados_repetidos_aux(Tablero, Arreglo, Numero, Categoria).

% Mantener los dados cuyo valor tiene la mayor cantidad de repeticiones en la configuracion, y ademas esta libre en la Upper Section
elegir_dados_repetidos(Dados, Tablero, Numero, Categoria) :-
    calcular_repeticiones(Dados, Arreglo, 1),
    sort(1, @>=, Arreglo, ArregloSort),
    elegir_dados_repetidos_aux(Tablero, ArregloSort, Numero, Categoria).
            
lista_elementos_repetidos(0, _, []).
lista_elementos_repetidos(Cant, Nume, [Nume|DadosAProbar2]) :-
    CantNuevo is Cant - 1,
    lista_elementos_repetidos(CantNuevo, Nume, DadosAProbar2).

max_func([_,P1,_], [_,P2,_]) :- P1 < P2.

elegir_N_dados_random(_, 0, []).
elegir_N_dados_random(Dados, N, [Elegido|Elegidos]) :-
    N > 0,
    random_select(Elegido, Dados, Resto),
    M is N - 1,
    elegir_N_dados_random(Resto, M, Elegidos). 

elegir_N_categorias_random([], _, _, []).
elegir_N_categorias_random(_, 0, _, []).
elegir_N_categorias_random(Categorias, N, Tablero, CategoriasAProbar) :-
    N > 0,
    Categorias \= [],
    random_select(Categoria, Categorias, Resto),
    elegir_N_categorias_random_aux(Categoria, Resto, N, Tablero, CategoriasAProbar).
elegir_N_categorias_random_aux(Categoria, Resto, N, Tablero, [CategoriaAProbar|CategoriasAProbar]) :-
    datos_categoria(Categoria, Datos),
    Datos = [Cat, Pun, Cats],
    una_libre(Tablero, Cats),
    CategoriaAProbar = [Cat, Pun],
    M is N - 1,
    elegir_N_categorias_random(Resto, M, Tablero, CategoriasAProbar).
elegir_N_categorias_random_aux(Categoria, Resto, N, Tablero, CategoriasAProbar) :-
    datos_categoria(Categoria, Datos),
    Datos = [_, _, Cats],
    not(una_libre(Tablero, Cats)),
    elegir_N_categorias_random(Resto, N, Tablero, CategoriasAProbar).

% ----------------------- Condiciones eleccion de slot ia_det -----------------------

% Calcula el maximo puntaje que se puede obtener colocando la configuracion de dados en algun slot libre, y devuelve la categoria correspondiente
calcular_maximo(_, [], 0, nil).
calcular_maximo(Dados, [Elemento|Tablero], PuntosRes, CategoriaRes) :-
    (
        (
            Elemento = s(Categoria, nil),
            puntaje(Dados, Categoria, Puntos),
            calcular_maximo(Dados, Tablero, PuntosAnterior, CategoriaAnterior),
            (
                (
                    CategoriaAnterior = nil,
                    PuntosRes = Puntos,
                    CategoriaRes = Categoria
                );
		Puntos < PuntosAnterior -> PuntosRes = PuntosAnterior, CategoriaRes = CategoriaAnterior;
		Puntos > PuntosAnterior -> PuntosRes = Puntos, CategoriaRes = Categoria
            )
        );
        (calcular_maximo(Dados, Tablero, PuntosRes, CategoriaRes))
    ).

% Condicion 1: Si se tiene yahtzee y el slot esta libre lo elige
eleccion_slot_condicion1(Dados, Tablero, yathzee) :-
    slot_libre(Tablero, yathzee),
    generar_patron_lista(Dados, [X,X,X,X,X], _).

% Condicion 2: Si se tiene large_straight y el slot esta libre se elige
eleccion_slot_condicion2(Dados, Tablero, large_straight) :- 
    slot_libre(Tablero, large_straight),
    (
        generar_patron_lista(Dados, [1,2,3,4,5], _);
        generar_patron_lista(Dados, [2,3,4,5,6], _)
    ).

% Condicion 3: Si se tiene small_straight y el slot esta libre se elige
eleccion_slot_condicion3(Dados, Tablero, small_straight) :- 
    slot_libre(Tablero, small_straight),
    (
        generar_patron_lista(Dados, [1,2,3,4], _);
        generar_patron_lista(Dados, [2,3,4,5], _);
        generar_patron_lista(Dados, [3,4,5,6], _)
    ).

% Condicion 4: Si se tiene el slot en la upper section correspondiente al numero dado libre, y aparece almenos 3 veces se elige
eleccion_slot_condicion4(Dados, Tablero, Num, Categoria) :- 
    seccion_superior(Categoria, Num),
    slot_libre(Tablero, Categoria),
    generar_patron_lista(Dados, [Num,Num,Num], _).

% Condicion 5: Si se tiene full_house y su slot esta libre lo elige
eleccion_slot_condicion5(Dados, Tablero, full_house) :- 
    slot_libre(Tablero, full_house),
    generar_patron_lista(Dados, [X,X,X,Y,Y], _),
    Y \= X.

% Condicion 6: Si se tiene four_of_a_kind y su slot esta libre lo elige 
eleccion_slot_condicion6(Dados, Tablero, four_of_a_kind) :- 
    slot_libre(Tablero, four_of_a_kind),
    generar_patron_lista(Dados, [X,X,X,X], _).

% Condicion 7: Si se tiene three_of_a_kind y su slot esta libre lo elige 
eleccion_slot_condicion7(Dados, Tablero, three_of_a_kind) :- 
    slot_libre(Tablero, three_of_a_kind),
    generar_patron_lista(Dados, [X,X,X], _).

% Elige el slot correspondiente evaluando las condiciones en un orden especifico
eleccion_slot(Dados, Tablero, ia_det, Categoria) :- (
    eleccion_slot_condicion1(Dados, Tablero, Categoria1) -> Categoria = Categoria1;
    eleccion_slot_condicion2(Dados, Tablero, Categoria2) -> Categoria = Categoria2;
    eleccion_slot_condicion3(Dados, Tablero, Categoria3) -> Categoria = Categoria3;
    eleccion_slot_condicion4(Dados, Tablero, 6, Categoria4) -> Categoria = Categoria4;
    eleccion_slot_condicion4(Dados, Tablero, 5, Categoria5) -> Categoria = Categoria5;
    eleccion_slot_condicion5(Dados, Tablero, Categoria6) -> Categoria = Categoria6;
    eleccion_slot_condicion4(Dados, Tablero, 4, Categoria7) -> Categoria = Categoria7;
    eleccion_slot_condicion4(Dados, Tablero, 3, Categoria8) -> Categoria = Categoria8;
    eleccion_slot_condicion4(Dados, Tablero, 2, Categoria9) -> Categoria = Categoria9;
    eleccion_slot_condicion4(Dados, Tablero, 1, Categoria10) -> Categoria = Categoria10;
    eleccion_slot_condicion6(Dados, Tablero, Categoria11) -> Categoria = Categoria11;
    eleccion_slot_condicion7(Dados, Tablero, Categoria12) -> Categoria = Categoria12;
    slot_libre(Tablero, yahtzee) -> Categoria = yahtzee;
    slot_libre(Tablero, aces) -> Categoria = aces;
    slot_libre(Tablero, twos) -> Categoria = twos;
    calcular_maximo(Dados, Tablero, _, CategoriaRes) -> Categoria = CategoriaRes
).

% -----------------------------------------------------------------------------------    

eleccion_slot(_, _, humano, Categoria) :-
    writeln('¿Que categoria desea completar? Ingresar nombre'),
    read(Categoria).

eleccion_slot(Dados, Tablero, ia_prob, Categoria) :- 
    eleccion_slot(Dados, Tablero, ia_det, Categoria).

escribir_archivo(Fuente, Destino) :-
    read_line_to_string(Fuente, Contenido),
    (
        (
            Contenido \= end_of_file,
            write(Destino, Contenido),
            write(Destino, "\n"),
            escribir_archivo(Fuente, Destino)
        );
        (Contenido = end_of_file)
    ).

escribir_dados(Destino, Dados) :- escribir_dados(Destino, Dados, 1).
escribir_dados(_, [], _).
escribir_dados(Destino, [Dado|Dados], PosDado) :-
    atomic_list_concat(["evidence(d", PosDado, "(", Dado, ")).\n"], "", Str),
    write(Destino, Str),
    PosDadoNuevo is PosDado + 1,
    escribir_dados(Destino, Dados, PosDadoNuevo).

escribir_categorias([], _, _).
escribir_categorias([Categoria|CategoriasAProbar], Tablero, Destino) :-
    Categoria = [Cat, Pun],
    atomic_list_concat(["query(puntaje(", Cat, ",", Pun, ")).\n"], "", Str),
    write(Destino, Str),
    escribir_categorias(CategoriasAProbar, Tablero, Destino).

datos_categoria(full_house, ["full_house", "25", [full_house]]). 
datos_categoria(large_straight, ["large_straight", "40", [large_straight, small_straight]]). 
datos_categoria(small_straight, ["small_straight", "30", [large_straight, small_straight]]). 
datos_categoria(aces, ["aces", "X", [aces, yahtzee, four_of_a_kind, three_of_a_kind]]). 
datos_categoria(twos, ["twos", "X", [twos, yahtzee, four_of_a_kind, three_of_a_kind]]). 
datos_categoria(threes, ["threes", "X", [threes, yahtzee, four_of_a_kind, three_of_a_kind]]). 
datos_categoria(fours, ["fours", "X", [fours, yahtzee, four_of_a_kind, three_of_a_kind]]). 
datos_categoria(fives, ["fives", "X", [fives, yahtzee, four_of_a_kind, three_of_a_kind]]). 
datos_categoria(sixes, ["sixes", "X", [sixes, yahtzee, four_of_a_kind, three_of_a_kind]]). 

creacion_escritura_archivo(DadosAProbar, CategoriasAProbar, Tablero) :-
    % Abro el archivo a utilizar 
    open("./modelo_problog_res.pl", write, Destino),
    
    % Copio el contenido del modelo a este
    open("./modelo_problog.pl", read, Fuente),
    escribir_archivo(Fuente, Destino),
    close(Fuente, [force(true)]),

    % Escribo las evidencias y queries
    escribir_dados(Destino, DadosAProbar),
    escribir_categorias(CategoriasAProbar, Tablero, Destino), 

    % Cierro el archivo
    close(Destino, [force(true)]).
    
peticion_problog(CategoriasAProbar, DadosAProbar, Tablero, Final, FinalAnterior) :-
    absolute_file_name(path(problog), Problog, [access(exist)]),
    creacion_escritura_archivo(DadosAProbar, CategoriasAProbar, Tablero),
    absolute_file_name(modelo_problog_res, Modelo, [file_type(prolog)]),
    setup_call_cleanup(
        process_create(Problog, [Modelo], [stdout(pipe(In))]),
        read_string(In, _, Result),
        close(In)
    ),
    split_string(Result, "\n\t", "\r ", L1),
    append(L2, [_], L1),

    parse(L2, L3),
    juntar_posibilidades(L3, DadosAProbar, FinalAnterior, Final).

juntar_posibilidades([], _, FinalAnterior, FinalAnterior).
juntar_posibilidades([Elemento|Categorias], Elegidos, FinalAnterior, [Nuevo|Final]) :-
    Elemento = [C, _],
    not(member([C|_], FinalAnterior)), 
    juntar_posibilidades(Categorias, Elegidos, FinalAnterior, Final),
    append(Elemento, [Elegidos], Nuevo).
juntar_posibilidades([Elemento|Categorias], Elegidos, FinalAnterior, [Nuevo|Final]) :-
    Elemento = [C, P1],
    member([C|_], FinalAnterior), 
    juntar_posibilidades(Categorias, Elegidos, FinalAnterior, FinalRes),
    select([C,P2,D], FinalRes, Final), 
    M is max(P2, P1),
    (
        M = P1 -> Nuevo = [C, M, Elegidos];
        M = P2 -> Nuevo = [C, M, D]
    ).

cambio_coeficiente([], _, _, []).
cambio_coeficiente([Elemento|NuevaListaCoeficientes], Categoria, R, [Element|ListaCoeficientes]) :-
    Elemento = [Categoria, _],
    Element = [Categoria, R],
    cambio_coeficiente(NuevaListaCoeficientes, Categoria, R, ListaCoeficientes).
cambio_coeficiente([Elemento|NuevaListaCoeficientes], Categoria, R, [Elemento|ListaCoeficientes]) :-
    cambio_coeficiente(NuevaListaCoeficientes, Categoria, R, ListaCoeficientes).

parse([], []).
parse([String, Numero | ListaRetorno], ListaCoeficientes) :-
    parse(ListaRetorno, NuevaListaCoeficientes),
    split_string(String, ":", "", [Str,_]),
    atom_string(Res, Str),
    read_term_from_atom(Res, Termino, []),
    Termino =.. [_, Categoria, Puntos],
    atom_number(Numero, Probabilidad),
    (
        not(member([Categoria, _], NuevaListaCoeficientes)) -> (
            Res1 is Puntos*Probabilidad,
            ListaCoeficientes = [[Categoria, Res1] | NuevaListaCoeficientes]
        );
        member([Categoria, P], NuevaListaCoeficientes) -> (
            R is max(P, Puntos*Probabilidad),
            cambio_coeficiente(NuevaListaCoeficientes, Categoria, R, ListaCoeficientes)
        )
    ).

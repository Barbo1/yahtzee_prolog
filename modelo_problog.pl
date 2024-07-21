:- use_module(library(lists)).

1/6::d1(1); 1/6::d1(2); 1/6::d1(3); 1/6::d1(4); 1/6::d1(5); 1/6::d1(6).
1/6::d2(1); 1/6::d2(2); 1/6::d2(3); 1/6::d2(4); 1/6::d2(5); 1/6::d2(6).
1/6::d3(1); 1/6::d3(2); 1/6::d3(3); 1/6::d3(4); 1/6::d3(5); 1/6::d3(6).
1/6::d4(1); 1/6::d4(2); 1/6::d4(3); 1/6::d4(4); 1/6::d4(5); 1/6::d4(6).
1/6::d5(1); 1/6::d5(2); 1/6::d5(3); 1/6::d5(4); 1/6::d5(5); 1/6::d5(6).

% ------------ Seccion superior ------------
seccion_superior(aces, 1). 
seccion_superior(twos, 2).
seccion_superior(threes, 3).
seccion_superior(fours, 4).
seccion_superior(fives, 5).
seccion_superior(sixes, 6).


check(Numero,0,[]).

check(Numero, P, [Numero | Xs]):-
    check(Numero, P1, Xs),
    P is P1 + Numero.

check(Numero, P, [X | Xs]):-
    X \= Numero,
    check(Numero, P, Xs).

puntaje(Categoria,Puntos):-
    seccion_superior(Categoria, Numero),
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    (X1 is Numero; X2 is Numero; X3 is Numero; X4 is Numero; X5 is Numero),
    check(Numero, Puntos, [X1, X2, X3, X4, X5]).
% ------------------------------------------------

% ------------ Full house ------------
full_house_calcular(X, Y) :- d1(X), d2(X), d3(X), d4(Y), d5(Y).
full_house_calcular(X, Y) :- d1(X), d2(X), d3(Y), d4(X), d5(Y).
full_house_calcular(X, Y) :- d1(X), d2(Y), d3(X), d4(X), d5(Y).
full_house_calcular(X, Y) :- d1(Y), d2(X), d3(X), d4(X), d5(Y).
full_house_calcular(X, Y) :- d1(X), d2(X), d3(Y), d4(Y), d5(X).
full_house_calcular(X, Y) :- d1(X), d2(Y), d3(X), d4(Y), d5(X).
full_house_calcular(X, Y) :- d1(Y), d2(X), d3(X), d4(Y), d5(X).
full_house_calcular(X, Y) :- d1(X), d2(Y), d3(Y), d4(X), d5(X).
full_house_calcular(X, Y) :- d1(Y), d2(X), d3(Y), d4(X), d5(X).
full_house_calcular(X, Y) :- d1(Y), d2(Y), d3(X), d4(X), d5(X).

puntaje(full_house, 25):-
    between(1,6,X),
    between(1,6,Y),    
    X \= Y,
    full_house_calcular(X, Y).
% ------------------------------------    

% ------------ Small Straight ------------
puntaje(small_straight, 30):-
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    subset([1,2,3,4], [X1,X2,X3,X4,X5]).
        
puntaje(small_straight, 30):-
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    subset([2,3,4,5],[X1,X2,X3,X4,X5]).

puntaje(small_straight, 30):-
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    subset([3,4,5,6],[X1,X2,X3,X4,X5]).
% ----------------------------------------  

% ------------ Large Straight ------------
puntaje(large_straight, 40):-
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    permutation([X1,X2,X3,X4,X5], [1,2,3,4,5]).
puntaje(large_straight, 40):-
    d1(X1), d2(X2), d3(X3), d4(X4), d5(X5),
    permutation([X1,X2,X3,X4,X5], [2,3,4,5,6]).
% ---------------------------------------- 


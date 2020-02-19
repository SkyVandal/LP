% -------------------------------------------------------------
%                                   Hanoi
% -------------------------------------------------------------

hanoi :- write('Numero de discos: '),
             read(N),
             number(N),
             move(N, origem, destino, aux).

move(1, Origem, Destino, _) :- write(Origem), write(' --> '), write(Destino), nl.

move(N, Origem, Destino, Aux) :-
	N > 1,
	M is N - 1,
	move(M, Origem, Aux, Destino),
	move(1, Origem, Destino, _),
	move(M, Aux, Destino, Origem).

%---------------------

% Rebenta a  partir de 21

% -------------------------------------------------------------
%                                   Liga
% -------------------------------------------------------------

amigo(harry, ron).
amigo(lupin, harry).
ligado(X, Y) :- amigo(X, Y).
ligado(X, Z) :- ligado(X, Y), amigo(Y, Z).

/*
?- ligado(lupin, ron).
true ;
ERROR: Out of local stack
?- ligado(lupin, X).
X = harry ;
X = ron ;
ERROR: Out of local stack
?- ligado(X, ron).
X = harry ;
X = lupin ;
ERROR: Out of local stack
?- ligado(X, Y).
X = harry,
Y = ron ;
X = lupin,
Y = harry ;
X = lupin,
Y = ron ;
ERROR: Out of local stack
*/

%--------- Liga: Nova Versao --------

ligado1(X, Y) :- amigo(X, Y).
ligado1(X, Z) :- amigo(Y, Z), ligado1(X, Y).

/*
?- ligado1(X, Y).
X = harry,
Y = ron ;
X = lupin,
Y = harry ;
X = lupin,
Y = ron ;
false.

?- ligado1(X, ron).
X = harry ;
X = lupin ;
false.

?- ligado1(lupin, X).
X = harry ;
X = ron ;
false.
*/

/*-------- Liga: Outra Nova Versao --------
Que tambem da asneira -- elementos repetidos
*/

ligado2(X, Y) :- amigo(X, Y).
ligado2(X, Y) :- amigo(Y, X).
ligado2(X, Z) :- amigo(Y, Z), ligado2(X, Y).

% -------------------------------------------------------------
%                                   Factorial
% -------------------------------------------------------------

% Gera processo recursivo
factorial(1, 1).
factorial(N, F) :- N > 1,
                             N1 is N-1,
                            factorial(N1, F1),
                            F is N * F1.

% factorial(3, F).
% factorial(X, 6).
% Vai dar o erro dos "Arguments are not sufficiently instantiated"

% Gera processo iterativo
% Nao resolve erro anterior.  Ou seja tambem da asneira na polimodalidade

factorial1(N, F) :- fact(N, 1, F).
fact(1, F, F).
fact(N, Ac, F) :- N > 1,
                            Ac1 is N*Ac,
                            N1 is N-1,
                            fact(N1, Ac1, F).

% factorial1(X, 6).

%------------------------------------------------------------ LISTAS---------------------------------------------------------

% -------------------------------------------------------------
%                                   Ultimo
% -------------------------------------------------------------

ultimo([X], X).
ultimo([_|Cauda], X) :- ultimo(Cauda, X).

/*
?- ultimo([1], X).
?- ultimo([1, 2, 3], X).
?- trace(ultimo).
?- ultimo([1, 2, 3], X).
?- nodebug.
*/

% -------------------------------------------------------------
%                                   Soma
% -------------------------------------------------------------

soma([], 0).
soma([Cabeca | Cauda], N) :- soma(Cauda, M), N is M + Cabeca.

% soma([1, 5, 6], X).
% trace(soma).
% soma([1, 5, 6], X).

% -------------------------------------------------------------
%                       Membro de uma lista
% -------------------------------------------------------------

membro(X, [X | _]).
membro(X, [_| R]) :- membro(X, R).

/*
?- membro(4, [4, 3, 5]).
?- membro([a, b], [a, [a, b], c]).
?- membro(X, [a, b, c]).
?- membro(a, X). % O Prolog e maravilhoso
*/

% -------------------------------------------------------------
%                                Junta
% -------------------------------------------------------------

junta([], L, L).
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2).

/*
? - junta([], [a, b], L).
? - junta([a, b], X, [a, b, c, d]). % Polimodalidade
? - junta(X, Y, [1, 2, 3]). % Maravilhoso
*/

% -------------------------------------------------------------
%                             Inverte lista
% -------------------------------------------------------------

% Gera processo recursivo
inverte([], []).
inverte([P | R], L) :- inverte(R, L1),
                                  junta(L1, [P], L).

% Gera processo iterativo
inverte1(L, L1)  :- inverte1(L, [], L1).
inverte1([], L, L).
inverte1([P | R], Ac, L) :- inverte1(R, [P | Ac], L).

 /*
 ?- inverte([x, a, i, 1, 8], L).
L = [8, 1, i, a, x].

?- inverte1([x, a, i, 1, 8], L).
L = [8, 1, i, a, x].
*/

% -------------------------------------------------------------
%                Comprimentos de uma lista
% -------------------------------------------------------------

% Versao O(N) - requisito de memoria para lista de tamanho N
% Gera processo recursivo

comp1([], 0).
comp1([_| Cauda], N) :- comp1(Cauda, M), N is M + 1.

% Versao O(1) - requisito de memoria para lista de tamanho N
% Gera processo iterativo
comp2(L, C) :- comp2(L, 0, C).
comp2([], C, C).
comp2([_ | Cauda], Acum, C) :- Acum1 is Acum + 1, comp2(Cauda, Acum1, C).

/*
[debug]  ?- comp1([1, 2, 6], C).
T Call: (6) comp1([1, 2, 6], _G2303)
T Call: (7) comp1([2, 6], _G2417)
T Call: (8) comp1([6], _G2417)
T Call: (9) comp1([], _G2417)
T Exit: (9) comp1([], 0)
T Exit: (8) comp1([6], 1)
T Exit: (7) comp1([2, 6], 2)
T Exit: (6) comp1([1, 2, 6], 3)
C = 3.

debug]  ?- comp2([1, 2, 6], C).
T Call: (6) comp2([1, 2, 6], _G2303)
T Call: (7) comp2([1, 2, 6], 0, _G2303)
T Call: (8) comp2([2, 6], 1, _G2303)
T Call: (9) comp2([6], 2, _G2303)
T Call: (10) comp2([], 3, _G2303)
T Exit: (10) comp2([], 3, 3)
T Exit: (9) comp2([6], 2, 3)
T Exit: (8) comp2([2, 6], 1, 3)
T Exit: (7) comp2([1, 2, 6], 0, 3)
T Exit: (6) comp2([1, 2, 6], 3)
C = 3.
*/

% -------------------------------------------------------------
%                                   Escolhe
% -------------------------------------------------------------
escolhe([P | R], P, R).
escolhe([P | R], E, [P | S]) :- escolhe(R, E, S).

% ?- escolhe([a, b, c], b, [a, c]).
% ?- escolhe([a, b, c], X, Y).

% -------------------------------------------------------------
%                                   Permuta
% -------------------------------------------------------------
permuta([], []).
permuta(L, [H | T]) :- escolhe(L, H, L1), permuta(L1, T).

% ?- permuta([1, 2, 3], X).

% -------------------------------------------------------------
%                                   Ordenada
% -------------------------------------------------------------

ordenada([]).
ordenada([_]).
ordenada([X, Y | L]) :-  X =< Y, ordenada([Y | L]).

% ?- ordenada([1, 2,4,3]).

% -------------------------------------------------------------
%                                   Ordena
% -------------------------------------------------------------

ordena(L, L1) :- permuta(L, L1), ordenada(L1).

/*
Se elementos repetidos da varias vezes a mesma hipotese
?- ordena([1, 4, 6, 2, 8, 2, 19, 2, 1], X).
*/

%------------------------------------------------------------ CORTE---------------------------------------------------------

% -------------------------------------------------------------
%      Junta listas ordenadas (e elimina repetidos)
% -------------------------------------------------------------

% O corte e so para evitar calculos desnecessarios

junta_ord(L, [], L).
junta_ord([], L, L).
junta_ord([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca1 |  CaudaFinal ]) :-
    Cabeca1 < Cabeca2, junta_ord(Cauda1 , [ Cabeca2 | Cauda2 ], CaudaFinal).
junta_ord([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca1 |  CaudaFinal ]) :-
    Cabeca1 =:= Cabeca2, junta_ord(Cauda1 , Cauda2, CaudaFinal).
junta_ord([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca2 |  CaudaFinal ]) :-
    Cabeca1 > Cabeca2, junta_ord([ Cabeca1 | Cauda1 ] , Cauda2, CaudaFinal).

%?- junta_ord([1, 2, 4, 8], [1, 3, 6, 10], L).
%L = [1, 2, 3, 4, 6, 8, 10] ;
%false.

junta_ord1(L, [], L) :- !.
junta_ord1([], L, L) :- !.
junta_ord1([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca1 |  CaudaFinal ]) :-
    Cabeca1 < Cabeca2, !, junta_ord1(Cauda1 , [ Cabeca2 | Cauda2 ], CaudaFinal).
junta_ord1([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca1 |  CaudaFinal ]) :-
    Cabeca1 =:= Cabeca2, !, junta_ord1(Cauda1 , Cauda2, CaudaFinal).
junta_ord1([ Cabeca1 | Cauda1 ], [ Cabeca2 | Cauda2 ], [ Cabeca2 |  CaudaFinal ]) :-
    Cabeca1 > Cabeca2, !, junta_ord1([ Cabeca1 | Cauda1 ] , Cauda2, CaudaFinal).

%?- junta_ord1([1, 2, 4, 8], [1, 3, 6, 10], L).
%L = [1, 2, 3, 4, 6, 8, 10].
% Nao da hipotese de por ;

% -------------------------------------------------------------
%                      Remove repetidos
% -------------------------------------------------------------

% Versao errada, sem corte
remove_repetidos([], []).
remove_repetidos([ Cabeca | Cauda], NovaLista) :- member(Cabeca, Cauda),
     remove_repetidos(Cauda, NovaLista).
remove_repetidos([ Cabeca | Cauda], [Cabeca | NovaLista]) :- % nao deveria entrar aqui, mas entra se o corte nao existir
     remove_repetidos(Cauda, NovaLista).

%?- remove_repetidos([1, 2, 3, 8, 9, 1, 2], L).
%L = [3, 8, 9, 1, 2] ;
%L = [2, 3, 8, 9, 1, 2] ;
%L = [1, 3, 8, 9, 1, 2] ;
%L = [1, 2, 3, 8, 9, 1, 2].

% Versao fixola
remove_repetidos1([], []).
remove_repetidos1([ Cabeca | Cauda], NovaLista) :- member(Cabeca, Cauda), !,
     remove_repetidos1(Cauda, NovaLista).
remove_repetidos1([ Cabeca | Cauda], [Cabeca | NovaLista]) :- % nao deveria entrar aqui, mas entra se o corte nao existir
     remove_repetidos1(Cauda, NovaLista).


% -------------------------------------------------------------
%                             Soma - varias - revisao
% -------------------------------------------------------------

% soma1(L, N, S) e verdade se S for a soma dos elementos de L maiores que N.

% Versao recursiva

soma1([], _, 0) :- !.

soma1([H | T], N, S) :-
	H > N, !,
	soma1(T, N, S1),
	S is S1 + H.

soma1([H | T], N, S) :-
	H =< N, !,
	soma1(T, N, S).


% Versao iterativa

soma2(L, N, S) :- soma2(L, N, S, 0).

soma2([], _, Acc, Acc).

soma2([H | T], N, S, Ac) :-
	H > N, !,
	Ac1 is Ac + H,
	soma2(T, N, S, Ac1).

soma2([H | T], N, S, Ac) :-
	H =< N, !,
	soma2(T, N, S, Ac).

%%%%%%%%%%%%%%%%%

% soma3(L1, N, L2) e verdade se L2 for a lista com os elementos de L1 maiores que N.

% Versao recursiva

soma3([], _, []).
soma3([H | T], N, [H | L]) :-
	H > N, !,
	soma3(T, N, L).

/* Nota: também podia ser:
soma3([H | T], N, L1) :-
	H > N, !,
	soma3(T, N, L),
	L1 = [H|L].
*/

soma3([H | T], N, L) :-
	H =< N, !,
	soma3(T, N, L).

% Versao iterativa

soma4(L1, N, L2) :- soma4(L1, N, L2, []).

soma4([], _, Acc, Acc).

soma4([H | T], N, L, Ac) :-
	H > N, !,
	append(Ac, [H], Ac1), % se fosse Ac1 = [H | Ac], a lista ficava invertida
	soma4(T, N, L, Ac1).

soma4([H | T], N, L, Ac) :-
	H =< N, !,
	soma4(T, N, L, Ac).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

?- soma3([1, 2, 6, 9], 3, S).
S = [6, 9].

?- soma4([1, 2, 6, 9], 3, S).
S = [6, 9].

?- soma2([1, 2, 6, 9], 3, S).
S = 15.

?- soma1([1, 2, 6, 9], 3, S).
S = 15.
*/

%---------------------------------------------Predicados pre-definidos--------------------------------------------------------

p(3, 5, 2).
p(1, 3, 5).
p(2, 4, 2).
p(2, 4, 1).
p(4, 3, 1).

/*
?- findall(Z, p(X, Y, Z), Bag).
Bag = [2, 5, 2, 1, 1].

?- findall(Z, (p(X, Y, Z), Z > 2), Bag).
Bag = [5].

?- findall(Z, (p(X, Y, Z), Z > 5), Bag).
Bag = [].

?- bagof(Z, (p(X, Y, Z), Z > 5), Bag).
false.

?- bagof(Z, p(X, Y, Z), Bag).
X = 1,
Y = 3,
Bag = [5] ;
X = 2,
Y = 4,
Bag = [2, 1] ;
X = 3,
Y = 5,
Bag = [2] ;
X = 4,
Y = 3,
Bag = [1].

?- bagof(Z, X^p(X, Y, Z), Bag).
Y = 3,
Bag = [5, 1] ;
Y = 4,
Bag = [2, 1] ;
Y = 5,
Bag = [2].

?- bagof(Z, X^Y^p(X, Y, Z), Bag).
Bag = [2, 5, 2, 1, 1].

?- setof(Z, X^Y^p(X, Y, Z), Bag).
Bag = [1, 2, 5].
*/

%---------------------------------------------Homoiconicidade--------------------------------------------------------

%?- dynamic maisAlto/2.
%true.
%?- dynamic maisBaixo/2.
%true.

/*
?- asserta(maisAlto(hulk, thor)).
true.

?- asserta(maisAlto(hulk, capAmerica)).
true.

?- asserta(maisBaixo(X, Y) :- maisAlto(Y, X)).
true.

?- retractall(maisBaixo(X, Y)).
true.

?- retract(maisAlto(hulk, thor)).
true.

?- maisAlto(hulk, X).
X = capAmerica.
*/
% -------------------------------------------------------------
%                                   Fibo
% -------------------------------------------------------------

:- dynamic fib/2. % Execucao forcada

fib(0, 0).
fib(1, 1).
fib(X, F) :- X > 1, % mostrar o que acontece se esta guarda nao existir e fizer ; apos fib(0, 0)
      X1 is X -1,
      fib(X1, F1),
      X2 is X - 2,
      fib(X2, F2),
      F is F1 + F2,
      memoriza(fib(X, F)).

memoriza(L) :- asserta(L :- !).

/*
?- fib(6, X).
X = 8 ;
false.
*/

/*
?- listing(fib).
...
fib(6, 8) :- !.
fib(5, 5) :- !.
fib(4, 3) :- !.
fib(3, 2) :- !.
fib(2, 1) :- !.
fib(0, 0).
fib(1, 1).
fib(A, D) :-
	A>1,
	B is A+ -1,
	fib(B, E),
	C is A+ -2,
	fib(C, F),
	D is E+F,
	memoriza(fib(A, D)).
true.
*/

/*
?- functor(maisAlto(x, y), maisAlto, 2).
true.

?- functor(maisAlto(x, y), F, Ar).
F = maisAlto,
Ar = 2.

?- functor(T, xpto, 2).
T = xpto(_G900, _G901).

?- arg(1, maisAlto(x, y), x).
true.

?- arg(1, maisAlto(X, Y), z).
X = z.

?- T =.. [maisAlto, x, y].
T = maisAlto(x, y).

?- maisAlto(x, y) =.. [P | R].
P = maisAlto,
R = [x, y].

*/

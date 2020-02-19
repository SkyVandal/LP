:- consult(codigo_comum).
%:- consult(puzzles_publicos).

%------------------------------------------------------------------------------
%                                 Bruno Codinha
%                                     90707
%------------------------------------------------------------------------------

%----------------------------Predicado rule1-----------------------------------
% Predicado que verifica a regra 1 do projeto, que e nao haver 3 numeros
% iguais seguidos.
%------------------------------------------------------------------------------
rule1([V1, V2, V3]):-
    (V1 =:= V2) -> (V3 =\= V1);
    (V2 =:= V3) -> (V1 =\= V2);
    (V1 =:= V3) -> (V2 =\= V1).

%------------------------------Predicado switcher------------------------------
% Predicado que recebe um 0 ou 1 e retorna o oposto.
%------------------------------------------------------------------------------
switcher(V, Nv):-
    (V =:= 0) -> (Nv is 1);
    (V =:= 1) -> (Nv is 0).

%-----------------------------Predicado fill-----------------------------------
% Predicado que recebe uma lista de 3 e retorna a lista que corresponde a
% preencher uma posicao livre.
%------------------------------------------------------------------------------
fill([V1, V2, V3], R):-
  (nonvar(V1), nonvar(V2), nonvar(V3)) -> R = [V1, V2, V3];
  (var(V1), (V2 =:= V3)) -> (switcher(V2, Nv1), R = [Nv1, V2, V3]);
  (var(V2), (V1 =:= V3)) -> (switcher(V1, Nv2), R = [V1, Nv2, V3]);
  (var(V3), (V1 =:= V2)) -> (switcher(V2, Nv3), R = [V1, V2, Nv3]).

%----------------------------Predicado preenche_temp---------------------------
% Predicado que recebe uma lista de 3 e retorna uma lista de 3 com as variaveis
% iguais a 2, para motivos de comparacao.
%------------------------------------------------------------------------------
preenche_temp([], []).
preenche_temp([H|T], [H1|T1]):-
    preenche_temp(T, T1),
    (var(H),
        H1 is 2;
        H1 is H), !.

%---------------------------Predicado variable_counter-------------------------
% Predicado que recebe uma lista de 3 e retorna o numero de variaveis que
% ela contem.
%------------------------------------------------------------------------------
variable_counter([], 0).
variable_counter([H|T], N):-
    var(H),
    variable_counter(T, N1),
    N is N1 + 1,!.
variable_counter([_|T], N):-
    variable_counter(T, N).

%--------------------------Predicado aplica_R1_triplo--------------------------
% Predicado que recebe uma lista de 3, conta as variaveis, se forem maiores que
% 2, sabemos que a lista e indeterminada, de seguida sao aplicadas as regras
% mencionadas.
%------------------------------------------------------------------------------
aplica_R1_triplo([V1, V2, V3], Res):-
    variable_counter([V1, V2, V3], Counter),
    Counter < 2,
    preenche_temp([V1, V2, V3], List2),
    rule1(List2),!,
    fill([V1, V2, V3], Res).

aplica_R1_triplo([V1, V2, V3], [V1, V2, V3]):-
    [V1, V2, V3] == [1,1,1], !,
    [V1, V2, V3] == [0,0,0], !.

aplica_R1_triplo([V1, V2, V3], [V1, V2, V3]).

%------------------------Predicado aplica_R1_fila_aux--------------------------
% Predicado que recebe uma fila completa do puzzle e aplica a regra 1 a todas
% as combinacoes seguidas de 3 elementos, retorna a fila preenchida e valida-a
%------------------------------------------------------------------------------
aplica_R1_fila_aux([A, B], [A, B]).
aplica_R1_fila_aux([V1, V2, V3 |T], N_Fila):-
    aplica_R1_triplo([V1, V2, V3], [A, B, C]),!,
    aplica_R1_fila_aux([B, C |T], Fila),
    N_Fila = [A|Fila].

%------------------------Predicado aplica_R1_fila------------------------------
% Predicado que aplica a funcao acima ate que nao hajam mais movimentos
% possiveis.
%------------------------------------------------------------------------------
aplica_R1_fila(Fila, N_Fila):-
    aplica_R1_fila_aux(Fila, Nova_Fila),
    (Fila \== Nova_Fila) -> aplica_R1_fila(Nova_Fila, N_Fila),!.

aplica_R1_fila(Fila, N_Fila):-
    aplica_R1_fila_aux(Fila, Nova_Fila),
    (Fila == Nova_Fila) -> (N_Fila = Nova_Fila).

%--------------------------Predicado aplica_R2_fila_counter--------------------
% Predicado que recebe uma fila e conta o numero de zeros e uns
%------------------------------------------------------------------------------
aplica_R2_fila_counter([], 0, 0).
aplica_R2_fila_counter([H|T],Z_Count, O_Count):-
    var(H) -> aplica_R2_fila_counter(T, Z_Count, O_Count);
    (H =:= 0) -> (aplica_R2_fila_counter(T, Z, O_Count), Z_Count is Z + 1);
    (H =:= 1) -> (aplica_R2_fila_counter(T, Z_Count, O), O_Count is O + 1).

%------------------------Predicado fill2---------------------------------------
% Predicado que recebe uma fila e um numero N, ou 0 ou 1, e preenche todas as
% posicoes disponiveis com esse valor.
%------------------------------------------------------------------------------
fill2([], _, []).
fill2([H|T], N, [Nv|Nova_Fila]):-
    nonvar(H),
    Nv is H,
fill2(T, N, Nova_Fila).
fill2([H|T], N, [Nv|Nova_Fila]):-
    var(H),
    Nv is N,
    fill2(T, N, Nova_Fila).

%-------------------------Predicado aplica_R2_fila-----------------------------
% Predicado que recebe uma fila e aplica a regra 2 do projeto a essa fila,
% calcula-se o comprimento (M) de uma fila, o numero maximo de 0s e 1s nessa
% fila vai ser metade desse valor, de seguida aplica-se o predicado fill2, caso
% esse maximo tenha sido atingido.
%------------------------------------------------------------------------------
aplica_R2_fila(Fila, N_Fila):-
    aplica_R2_fila_counter(Fila, Zeros, Uns),
    length(Fila, M),
    Max is M//2,
    Zeros =< Max, !,
    Uns =< Max, !,
    (Zeros =:= Max -> fill2(Fila, 1, N_Fila);
    Uns =:= Max -> fill2(Fila, 0, N_Fila); N_Fila = Fila).

%------------------------Predicado aplica_R1_R2_fila---------------------------
% Predicado que aplica as regras 1 e 2 a uma fila e retorna a fila resultante
%------------------------------------------------------------------------------
aplica_R1_R2_fila(Fila, N_Fila):-
    aplica_R1_fila(Fila, F), !,
    aplica_R2_fila(F, N_Fila).

%------------------------Predicado aplica_R1_R2_puzzle_aux---------------------
% Predicado que recebe um puzzle, divide-o em filas e aplica a regra 1 e 2 a
% cada fila, retorna o puzzle resultante.
%------------------------------------------------------------------------------
aplica_R1_R2_puzzle_aux([], []).
aplica_R1_R2_puzzle_aux([Linha|Resto], [N_Linha|T]):-
    aplica_R1_R2_fila(Linha, N_Linha),
    aplica_R1_R2_puzzle_aux(Resto, T).

%----------------------Predicado aplica_R1_R2_puzzle_coluna--------------------
% Predicado que recebe um puzzle e retorna esse puzzle transposto e em seguida
% chama a funcao auxiliar acima, para aplicar as regras 1 e 2 tambem as colunas.
%------------------------------------------------------------------------------
aplica_R1_R2_puzzle_coluna(Puz, N_Puz):-
    mat_transposta(Puz, Puz_T),
    aplica_R1_R2_puzzle_aux(Puz_T, N_Puz).

%------------------Predicado aplica_R1_R2_puzzle-------------------------------
% Predicado que recebe um puzzle e aplica as regras 1 e 2 a todas as linhas
% e colunas, retorna o puzzle resultante.
%------------------------------------------------------------------------------
aplica_R1_R2_puzzle(Puz, N_Puz):-
    aplica_R1_R2_puzzle_aux(Puz, Novo_Puz),
    aplica_R1_R2_puzzle_coluna(Novo_Puz, T_Puz),
    mat_transposta(T_Puz, N_Puz).

%--------------------------Predicado inicializa--------------------------------
% Predicado que recebe um puzze e aplica as regras 1 e 2 a todas as linhas
% e colunas ate que nao hajam mais acoes possiveis.
%------------------------------------------------------------------------------
inicializa(Puz, N_Puz):-
    aplica_R1_R2_puzzle(Puz, T_Puz),
    (Puz \== T_Puz) -> inicializa(T_Puz, N_Puz),!.

inicializa(Puz, N_Puz):-
    aplica_R1_R2_puzzle(Puz, T_Puz),
    (Puz == T_Puz) -> (N_Puz = T_Puz).

%------------------------Predicado verifica_R3_tester--------------------------
% Predicado que recebe um conjunto de linhas (todas as linhas do puzzle menos
% a selecionada) e uma linha, e verifica se todas as linhas do conjunto sao
% diferentes da linha dada.
%------------------------------------------------------------------------------
verifica_R3_tester([], _).
verifica_R3_tester([L|T], Linha):-
    (L \== Linha), !,
    verifica_R3_tester(T, Linha).

%-------------------------Predicado verifica_R3_aux----------------------------
% Predicado que recebe um puzzle, pega em cada linha e envia-a para o
% predicado verifica_R3_tester, juntamente com o conjunto de linhas
%------------------------------------------------------------------------------
verifica_R3_aux([]).
verifica_R3_aux([Linha|T]):-
    verifica_R3_tester(T, Linha),
    verifica_R3_aux(T).

%--------------------------Predicado verifica_R3-------------------------------
% Predicado que envia o puzzle para a auxiliar avaliar as linhas, se seguida
% faz a transposta do puzzle, e envia novamente para avaliar as colunas.
%------------------------------------------------------------------------------
verifica_R3(Puz):-
    verifica_R3_aux(Puz),
    mat_transposta(Puz, T_Puz),
    verifica_R3_aux(T_Puz).

%-------------------Predicado posicoes_alteradas_coluna------------------------
% Predicado que recebe uma coluna original(Fila1), uma coluna modificada(Fila2),
% o numero da coluna, e retorna as posicoes que foram alteradas.
%------------------------------------------------------------------------------
posicoes_alteradas_coluna(Fila1, Fila2, Coluna, Counter, Pos):- posicoes_alteradas_coluna(Fila1, Fila2, Coluna, Counter, Pos, []).

posicoes_alteradas_coluna([], [], _, _, P, P).

posicoes_alteradas_coluna([H|T], [H1|T1], Coluna, Counter, Pos, Aux):-
    var(H), var(H1), !,
    C is Counter + 1,
    posicoes_alteradas_coluna(T, T1, Coluna, C, Pos, Aux).

posicoes_alteradas_coluna([H|T], [H1|T1], Coluna, Counter, Pos, Aux):-
    H \== H1, !,
    El = [(Counter, Coluna)],
    union(Aux, El, Aux1),
    C is Counter + 1,
    posicoes_alteradas_coluna(T, T1, Coluna, C, Pos, Aux1).

posicoes_alteradas_coluna([H|T], [H1|T1], Coluna, Counter, Pos, Aux):-
    H == H1, !,
    C is Counter + 1,
    posicoes_alteradas_coluna(T, T1, Coluna, C, Pos, Aux).

%-------------------Predicado posicoes_alteradas_linha------------------------
% Predicado que recebe uma linha original(Fila1), uma linha modificada(Fila2),
% o numero da linha, e retorna as posicoes que foram alteradas.
%------------------------------------------------------------------------------
posicoes_alteradas_linha(Fila1, Fila2, Linha, Counter, Pos):- posicoes_alteradas_linha(Fila1, Fila2, Linha, Counter, Pos, []).

posicoes_alteradas_linha([], [], _, _, P, P).

posicoes_alteradas_linha([H|T], [H1|T1], Linha, Counter, Pos, Aux):-
    var(H), var(H1), !,
    C is Counter + 1,
    posicoes_alteradas_linha(T, T1, Linha, C, Pos, Aux).

posicoes_alteradas_linha([H|T], [H1|T1], Linha, Counter, Pos, Aux):-
    H \== H1, !,
    El = [(Linha, Counter)],
    union(Aux, El, Aux1),
    C is Counter + 1,
    posicoes_alteradas_linha(T, T1, Linha, C, Pos, Aux1).

posicoes_alteradas_linha([H|T], [H1|T1], Linha, Counter, Pos, Aux):-
    H == H1, !,
    C is Counter + 1,
    posicoes_alteradas_linha(T, T1, Linha, C, Pos, Aux).

%-------------------Predicado propaga_posicoes---------------------------------
% Predicado que recebe uma lista de posicoes e um puzzle, tem como objetivo
% aplicas as regras 1 e 2 a cada linha e coluna na lista de posicoes, e ainda
% descobrir quais as posicoes alteradas e voltar a visitar essas linhas e
% colunas.
%------------------------------------------------------------------------------
propaga_posicoes([], N_Puz, N_Puz).
propaga_posicoes([(L, C)|Tail], Puz, N_Puz):-

    mat_elementos_coluna(Puz, C, Coluna),
    aplica_R1_R2_fila(Coluna, N_Col),
    posicoes_alteradas_coluna(Coluna, N_Col, C, 1, Posicoes_alteradas),
    union(Posicoes_alteradas, Tail, Pos_A1),
    mat_muda_coluna(Puz, C, N_Col, Novo_Puz),

    nth1(L, Novo_Puz, Linha),
    aplica_R1_R2_fila(Linha, N_Linha),
    posicoes_alteradas_linha(Linha, N_Linha, L, 1, Posicoes_alt),
    union(Posicoes_alt, Pos_A1, Pos_A2),
    mat_muda_linha(Novo_Puz, L, N_Linha, Novo_P1),

    verifica_R3(Novo_P1),

    propaga_posicoes(Pos_A2, Novo_P1, N_Puz).

%-----------------------Predicado e_variavel-----------------------------------
% Predicado que revebe um puzzle e uma posicao e determina se essa posicao e uma
% variavel livre.
%------------------------------------------------------------------------------
e_variavel(Mat, (L, C)) :-
    nth1(L,Mat,Linha),
    nth1(C,Linha,El),
    var(El).

%-------------------------Predicado verifica_hipoteses-------------------------
% Predicado que recebe um puzzle, uma lista de posicoes, e retorna uma lista
% com posicoes que estao livres no puzzle.
%------------------------------------------------------------------------------
verifica_hipoteses(_, [], []).
verifica_hipoteses(Puz, [(L,C)|Tail], [(L,C)|Disponiveis]):-
    e_variavel(Puz, (L,C)),!,
    verifica_hipoteses(Puz, Tail, Disponiveis).

verifica_hipoteses(Puz, [_|Tail], Disponiveis):-
    verifica_hipoteses(Puz, Tail, Disponiveis).

%--------------------------Predicado resolve_aux-------------------------------
% Predicado que recebe um puzzle, uma Lista de posicoes livres e uma hipotese
% (0 ou 1), este predicado testa uma hipotese com 0 ou 1 e propaga essa mudanca
%------------------------------------------------------------------------------
resolve_aux(P, P, [], _).
resolve_aux(Puz, Puzzle, [(L,C)|Tail], Hip):-
    mat_muda_posicao(Puz, (L,C), Hip, Novo_Puz),
    propaga_posicoes([(L,C)], Novo_Puz, Puzzle), !,
    inicializa(Puzzle, Puzzle_I),!,
    resolve_aux(Puzzle, Puzzle_I, Tail, 0).

resolve_aux(Puz, Puzzle, [(L,C)|Tail], 1):-
    mat_muda_posicao(Puz, (L,C), 1, Novo_Puz),
    propaga_posicoes([(L,C)], Novo_Puz, Puzzle), !,
    inicializa(Puzzle, Puzzle_I),!,
    resolve_aux(Puzzle, Puzzle_I, Tail, 0).

resolve_aux(Puz, Puzzle, [_|Tail], _):-
    resolve_aux(Puz, Puzzle, Tail, 0).

%-------------------------Predicado resolve------------------------------------
% Predicado final que gera todas as posicoes da matriz, inicializa o puzzle,
% verifica se as posicoes sao variaveis livres e envia essa informacao para
% a auxiliar avaliar os casos.
%------------------------------------------------------------------------------
resolve(Puz, Sol):-
    mat_dimensoes(Puz, Dim, _),
    findall((L, C), (between(1,Dim, L), between(1,Dim, C)), Hipoteses),
    inicializa(Puz, Puz_I),
    verifica_hipoteses(Puz_I, Hipoteses, Disponiveis),
    resolve_aux(Puz_I, Sol, Disponiveis, 0).

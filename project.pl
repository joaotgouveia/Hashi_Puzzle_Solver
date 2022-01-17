gera_ilha(P, Y, X, Ilha) :- Ilha =.. [ilha, P, (Y, X)].

extrai_ilhas_linha(Y, Linha, Ilhas) :- extrai_ilhas_linha(Y, 0, Linha, Ilhas).
extrai_ilhas_linha(_, _, [], []).
extrai_ilhas_linha(Y, X, [P|R], [Q|R1]) :-
    P =\= 0, !,
    X_N is X + 1,
    gera_ilha(P, Y, X_N, Q),
    extrai_ilhas_linha(Y, X_N, R, R1).
extrai_ilhas_linha(Y, X, [_|R], Ilhas) :-
    X_N is X + 1,
    extrai_ilhas_linha(Y, X_N, R, Ilhas).

ilhas(Puz, Ilhas) :- ilhas(0, Puz, Ilhas, []).
ilhas(_, [], IlhasFinal, IlhasFinal).
ilhas(Y, [L|R], Ilhas, IlhasFinal) :-
    Y_N is Y + 1,
    extrai_ilhas_linha(Y_N, L, Ilhas_Linha),
    append(IlhasFinal, Ilhas_Linha, Ilhas_N),
    ilhas(Y_N, R, Ilhas, Ilhas_N).

ilhas_x(Ilha1, Ilha2) :-
    Ilha1 =.. [_, _, (Y, X)],
    Ilha2 =.. [_, _, (Y1, X1)],
    Y =\= Y1,
    X =:= X1.

ilhas_y(Ilha1, Ilha2) :-
    Ilha1 =.. [_, _, (Y, X)],
    Ilha2 =.. [_, _, (Y1, X1)],
    X =\= X1,
    Y =:= Y1.

max(Lista, Max) :- max(Lista, Max, 0).
max([], Max, Max).
max([P|R], Max, Aux) :-
    Aux < P,
    max(R, Max, P).
max([_|R], Max, Aux) :- max(R, Max, Aux).

x_max(Ilhas, X_MAX) :-
    findall(X, (member(Ilha, Ilhas), Ilha =.. [_,_,(_,X)]), Cord_X),
    max(Cord_X, X_MAX).

y_max(Ilhas, Y_MAX) :-
    findall(Y, (member(Ilha, Ilhas), Ilha =.. [_,_,(Y,_)]), Cord_Y),
    max(Cord_Y, Y_MAX).

vizinha_baixo(Mult, Ilhas, Ilha, Vizinhas) :-
    include(ilhas_x(Ilha), Ilhas, Ilhas_N),
    y_max(Ilhas_N, Y_MAX),
    vizinha_baixo(Ilhas_N, Ilha, Vizinhas, _, 0, Y_MAX, Mult).
vizinha_baixo(_, _, VizinhaYFinal, VizinhaYFinal, _, _, _) :- VizinhaYFinal \= [].
vizinha_baixo(_, Ilha, VizinhaYFinal, VizinhaYFinal, Y_Sum, Y_MAX, Mult) :-
    Ilha =.. [_,_,(Y,_)],
    Mult =:= 1,
    Y + Y_Sum >= Y_MAX.
vizinha_baixo(_, Ilha, VizinhaYFinal, VizinhaYFinal, Y_Sum, _, Mult) :-
    Ilha =.. [_,_,(Y,_)],
    Mult =:= -1,
    Y - Y_Sum =< 0.
vizinha_baixo(Ilhas, Ilha, VizinhaY, [], Y_Sum, Y_MAX, Mult) :-
    Y_Sum_N is Y_Sum + 1,
    Ilha =.. [_,_,(Y,_)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= +(Y,Y_Sum_N*Mult)), VizinhaYAux),
    vizinha_baixo(Ilhas, Ilha, VizinhaY, VizinhaYAux, Y_Sum_N, Y_MAX, Mult).

vizinha_direita(Mult, Ilhas, Ilha, Vizinhas) :-
    include(ilhas_y(Ilha), Ilhas, Ilhas_N),
    x_max(Ilhas_N, X_MAX),
    vizinha_direita(Ilhas_N, Ilha, Vizinhas, _, 0, X_MAX, Mult).
vizinha_direita(_, _, VizinhaXFinal, VizinhaXFinal, _, _, _) :- VizinhaXFinal \= [].
vizinha_direita(_, Ilha, VizinhaXFinal, VizinhaXFinal, X_Sum, X_MAX, Mult) :-
    Ilha =.. [_,_,(_,X)],
    Mult =:= 1,
    X + X_Sum >= X_MAX.
vizinha_direita(_, Ilha, VizinhaXFinal, VizinhaXFinal, X_Sum, _, Mult) :-
    Ilha =.. [_,_,(_,X)],
    Mult =:= -1,
    X - X_Sum =< 0.
vizinha_direita(Ilhas, Ilha, VizinhaX, [], X_Sum, X_MAX, Mult) :-
    X_Sum_N is X_Sum + 1,
    Ilha =.. [_,_,(_,X)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= +(X,X_Sum_N*Mult)), VizinhaXAux),
    vizinha_direita(Ilhas, Ilha, VizinhaX, VizinhaXAux, X_Sum_N, X_MAX, Mult).

vizinhas(Ilhas, Ilha, Vizinhas) :-
    vizinha_direita(1, Ilhas, Ilha, VizinhaD),
    vizinha_direita(-1, Ilhas, Ilha, VizinhaE),
    vizinha_baixo(1, Ilhas, Ilha, VizinhaB),
    vizinha_baixo(-1, Ilhas, Ilha, VizinhaC),
    append(VizinhaC, VizinhaE, VizinhasAux),
    append(VizinhasAux, VizinhaD, VizinhasAux_N),
    append(VizinhasAux_N, VizinhaB, Vizinhas).





estado(Ilhas, Estado) :- estado(Ilhas, Estado, Ilhas).
estado([], [],_).
estado([Ilha|R], [Estado|R1], Ilhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas),
    Estado = [Ilha, Vizinhas, []],
    estado(R, R1, Ilhas).

posicoes_entre_x(Pos1, Pos2, Posicoes) :- posicoes_entre_x(Pos1, Pos2, Posicoes, 0).
posicoes_entre_x((Y, X1), (Y, X2), [], Pos_Sum) :-  X1 + Pos_Sum + 1 =:= X2, !.
posicoes_entre_x((Y, X1), (Y, X2), [P|R], Pos_Sum) :-
    Pos_Sum_N is Pos_Sum + 1,
    X_N is X1 + Pos_Sum_N,
    P = (Y, X_N),
    posicoes_entre_x((Y, X1), (Y, X2), R, Pos_Sum_N).

posicoes_entre_y(Pos1, Pos2, Posicoes) :- posicoes_entre_y(Pos1, Pos2, Posicoes, 0).
posicoes_entre_y((Y1, X), (Y2, X), [], Pos_Sum) :-  Y1 + Pos_Sum + 1 =:= Y2, !.
posicoes_entre_y((Y1, X), (Y2, X), [P|R], Pos_Sum) :-
    Pos_Sum_N is Pos_Sum + 1,
    Y_N is Y1 + Pos_Sum_N,
    P = (Y_N, X),
    posicoes_entre_y((Y1, X), (Y2, X), R, Pos_Sum_N).

posicoes_entre((Y1, X), (Y2, X), Posicoes) :-
    Y1 > Y2,
    posicoes_entre_y((Y2,X), (Y1, X), Posicoes).
posicoes_entre((Y1, X), (Y2, X), Posicoes) :-
    Y2 > Y1,
    posicoes_entre_y((Y1,X), (Y2, X), Posicoes).
posicoes_entre((Y, X1), (Y, X2), Posicoes) :-
    X1 > X2,
    posicoes_entre_x((Y,X2), (Y, X1), Posicoes).
posicoes_entre((Y, X1), (Y, X2), Posicoes) :-
    X2 > X1,
    posicoes_entre_x((Y,X1), (Y, X2), Posicoes).

cria_ponte((Y1, X1), (Y2, X2), Ponte) :-
    Y1 < Y2,
    Ponte =.. [ponte, (Y1, X1), (Y2, X2)].
cria_ponte((Y1, X1), (Y2, X2), Ponte) :-
    Y2 < Y1,
    Ponte =.. [ponte, (Y2, X2), (Y1, X1)].
cria_ponte((Y1, X1), (Y2, X2), Ponte) :-
    Y1 =:= Y2,
    X1 < X2,
    Ponte =.. [ponte, (Y1, X1), (Y2, X2)].
cria_ponte((Y1, X1), (Y2, X2), Ponte) :-
    Y1 =:= Y2,
    X2 < X1,
    Ponte =.. [ponte, (Y2, X2), (Y1, X1)].

caminho_livre((Y1,X1), (Y2,X2), _, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :- !.
caminho_livre((Y2,X2), (Y1,X1), _, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :- !.
caminho_livre(_, _, Posicoes, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :-
    posicoes_entre((Y1, X1), (Y2, X2), Caminho),
    findall(El1, (member(El1, Posicoes), member(El2, Caminho), El1 = El2), PontosComuns),
    PontosComuns \= [_].

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas_N, Pontes]) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha)), Vizinhas_N).

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_Estado).

ilhas_terminadas(Estado, Ilhas_Term) :-
    findall(Ilha, (member(El, Estado), El = [Ilha, _, Pontes], Ilha = ilha(P,_), P \= 'X', length(Pontes, P)), Ilhas_Term).

tira_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas_N, Pontes]) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), member(Terminada, Ilhas_Term), Vizinha = Terminada), Vizinhas_Term),
    subtract(Vizinhas, Vizinhas_Term, Vizinhas_N).

tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

marca_ilhas_terminadas_entrada(Ilhas_Term, [ilha(P, (Y,X)), Vizinhas, Pontes], [ilha('X', (Y,X)), Vizinhas, Pontes]) :-
    member(ilha(P, (Y,X)), Ilhas_Term).
marca_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas, Pontes]) :-
    \+ member(Ilha, Ilhas_Term).

marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

trata_ilhas_terminadas(Estado, Novo_Estado) :-
    ilhas_terminadas(Estado, Ilhas_Term),
    tira_ilhas_terminadas(Estado, Ilhas_Term, EstadoAux),
    marca_ilhas_terminadas(EstadoAux, Ilhas_Term, Novo_Estado).

ordena_estado(Estado, Novo_Estado) :-
    sort([1,2,2], @=<, Estado, Novo_Estado_Aux),
    sort([1,2,1], @=<, Novo_Estado_Aux, Novo_Estado).

junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_Estado) :-
    Ilha1 = ilha(_, (Y1,X1)),
    Ilha2 = ilha(_, (Y2,X2)),
    cria_ponte((Y1,X1), (Y2, X2), Ponte),
    length(Lista_Pontes, Num_Pontes),
    maplist(=(Ponte), Lista_Pontes),
    ListaIlhas = [Ilha1, Ilha2],
    findall([Ilha, Vizinhas, Pontes], (member([Ilha, Vizinhas, Pontes], Estado), member(Ilha, ListaIlhas)), Entradas),
    subtract(Estado, Entradas, Estado_Aux),
    Entradas = [[Ilha1, Vizinhas1, Pontes1], [Ilha2, Vizinhas2, Pontes2]],
    append(Pontes1, Lista_Pontes, Pontes1_N),
    append(Pontes2, Lista_Pontes, Pontes2_N),
    Entradas_N = [[Ilha1, Vizinhas1, Pontes1_N], [Ilha2, Vizinhas2, Pontes2_N]],
    append(Estado_Aux, Entradas_N, Novo_Estado_Aux_Nao_Ordenado),
    ordena_estado(Novo_Estado_Aux_Nao_Ordenado, Novo_Estado_Aux),
    trata_ilhas_terminadas(Novo_Estado_Aux, Novo_Estado_Aux_N),
    actualiza_vizinhas_apos_pontes(Novo_Estado_Aux_N, (Y1, X1), (Y2, X2), Novo_Estado).

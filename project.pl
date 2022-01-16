gera_ilha(Ilha, P, Y, X) :- Ilha =.. [ilha, P, (Y, X)].

extrai_ilhas_linha(Y, Linha, Ilhas) :- extrai_ilhas_linha(Y, 0, Linha, Ilhas).
extrai_ilhas_linha(_, _, [], []).
extrai_ilhas_linha(Y, X, [P|R], [Q|R1]) :-
    P \== 0, !,
    X_N is X + 1,
    gera_ilha(Q, P, Y, X_N),
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

ordem_leitura(Ilhas, IlhasOrdenadas) :- ordem_leitura(Ilhas, IlhasOrdenadas, []).
ordem_leitura([], IlhasOrdenadas, IlhasOrdenadas).

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

vizinhas_x(Ilhas, Ilha, Vizinhas) :- include(ilhas_x(Ilha), Ilhas, Ilhas_N), y_max(Ilhas_N, Y_MAX), vizinhas_x(Ilhas_N, Ilha, Vizinhas, [[],[]], 0, Y_MAX).
vizinhas_x(_, _, [P, Q], [P, Q], _, _) :- P \= [], Q \= [].
vizinhas_x(_, Ilha, VizinhasYFinal, VizinhasYFinal, Y_Sum, Y_MAX) :-
    Ilha =.. [_,_,(Y,_)],
    Y + Y_Sum >= Y_MAX,
    Y - Y_Sum =< 0.
vizinhas_x(Ilhas, Ilha, VizinhasY, [C,B], Y_Sum, Y_MAX) :-
    Y_Sum_N is Y_Sum + 1,
    Ilha =.. [_,_,(Y,_)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= +(Y,Y_Sum_N)), VizinhoBaixo),
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= -(Y,Y_Sum_N)), VizinhoCima),
    C \= [_],
    B \= [_],
    vizinhas_x(Ilhas, Ilha, VizinhasY, [VizinhoCima, VizinhoBaixo], Y_Sum_N, Y_MAX).
vizinhas_x(Ilhas, Ilha, VizinhasY, [C,B], Y_Sum, Y_MAX) :-
    Y_Sum_N is Y_Sum + 1,
    Ilha =.. [_,_,(Y,_)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= -(Y,Y_Sum_N)), VizinhoCima),
    C \= [_],
    vizinhas_x(Ilhas, Ilha, VizinhasY, [VizinhoCima, B], Y_Sum_N, Y_MAX).
vizinhas_x(Ilhas, Ilha, VizinhasY, [C,B], Y_Sum, Y_MAX) :-
    Y_Sum_N is Y_Sum + 1,
    Ilha =.. [_,_,(Y,_)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= +(Y,Y_Sum_N)), VizinhoBaixo),
    B \= [_],
    vizinhas_x(Ilhas, Ilha, VizinhasY, [C, VizinhoBaixo], Y_Sum_N, Y_MAX).

vizinhas_y(Ilhas, Ilha, Vizinhas) :- include(ilhas_y(Ilha), Ilhas, Ilhas_N), x_max(Ilhas_N, X_MAX), vizinhas_y(Ilhas_N, Ilha, Vizinhas, [[],[]], 0, X_MAX).
vizinhas_y(_, _, [P, Q], [P, Q], _, _) :- P \= [], Q \= [].
vizinhas_y(_, Ilha, VizinhasXFinal, VizinhasXFinal, X_Sum, X_MAX) :-
    Ilha =.. [_,_,(_,X)],
    X + X_Sum >= X_MAX,
    X - X_Sum =< 0.
vizinhas_y(Ilhas, Ilha, VizinhasX, [E,D], X_Sum, X_MAX) :-
    X_Sum_N is X_Sum + 1,
    Ilha =.. [_,_,(_,X)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= +(X,X_Sum_N)), VizinhoDir),
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= -(X,X_Sum_N)), VizinhoEsq),
    E \= [_],
    D \= [_],
    vizinhas_y(Ilhas, Ilha, VizinhasX, [VizinhoEsq, VizinhoDir], X_Sum_N, X_MAX).
vizinhas_y(Ilhas, Ilha, VizinhasX, [E,D], X_Sum, X_MAX) :-
    X_Sum_N is X_Sum + 1,
    Ilha =.. [_,_,(_,X)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= +(X,X_Sum_N)), VizinhoDir),
    D \= [_],
    vizinhas_y(Ilhas, Ilha, VizinhasX, [E, VizinhoDir], X_Sum_N, X_MAX).
vizinhas_y(Ilhas, Ilha, VizinhasX, [E,D], X_Sum, X_MAX) :-
    X_Sum_N is X_Sum + 1,
    Ilha =.. [_,_,(_,X)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= -(X,X_Sum_N)), VizinhoEsq),
    E \= [_],
    vizinhas_y(Ilhas, Ilha, VizinhasX, [VizinhoEsq, D], X_Sum_N, X_MAX).

vizinhas(Ilhas, Ilha, Vizinhas) :-
    vizinhas_x(Ilhas, Ilha, VizinhasX),
    vizinhas_y(Ilhas, Ilha, VizinhasY),
    append(VizinhasX, VizinhasY, VizinhasAux),
    findall(IlhaVizinha, (member(L, VizinhasAux), L = [IlhaVizinha]), Vizinhas).

estado([], []).
estado([Ilha|R], [Estado|R1]) :-
    vizinhas(R, Ilha, Vizinhas),
    Estado = [Ilha, Vizinhas, []],
    estado(R, R1).

posicoes_entre_x(Pos1, Pos2, Posicoes) :- posicoes_entre_x(Pos1, Pos2, Posicoes, 0).
posicoes_entre_x((Y, X1), (Y, X2), [], Pos_Sum) :-  X1 + Pos_Sum + 1 =:= X2.
posicoes_entre_x((Y, X1), (Y, X2), [P|R], Pos_Sum) :-
    Pos_Sum_N is Pos_Sum + 1,
    X_N is X1 + Pos_Sum_N,
    P = (Y, X_N),
    posicoes_entre_x((Y, X1), (Y, X2), R, Pos_Sum_N).

posicoes_entre_y(Pos1, Pos2, Posicoes) :- posicoes_entre_y(Pos1, Pos2, Posicoes, 0).
posicoes_entre_y((Y1, X), (Y2, X), [], Pos_Sum) :-  Y1 + Pos_Sum + 1 =:= Y2.
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
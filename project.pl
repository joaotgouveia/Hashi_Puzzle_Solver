% Joao Talone Gouveia, 102611.

%-----------------------------------------------------------------------------
% gera_ilha(P, Y, X, Ilha):
% Ilha e' uma estrutura da forma ilha(P,(Y,X)). Predicado auxiliar que ajuda 
% com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
gera_ilha(P, Y, X, ilha(P, (Y, X))).

%-----------------------------------------------------------------------------
% extrai_ilhas_linha(Y, Linha, Ilhas):
% Ilhas e' o resultado de extrair as ilhas da linha numero Y, cujo conteudo esta em Linha.
%-----------------------------------------------------------------------------
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

%-----------------------------------------------------------------------------
% ilhas(Puz, Ilhas):
% Ilhas e' o resultado de extrair as ilhas de todas as linhas de Puz.
%-----------------------------------------------------------------------------
ilhas(Puz, Ilhas) :- ilhas(0, Puz, Ilhas, []).
ilhas(_, [], IlhasFinal, IlhasFinal).
ilhas(Y, [L|R], Ilhas, IlhasFinal) :-
    Y_N is Y + 1,
    extrai_ilhas_linha(Y_N, L, Ilhas_Linha),
    append(IlhasFinal, Ilhas_Linha, Ilhas_N),
    ilhas(Y_N, R, Ilhas, Ilhas_N).

%-----------------------------------------------------------------------------
% ilhas_x(Ilha1, Ilha2):
% Este predicado averigua se as ilhas sao diferentes mas
% tem componentes X iguais. Predicado auxiliar que ajuda 
% com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
ilhas_x(ilha(_,(Y,X)), ilha(_,(Y1,X))) :-
    Y =\= Y1.

%-----------------------------------------------------------------------------
% ilhas_y(Ilha1, Ilha2):
% Este predicado averigua se as ilhas sao diferentes mas
% tem componentes Y iguais. Predicado auxiliar que ajuda 
% com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
ilhas_y(ilha(_,(Y,X)), ilha(_,(Y,X1))) :-
    X =\= X1.

%-----------------------------------------------------------------------------
% max(Lista, Max):
% Max e' o valor mais alto em Lista. Predicado auxiliar que ajuda com a clareza e
% e organizacao do codigo, reduzindo tambem a quantidade de codigo repetido.
%-----------------------------------------------------------------------------
max(Lista, Max) :- max(Lista, Max, 0).
max([], Max, Max).
max([P|R], Max, Aux) :-
    Aux < P,
    max(R, Max, P).
max([_|R], Max, Aux) :- max(R, Max, Aux).

%-----------------------------------------------------------------------------
% x_max(Ilhas, X_MAX):
% X_MAX e' o valor mais alto da componente x das ilhas em Ilhas.
% Predicado auxiliar que ajuda com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
x_max(Ilhas, X_MAX) :-
    findall(X, (member(Ilha, Ilhas), Ilha =.. [_,_,(_,X)]), Cord_X),
    max(Cord_X, X_MAX).

%-----------------------------------------------------------------------------
% y_max(Ilhas, Y_MAX):
% Y_MAX e' o valor mais alto da componente y das ilhas em Ilhas.
% Predicado auxiliar que ajuda com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
y_max(Ilhas, Y_MAX) :-
    findall(Y, (member(Ilha, Ilhas), Ilha =.. [_,_,(Y,_)]), Cord_Y),
    max(Cord_Y, Y_MAX).

%-----------------------------------------------------------------------------
% vizinha_baixo(Mult, Ilhas, Ilha, Vizinha):
% Vizinha e' uma lista com a vizinha de baixo
% de Ilha (lista vazia se nao existir), quando Mult e' 1.
% Se Mult for -1, Vizinha e' a lista com a vizinha de cima
% de Ilha. Predicado auxiliar que ajuda com a clareza e organizacao do codigo
% e reduz substancialmente a quantidade de codigo necessario.
%-----------------------------------------------------------------------------
vizinha_baixo(Mult, Ilhas, Ilha, Vizinhas) :-
    include(ilhas_x(Ilha), Ilhas, Ilhas_N),
    y_max(Ilhas_N, Y_MAX),
    vizinha_baixo(Ilhas_N, Ilha, Vizinhas, [], 0, Y_MAX, Mult).
vizinha_baixo(_, _, [VizinhaYFinal], [VizinhaYFinal], _, _, _).
vizinha_baixo(_, Ilha, VizinhaYFinal, VizinhaYFinal, Y_Sum, Y_MAX, 1) :-
    Ilha =.. [_,_,(Y,_)],
    Y + Y_Sum >= Y_MAX.
vizinha_baixo(_, Ilha, VizinhaYFinal, VizinhaYFinal, Y_Sum, _, -1) :-
    Ilha =.. [_,_,(Y,_)],
    Y - Y_Sum =< 0.
vizinha_baixo(Ilhas, Ilha, VizinhaY, [], Y_Sum, Y_MAX, Mult) :-
    Y_Sum_N is Y_Sum + 1,
    Ilha =.. [_,_,(Y,_)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(Y1,_)], Y1 =:= +(Y,Y_Sum_N*Mult)), VizinhaYAux),
    vizinha_baixo(Ilhas, Ilha, VizinhaY, VizinhaYAux, Y_Sum_N, Y_MAX, Mult).

%-----------------------------------------------------------------------------
% vizinha_direita(Mult, Ilhas, Ilha, Vizinha):
% Vizinha e' uma lista com a vizinha direita
% de Ilha (lista vazia se nao existir), quando Mult e' 1.
% Se Mult for -1, Vizinha e' a lista com a vizinha esquerda
% de Ilha. Predicado auxiliar que ajuda com a clareza e organizacao do codigo
% e reduz substancialmente a quantidade de codigo necessario.
%-----------------------------------------------------------------------------
vizinha_direita(Mult, Ilhas, Ilha, Vizinhas) :-
    include(ilhas_y(Ilha), Ilhas, Ilhas_N),
    x_max(Ilhas_N, X_MAX),
    vizinha_direita(Ilhas_N, Ilha, Vizinhas, [], 0, X_MAX, Mult).
vizinha_direita(_, _, [VizinhaXFinal], [VizinhaXFinal], _, _, _).
vizinha_direita(_, Ilha, VizinhaXFinal, VizinhaXFinal, X_Sum, X_MAX, 1) :-
    Ilha =.. [_,_,(_,X)],
    X + X_Sum >= X_MAX.
vizinha_direita(_, Ilha, VizinhaXFinal, VizinhaXFinal, X_Sum, _, -1) :-
    Ilha =.. [_,_,(_,X)],
    X - X_Sum =< 0.
vizinha_direita(Ilhas, Ilha, VizinhaX, [], X_Sum, X_MAX, Mult) :-
    X_Sum_N is X_Sum + 1,
    Ilha =.. [_,_,(_,X)],
    findall(Ilha1, (member(Ilha1, Ilhas), Ilha1 =.. [_,_,(_,X1)], X1 =:= +(X,X_Sum_N*Mult)), VizinhaXAux),
    vizinha_direita(Ilhas, Ilha, VizinhaX, VizinhaXAux, X_Sum_N, X_MAX, Mult).

%-----------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas):
% Vizinhas e' a lista das ilhas vizinhas de Ilha.
%-----------------------------------------------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas) :-
    vizinha_direita(1, Ilhas, Ilha, VizinhaD),
    vizinha_direita(-1, Ilhas, Ilha, VizinhaE),
    vizinha_baixo(1, Ilhas, Ilha, VizinhaB),
    vizinha_baixo(-1, Ilhas, Ilha, VizinhaC),
    append(VizinhaC, VizinhaE, VizinhasAux),
    append(VizinhasAux, VizinhaD, VizinhasAux_N),
    append(VizinhasAux_N, VizinhaB, Vizinhas).

%-----------------------------------------------------------------------------
% estado(Ilhas, Estado):
% Estado e' a lista das entradas correspondentes
% a cada uma das ilhas em Ilhas. Uma entrada tem
% a forma [Ilha, Vizinhas, Pontes].
%-----------------------------------------------------------------------------
estado(Ilhas, Estado) :- estado(Ilhas, Estado, Ilhas).
estado([], [],_).
estado([Ilha|R], [Estado|R1], Ilhas) :-
    vizinhas(Ilhas, Ilha, Vizinhas),
    Estado = [Ilha, Vizinhas, []],
    estado(R, R1, Ilhas).

%-----------------------------------------------------------------------------
% ordena([Key1, Key2], Lista, Lista_N):
% Lista_N e' o resultado de ordenar as posicoes de Lista
% de acordo com a ordem de leitura do puzzle.
% Predicado auxiliar que ajuda com a clareza e organizacao
% do codigo, reduzindo tambem a quantidade de codigo repetido.
%-----------------------------------------------------------------------------
ordena([Key1, Key2], Lista, Lista_N) :-
    sort(Key1, @=<, Lista, Lista_N_Aux),
    sort(Key2, @=<, Lista_N_Aux, Lista_N).

%-----------------------------------------------------------------------------
% posicoes_entre_x(Pos1, Pos2, Posicoes):
% Sendo Pos1 e Pos2 duas posicoes com componentes Y iguais,
% Posicoes e' a lista de posicoes entre as duas. Predicado auxiliar
% que ajuda com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
posicoes_entre_x(Pos1, Pos2, Posicoes) :- posicoes_entre_x(Pos1, Pos2, Posicoes, 0).
posicoes_entre_x((Y, X1), (Y, X2), [], Pos_Sum) :-  X1 + Pos_Sum + 1 =:= X2, !.
posicoes_entre_x((Y, X1), (Y, X2), [P|R], Pos_Sum) :-
    Pos_Sum_N is Pos_Sum + 1,
    X_N is X1 + Pos_Sum_N,
    P = (Y, X_N),
    posicoes_entre_x((Y, X1), (Y, X2), R, Pos_Sum_N).

%-----------------------------------------------------------------------------
% posicoes_entre_y(Pos1, Pos2, Posicoes):
% Sendo Pos1 e Pos2 duas posicoes com componentes X iguais,
% Posicoes e' a lista de posicoes entre as duas. Predicado auxiliar
% que ajuda com a clareza e organizacao do codigo.
%-----------------------------------------------------------------------------
posicoes_entre_y(Pos1, Pos2, Posicoes) :- posicoes_entre_y(Pos1, Pos2, Posicoes, 0).
posicoes_entre_y((Y1, X), (Y2, X), [], Pos_Sum) :-  Y1 + Pos_Sum + 1 =:= Y2, !.
posicoes_entre_y((Y1, X), (Y2, X), [P|R], Pos_Sum) :-
    Pos_Sum_N is Pos_Sum + 1,
    Y_N is Y1 + Pos_Sum_N,
    P = (Y_N, X),
    posicoes_entre_y((Y1, X), (Y2, X), R, Pos_Sum_N).

%-----------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes):
% Sendo Pos1 e Pos2 duas posicoes na forma (Y, X),
% Posicoes e' a lista de posicoes entre as duas.
%-----------------------------------------------------------------------------
posicoes_entre((Y1, X), (Y2, X), Posicoes) :-
    ordena([2, 1], [(Y1, X), (Y2, X)], [Pos1, Pos2]),
    posicoes_entre_y(Pos1, Pos2, Posicoes).
posicoes_entre((Y, X1), (Y, X2), Posicoes) :-
    ordena([2, 1], [(Y, X1), (Y, X2)], [Pos1, Pos2]),
    posicoes_entre_x(Pos1, Pos2, Posicoes).

%-----------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte):
% Ponte e' uma estrutura da forma ponte(Pos1, Pos2),
% em que Pos1 vem primeiro que Pos2 em termos de ordem
% de leitura do puzzle.
%-----------------------------------------------------------------------------
cria_ponte(Pos1, Pos2, ponte(Pos1_N, Pos2_N)) :-
    ordena([2, 1], [Pos1, Pos2], [Pos1_N, Pos2_N]). 

%-----------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, Ilha1, Ilha2):
% Sendo Pos1 e Pos2 duas posicoes na forma (Y, X),
% Posicoes e' a lista de posicoes entre as duas.
% Ilha1 e Ilha2 sao estruturas da forma ilha(P,(Y,X)).
% Este predicado averigua se Ilha1 continua vizinha de Ilha2
% quando se constroi uma ponte de Pos1 a Pos2.
%-----------------------------------------------------------------------------
caminho_livre((Y1,X1), (Y2,X2), _, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :- !.
caminho_livre((Y2,X2), (Y1,X1), _, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :- !.
caminho_livre(_, _, Posicoes, ilha(_,(Y1,X1)), ilha(_,(Y2,X2))) :-
    posicoes_entre((Y1, X1), (Y2, X2), Caminho),
    findall(El1, (member(El1, Posicoes), member(El2, Caminho), El1 = El2), []).

%-----------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Entrada_N):
% Sendo Pos1 e Pos2 duas posicoes na forma (Y, X),
% Posicoes e' a lista de posicoes entre as duas.
% Entrada e Entrada_N sao estruturas da forma [Ilha, Vizinhas, Pontes].
% Entrada_N e' o resultado de atualizar as vizinhas de Entrada em funcao
% de uma ponte construida entre Pos1 e Pos2.
%-----------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas_N, Pontes]) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha)), Vizinhas_N).

%-----------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Estado, Pos1, Pos2, Novo_Estado):
% Novo_Estado e' o resultado de atualizar as vizinhas de todas as
% entradas de Estado apos a adicao de uma ponte entre Pos1 e Pos2.
%-----------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_Estado).

%-----------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_Term):
% Ilhas_Term e' a lista de ilhas terminadas
% de Estado, isto e', a lista de ilhas que
% ja tem o numero maximo de pontes construidas.
%-----------------------------------------------------------------------------
ilhas_terminadas(Estado, Ilhas_Term) :-
    findall(Ilha, (member(El, Estado), El = [Ilha, _, Pontes], Ilha = ilha(P,_), P \= 'X', length(Pontes, P)), Ilhas_Term).

%-----------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Entrada_N):
% Entrada e Entrada_N sao estruturas da forma [Ilha, Vizinhas, Pontes].
% Entrada_N e' o resultado de atualizar as vizinhas de Entrada em funcao
% das ilhas que ja estao terminadas.
%-----------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas_N, Pontes]) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), member(Terminada, Ilhas_Term), Vizinha = Terminada), Vizinhas_Term),
    subtract(Vizinhas, Vizinhas_Term, Vizinhas_N).

%-----------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):
% Novo_Estado e' o resultado de atualizar as vizinhas de todas as
% entradas de Estado em funcao das ilhas terminadas.
%-----------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

%-----------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Entrada_N):
% Entrada e Entrada_N sao estruturas da forma [Ilha, Vizinhas, Pontes].
% Entrada_N e' o resultado de marcar Ilha em funcao do facto de esta
% estar terminada ou nao.
%-----------------------------------------------------------------------------
marca_ilhas_terminadas_entrada(Ilhas_Term, [ilha(P, (Y,X)), Vizinhas, Pontes], [ilha('X', (Y,X)), Vizinhas, Pontes]) :-
    member(ilha(P, (Y,X)), Ilhas_Term).
marca_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Vizinhas, Pontes], [Ilha, Vizinhas, Pontes]) :-
    \+ member(Ilha, Ilhas_Term).

%-----------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):
% Novo_Estado e' o resultado de marcar cada Ilha que esteja
% terminada das entradas de Estado.
%-----------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

%-----------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_Estado):
% Novo_Estado e' o resultado de atualizar as entradas
% de Estado em funcao das ilhas terminadas, ou seja,
% tirando-as das listas de vizinhas em que constam e
% marcando-as.
%-----------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_Estado) :-
    ilhas_terminadas(Estado, Ilhas_Term),
    tira_ilhas_terminadas(Estado, Ilhas_Term, EstadoAux),
    marca_ilhas_terminadas(EstadoAux, Ilhas_Term, Novo_Estado).

%-----------------------------------------------------------------------------
% junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_Estado):
% Novo_Estado e' o resultado de atualizar as entradas
% de Estado em funcao das pontes adicionadas entre Ilha1 e
% Ilha2.
%-----------------------------------------------------------------------------
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
    ordena([[1,2,2], [1,2,1]], Novo_Estado_Aux_Nao_Ordenado, Novo_Estado_Aux),
    trata_ilhas_terminadas(Novo_Estado_Aux, Novo_Estado_Aux_N),
    actualiza_vizinhas_apos_pontes(Novo_Estado_Aux_N, (Y1, X1), (Y2, X2), Novo_Estado).


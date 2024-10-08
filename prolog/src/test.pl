:- dynamic usuario/6.

% Definição de alguns usuários
cadastrar_usuarios :-
    assert(usuario('user1', 'Alice', 'alice@example.com', 'senha123', 1, 'empresa1')),
    assert(usuario('user2', 'Bob', 'bob@example.com', 'senha123', 2, 'empresa2')),
    assert(usuario('user3', 'Charlie', 'charlie@example.com', 'senha123', 3, 'empresa3')).

% Função para verificar o papel de um usuário
usuario_papel(Usuario, Papel) :- 
    usuario(Usuario, _, _, _, PapelNum, _),  
    format('Usuário: ~w, Papel numérico: ~w~n', [Usuario, PapelNum]),
    Papel = PapelNum.

% Testando o papel de usuários
testar_papeis :-
    cadastrar_usuarios,
    (   usuario_papel('user1', Papel1) -> format('User1 tem papel: ~w~n', [Papel1]) ; writeln('User1 não encontrado.') ),
    (   usuario_papel('user2', Papel2) -> format('User2 tem papel: ~w~n', [Papel2]) ; writeln('User2 não encontrado.') ),
    (   usuario_papel('user3', Papel3) -> format('User3 tem papel: ~w~n', [Papel3]) ; writeln('User3 não encontrado.') ).

% Início do programa
:- initialization(testar_papeis).

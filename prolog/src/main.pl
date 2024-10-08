:- module(main, [main/0]).

:- use_module(usuario).
:- use_module(tarefa).
:- use_module(sprint).
:- use_module(tela_inicial).

% Menu principal
menu_principal(UsuarioLogado) :-
    (   UsuarioLogado = none ->
        writeln('\nMenu Principal:'),
        writeln('1. Cadastrar Usuário'),
        writeln('2. Login'),
        writeln('3. Sair'),
        writeln('Escolha uma opção: '),
        read(Escolha),
        (   Escolha = 1 -> 
            cadastrar_usuario(),
            menu_principal(none)
        ;   Escolha = 2 -> 
            login(UsuarioLogado),
            (   UsuarioLogado \= none -> 
                format('Logado como: ~w~n', [UsuarioLogado]),
                menu_principal(UsuarioLogado)  % Passa apenas o usuário logado
            ;   writeln('Login falhou. Tente novamente.'),
                menu_principal(none)
            )
        ;   Escolha = 3 -> 
            writeln('Saindo...')
        ;   
            writeln('Opção inválida, tente novamente.'),
            menu_principal(none)
        )
    ;   % Se o usuário está logado
        (   usuario_papel(UsuarioLogado, product_owner) ; usuario_papel(UsuarioLogado, scrum_master) ->
            menu_administrador(UsuarioLogado)
        ;   menu_comum(UsuarioLogado)
        ),
        menu_principal(UsuarioLogado)  % Retorna ao menu principal
    ).

% Predicado principal
main :- 
    writeln('Bem-vindo ao Sistema de Gerenciamento Scrum'),
    menu_principal(none).  % Inicia com nenhum usuário logado

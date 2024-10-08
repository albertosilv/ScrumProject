:- use_module(library(readutil)).
:- use_module(usuario).
:- use_module(tela_inicial).


% Menu principal
menu_principal(UsuarioLogado) :-
    (   UsuarioLogado = none ->
        writeln('\nMenu Principal:'),
        writeln('1. Cadastrar Usuário'),
        writeln('2. Login'),
        writeln('3. Sair'),
        writeln('Escolha uma opção: '),
        read_line_to_string(user_input, OpcaoStr),
        processar_opcao(OpcaoStr, none)  % Passa none para processar_opcao
    ;   % Se o usuário está logado
        (   usuario_papel(UsuarioLogado, 1) ->
            menu_administrador(UsuarioLogado)  % Acesso ao menu de administrador
        ;   usuario_papel(UsuarioLogado, 2) ->
            menu_administrador(UsuarioLogado)  % Acesso ao menu de administrador
        ;   menu_comum(UsuarioLogado)  % Acesso ao menu comum
        ),
        writeln('Você deseja sair? (s/n)'),
        read_line_to_string(user_input, Resposta),
        (   Resposta = "s" -> 
            menu_principal(none)  % Retorna ao menu principal com UsuarioLogado como none
        ;   menu_principal(UsuarioLogado)  % Retorna ao menu principal com UsuarioLogado ainda ativo
        )
    ).

% Processa as opções do menu principal
processar_opcao("1", _) :- 
    cadastrar_usuario(), 
    menu_principal(none).

processar_opcao("2", _) :- 
    login(NovoUsuarioLogado),
    (   NovoUsuarioLogado \= none -> 
        format('Logado como: ~w~n', [NovoUsuarioLogado]),
        menu_principal(NovoUsuarioLogado)
    ;   writeln('Login falhou. Tente novamente.'),
        menu_principal(none)
    ).

processar_opcao("3", _) :- 
    writeln('Saindo...'), 
    halt.

processar_opcao(_, UsuarioLogado) :- 
    writeln('Opção inválida, tente novamente.'),
    menu_principal(UsuarioLogado).
% Início do programa
:- initialization(menu_principal(none)).

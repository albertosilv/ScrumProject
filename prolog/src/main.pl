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
        writeln('Escolha uma opção:'), 
        read_line_to_string(user_input, OpcaoStr), 
        processar_opcao(OpcaoStr, none)
    ;   ( usuario_papel(UsuarioLogado, Papel),writeln('Papel encontrado: ~w', [Papel]),  % Mostra o papel encontrado
            (   (Papel = 1 ; Papel = 2),  % 1 para Product Owner ou 2 para Scrum Master
                menu_administrador(UsuarioLogado)
            ;   menu_comum(UsuarioLogado)
            )
        ;   
            menu_principal(UsuarioLogado)
        ),
        writeln('Você deseja sair? (s/n)'),
        read_line_to_string(user_input, Resposta),
        (   Resposta = "s" -> 
            menu_principal(none)
        ;   menu_principal(UsuarioLogado)
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

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
    ).

% Processa as opções do menu principal
processar_opcao("1", _) :- 
    cadastrar_usuario(), 
    menu_principal(none). 

processar_opcao("2", _) :- 
    login(NovoUsuarioLogado, PapelNum),
    (   NovoUsuarioLogado \= none -> 
        (   PapelNum = 1 -> 
            menu_administrador(NovoUsuarioLogado),
            menu_principal(none)
        ;
            menu_comum(NovoUsuarioLogado),
            menu_principal(none) 
        )
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

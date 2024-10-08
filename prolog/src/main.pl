:- use_module(library(readutil)).
:- use_module(usuario).
:- use_module(tela_inicial).

% Menu principal
menu_principal(UsuarioLogado) :-
  (   UsuarioLogado = none -> 
    writeln('\nMenu Principal:'), 
    writeln('1. Cadastrar Usuario'), 
    writeln('2. Login'), 
    writeln('3. Sair'), 
    writeln('Escolha uma opcao:'), 
    read_line_to_string(user_input, OpcaoStr), 
    processar_opcao(OpcaoStr, none)
  ).

% Processa as opcoes do menu principal
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
  writeln('Opcao invalida, tente novamente.'),
  menu_principal(UsuarioLogado).

% Inicio do programa
:- initialization(menu_principal(none)).

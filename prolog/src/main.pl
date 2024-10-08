:- use_module(library(readutil)).
:- use_module(usuario).
:- use_module(tela_inicial).

% Menu principal
menu_principal :-
    writeln('\nMenu Principal:'), 
    writeln('1. Cadastrar Usuario'), 
    writeln('2. Login'), 
    writeln('3. Sair'), 
    writeln('Escolha uma opcao:'), 
    read_line_to_string(user_input, OpcaoStr), 
    processar_opcao(OpcaoStr, none)
  .


processar_opcao("1", _) :- 
  cadastrar_usuario(), 
  menu_principal.

processar_opcao("2", _) :- 
  login(NovoUsuarioLogado, PapelNum),
  (   NovoUsuarioLogado \= none -> 
    (   PapelNum = 1 -> 
      menu_administrador(NovoUsuarioLogado),
      menu_principal
    ;
      menu_comum(NovoUsuarioLogado),
      menu_principal
    )
  ;   writeln('Login falhou. Tente novamente.'),
    menu_principal
  ).

processar_opcao("3", _) :- 
  writeln('Saindo...'), 
  halt.

processar_opcao(_, _) :- 
  writeln('Opcao invalida, tente novamente.'),
  menu_principal.


:- initialization(menu_principal).

:- module(tela_inicial, [menu_administrador/1, menu_comum/1]).

:- use_module(sprint).
:- use_module(tarefa).
:- use_module(usuario).

menu_administrador(Usuario) :-
  writeln('\nMenu Administrador:'),
  writeln('1. Backlog'),
  writeln('2. Sprint'),
  writeln('3. Dados do Usuario'),
  writeln('4. Logout'),
  read_line_to_string(user_input, EscolhaString),
  atom_number(EscolhaString, Escolha),  % Converte a string para numero
  (   Escolha = 1 -> 
    listar_tarefas_backlog(Usuario),
    menu_administrador(Usuario)
  ;   Escolha = 2 -> 
    listar_sprints_da_empresa(Usuario),
    menu_administrador(Usuario)
  ;   Escolha = 3 -> 
    modificar_usuario(Usuario, UsuarioAtualizado),
    menu_administrador(UsuarioAtualizado)
  ;   Escolha = 4 -> 
    null
  ;   
    writeln('Opcao invalida, tente novamente.'),
    menu_administrador(Usuario)
  ).

menu_comum(Usuario) :-
  writeln('\nMenu Usuario Comum:'),
  writeln('1. Sprints'),
  writeln('2. Tarefas'),
  writeln('3. Dados do Usuario'),
  writeln('4. Logout'),
  read_line_to_string(user_input, EscolhaString),
  atom_number(EscolhaString, Escolha),  % Converte a string para numero
  (   Escolha = 1 -> 
    listar_sprints_da_empresa(Usuario),
    menu_comum(Usuario)
  ;   Escolha = 2 -> 
    listar_tarefas_usuario(Usuario),
    menu_comum(Usuario)
  ;   Escolha = 3 -> 
    modificar_usuario(Usuario, UsuarioAtualizado),
    menu_comum(UsuarioAtualizado)
  ;   Escolha = 4 -> 
    writeln('Saindo...'), 
    true  
  ;   
    writeln('Opcao invalida, tente novamente.'),
    menu_comum(Usuario)  % Repete o menu em caso de erro
  ).


update_usuario(Usuario, UsuarioAtualizado) :-
  findall(U, usuario(U), Usuarios),  % Obtem a lista de todos os usuarios
  maplist(replace_usuario(Usuario, UsuarioAtualizado), Usuarios, UsuariosAtualizados),
  retractall(usuario(_)),  % Remove todos os usuarios antigos
  maplist(assertz, UsuariosAtualizados).  % Adiciona a lista atualizada de usuarios


replace_usuario(Usuario, UsuarioAtualizado, Usuario, UsuarioAtualizado) :- !.  
replace_usuario(_, _, U, U). 

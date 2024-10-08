:- module(tela_inicial, [menu_administrador/1, menu_comum/1]).

:- use_module(sprint).
:- use_module(tarefa).
:- use_module(usuario).

% Menu para usuários administradores
menu_administrador(Usuario) :-
    writeln('\nMenu Administrador:'),
    writeln('1. Backlog'),
    writeln('2. Sprint'),
    writeln('3. Dados do Usuário'),
    writeln('4. Logout'),
    read(Escolha),
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
        true
    ;   
        writeln('Opção inválida, tente novamente.'),
        menu_administrador(Usuario)
    ).

% Menu para usuários comuns
menu_comum(Usuario) :-
    writeln('\nMenu Usuário Comum:'),
    writeln('1. Sprints'),
    writeln('2. Tarefas'),
    writeln('3. Dados do Usuário'),
    writeln('4. Logout'),
    read(Escolha),
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
        true
    ;   
        writeln('Opção inválida, tente novamente.'),
        menu_comum(Usuario)
    ).

% Atualiza os dados do usuário
update_usuario(Usuario, UsuarioAtualizado) :-
    findall(U, usuario(U), Usuarios),  % Obtém a lista de todos os usuários
    maplist(replace_usuario(Usuario, UsuarioAtualizado), Usuarios, UsuariosAtualizados).

% Substitui um usuário na lista
replace_usuario(Usuario, UsuarioAtualizado, Usuario, UsuarioAtualizado) :- 
    usuario_id(UsuarioAtualizado, Id),
    usuario_id(Usuario, Id), !.
replace_usuario(_, Usuario, Usuario, Usuario).

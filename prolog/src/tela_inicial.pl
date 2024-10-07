:- module(tela_inicial, [menu_administrador/4, menu_comum/4]).

:- use_module(sprint).
:- use_module(tarefa).
:- use_module(usuario).

menu_administrador(Usuario, Usuarios, Tarefas, Sprints) :-
    writeln('\nMenu Administrador:'),
    writeln('1. Backlog'),
    writeln('2. Sprint'),
    writeln('3. Dados do Usuario'),
    writeln('4. Logout'),
    read(Escolha),
    (   Escolha = 1 -> 
        backlog_empresa(Usuario, Tarefas, TarefasAtualizado),
        menu_administrador(Usuario, Usuarios, TarefasAtualizado, Sprints)
    ;   Escolha = 2 -> 
        listar_sprints_da_empresa(Usuario, Sprints, Usuarios, Tarefas, SprintsAtualizada, TarefasAtualizado),
        menu_administrador(Usuario, Usuarios, TarefasAtualizado, SprintsAtualizada)
    ;   Escolha = 3 -> 
        modificar_usuario(Usuario, UsuarioAtualizado),
        update_usuario(Usuario, UsuarioAtualizado, Usuarios, UsuariosAtualizados),
        menu_administrador(UsuarioAtualizado, UsuariosAtualizados, Tarefas, Sprints)
    ;   Escolha = 4 -> 
        true
    ;   
        writeln('Opção inválida, tente novamente.'),
        menu_administrador(Usuario, Usuarios, Tarefas, Sprints)
    ).

menu_comum(Usuario, Usuarios, Tarefas, Sprints) :-
    writeln('\nMenu Usuário Comum:'),
    writeln('1. Sprints'),
    writeln('2. Tarefas'),
    writeln('3. Dados do Usuario'),
    writeln('4. Logout'),
    read(Escolha),
    (   Escolha = 1 -> 
        listar_sprint(Usuario)
    ;   Escolha = 2 -> 
        listar_tarefas_usuario(Usuario, Tarefas, TarefasAtualizadas),
        menu_comum(Usuario, Usuarios, TarefasAtualizadas, Sprints)
    ;   Escolha = 3 -> 
        modificar_usuario(Usuario, UsuarioAtualizado),
        update_usuario(Usuario, UsuarioAtualizado, Usuarios, UsuariosAtualizados),
        menu_comum(UsuarioAtualizado, UsuariosAtualizados, Tarefas, Sprints)
    ;   Escolha = 4 -> 
        true
    ;   
        writeln('Opção inválida, tente novamente.'),
        menu_comum(Usuario, Usuarios, Tarefas, Sprints)
    ).

update_usuario(Usuario, UsuarioAtualizado, Usuarios, UsuariosAtualizados) :-
    maplist(replace_usuario(Usuario, UsuarioAtualizado), Usuarios, UsuariosAtualizados).

replace_usuario(Usuario, UsuarioAtualizado, Usuario, UsuarioAtualizado) :- 
    usuario_id(UsuarioAtualizado, Id),
    usuario_id(Usuario, Id), !.
replace_usuario(_, Usuario, Usuario, Usuario).

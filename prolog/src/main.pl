:- module(main, [main/0]).

:- use_module(usuario).
:- use_module(tarefa).
:- use_module(sprint).
:- use_module(tela_inicial).

% Menu principal
menu_principal(Usuarios, Tarefas, Sprints, UsuarioLogado) :-
    (   UsuarioLogado = none ->
        writeln('\nMenu Principal:'),
        writeln('1. Cadastrar Usuário'),
        writeln('2. Login'),
        writeln('3. Sair'),
        writeln('Escolha uma opção: '),
        read(Escolha),
        (   Escolha = 1 -> 
            cadastrar_usuario(Usuario, UsuariosAtualizados),
            menu_principal(UsuariosAtualizados, Tarefas, Sprints, none)
        ;   Escolha = 2 -> 
            login(Usuarios, UsuarioLogado),
            (   UsuarioLogado \= none -> 
                usuario_empresa_id(UsuarioLogado, EmpresaId),
                findall(Tarefa, tarefa_empresa_id(Tarefa, EmpresaId), TarefasFiltradas),
                findall(Sprint, sprint_empresa_id(Sprint, EmpresaId), SprintsFiltrados),
                format('Logado como: ~w~n', [UsuarioLogado]),
                menu_principal(Usuarios, TarefasFiltradas, SprintsFiltrados, UsuarioLogado)
            ;   writeln('Login falhou. Tente novamente.'),
                menu_principal(Usuarios, Tarefas, Sprints, none)
            )
        ;   Escolha = 3 -> 
            writeln('Saindo...')
        ;   
            writeln('Opção inválida, tente novamente.'),
            menu_principal(Usuarios, Tarefas, Sprints, none)
        )
    ;   % Se usuário está logado
        (   usuario_papel(UsuarioLogado, product_owner) ; usuario_papel(UsuarioLogado, scrum_master) ->
            menu_administrador(UsuarioLogado, Usuarios, Tarefas, Sprints, UsuariosAtualizados, TarefasAtualizadas, SprintsAtualizados),
            menu_principal(UsuariosAtualizados, TarefasAtualizadas, SprintsAtualizados, none)
        ;   menu_comum(UsuarioLogado, Usuarios, Tarefas, Sprints, UsuariosAtualizados, TarefasAtualizadas, SprintsAtualizados),
            menu_principal(UsuariosAtualizados, TarefasAtualizadas, SprintsAtualizados, none)
        )
    ).

% Predicado principal
main :- 
    writeln('Bem-vindo ao Sistema de Gerenciamento Scrum'),
    menu_principal([], [], [], none).

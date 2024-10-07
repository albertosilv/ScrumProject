:- module(sprint, [criar_sprint/1, listar_sprint/1, listar_sprints_da_empresa/4, acessar_sprint/5, adicionar_tarefa_a_sprint/4, atribuir_tarefa/3]).

:- use_module(usuario).
:- use_module(tarefa).

:- dynamic sprint/6.

criar_sprint(Usuario) :-
    write('Digite o ID da Sprint:'), nl,
    read(Id),
    write('Digite o Nome da Sprint:'), nl,
    read(Nome),
    write('Digite a Duração da Sprint (em dias):'), nl,
    read(Duracao),
    usuario:usuarioEmpresaId(Usuario, EmpresaId),
    assertz(sprint(Id, Nome, Duracao, [], usuarioId(Usuario), EmpresaId)),
    format('Sprint criada: ~w~n', [Id]).

listar_sprint(Usuario) :-
    findall(Sprint, sprint(_, Sprint, _, _, usuarioId(Usuario), _), SprintsCriadas),
    findall(Tarefa, (tarefa:Tarefa = Tarefa, (tarefaIdCriador(Tarefa, usuarioId(Usuario)); tarefaIdResponsavel(Tarefa, usuarioId(Usuario)))), TarefasUsuario),
    findall(IdSprint, (member(Tarefa, TarefasUsuario), tarefaId(Tarefa, IdSprint)), IdsSprintsUsuario),
    findall(Sprint, (member(Sprint, SprintsCriadas), member(_, IdsSprintsUsuario), member(_, sprintTarefas(Sprint))), SprintsEnvolvido),
    append(SprintsCriadas, SprintsEnvolvido, TodasSprints),
    remove_duplicates(TodasSprints, SprintsUnicas),
    (SprintsUnicas == [] -> write('O usuário não possui sprints.'); maplist(print_sprint, SprintsUnicas)).

print_sprint(Sprint) :-
    sprintId(Sprint, Id),
    sprintNome(Sprint, Nome),
    format('ID: ~w, Nome: ~w~n', [Id, Nome]).

listar_sprints_da_empresa(Usuario, Sprints, Usuarios, Tarefas) :-
    findall(Sprint, sprint(_, Sprint, _, _, _, usuarioEmpresaId(Usuario)), SprintsDaEmpresa),
    write('Sprints da Empresa:'), nl,
    maplist(print_sprint, SprintsDaEmpresa),
    write('Digite o ID da sprint para visualizar suas tarefas (ou -1 para voltar, 0 para criar uma nova sprint):'), nl,
    read(Entrada),
    (Entrada = -1 -> true;
     Entrada = 0 -> criar_sprint(Usuario), listar_sprints_da_empresa(Usuario, Sprints, Usuarios, Tarefas);
     findall(Sprint, sprint(Entrada, Sprint, _, _, _, _), [SprintEscolhida | _]),
     (SprintEscolhida == [] -> write('Sprint não encontrada.'), nl, listar_sprints_da_empresa(Usuario, Sprints, Usuarios, Tarefas);
      acessar_sprint(Usuario, SprintEscolhida, Sprints, Tarefas, Usuarios))).

acessar_sprint(Usuario, Sprint, Sprints, Tarefas, Usuarios) :-
    sprintNome(Sprint, Nome),
    format('Sprint Selecionada: ~w~n', [Nome]),
    write('Tarefas da Sprint:'), nl,
    findall(Tarefa, (tarefa:Tarefa = Tarefa, tarefaId(Tarefa, TarefaId), member(TarefaId, sprintTarefas(Sprint))), TarefasDaSprint),
    maplist(print_tarefa, TarefasDaSprint),
    write('Escolha uma opção:'), nl,
    write('1. Adicionar Tarefa à Sprint'), nl,
    write('2. Atribuir Tarefa a um Usuário'), nl,
    write('0. Voltar'), nl,
    read(Escolha),
    (Escolha = 1 -> adicionar_tarefa_a_sprint(Usuario, Sprint, Sprints, Tarefas);
     Escolha = 2 -> atribuir_tarefa(Usuario, Tarefas, Usuarios);
     Escolha = 0 -> true;
     write('Opção inválida, tente novamente.'), nl, acessar_sprint(Usuario, Sprint, Sprints, Tarefas, Usuarios)).

adicionar_tarefa_a_sprint(Usuario, Sprint, Sprints, Tarefas) :-
    write('Digite o ID da tarefa para adicionar à sprint:'), nl,
    read(TarefaIdEscolhida),
    (findall(Tarefa, (tarefa:Tarefa = Tarefa, tarefaId(Tarefa, TarefaIdEscolhida), tarefaStatus(Tarefa, backlog)), [Tarefa | _]) ->
        retract(sprint(SprintId, SprintNome, SprintDuracao, SprintTarefas, CriadorId, EmpresaId)),
        assertz(sprint(SprintId, SprintNome, SprintDuracao, [TarefaIdEscolhida | SprintTarefas], CriadorId, EmpresaId)),
        write('Tarefa adicionada à sprint e status atualizado com sucesso!'), nl,
        acessar_sprint(Usuario, Sprint, Sprints, Tarefas, Usuarios);
        write('Tarefa não encontrada ou não está no status "Backlog".'), nl,
        acessar_sprint(Usuario, Sprint, Sprints, Tarefas, Usuarios)).

atribuir_tarefa(Usuario, Tarefas, Usuarios) :-
    write('Digite o ID da tarefa para atribuir a um usuário:'), nl,
    read(TarefaIdEscolhida),
    (findall(Tarefa, (tarefa:Tarefa = Tarefa, tarefaId(Tarefa, TarefaIdEscolhida), tarefaStatus(Tarefa, pendente)), [Tarefa | _]) ->
        write('Digite o ID do usuário para atribuir a tarefa:'), nl,
        read(UsuarioIdEscolhido),
        (findall(UsuarioAtribuido, (usuario:UsuarioAtribuido = Usuario, usuarioId(UsuarioAtribuido, UsuarioIdEscolhido), usuarioEmpresaId(UsuarioAtribuido, EmpresaId), tarefa:Tarefa, tarefaEmpresaId(Tarefa, EmpresaId), usuarioPapel(UsuarioAtribuido, devteam)), [UsuarioAtribuido | _]) ->
            retract(tarefa:Tarefa),
            assertz(tarefa(TarefaIdEscolhida, Titulo, Descricao, Prioridade, Pendente, UsuarioIdEscolhido)),
            write('Tarefa atribuída com sucesso!'), nl;
            write('Usuário não encontrado, não pertence à empresa ou não é um desenvolvedor.'), nl);
        write('Tarefa não encontrada ou não está no status "Pendente".'), nl).

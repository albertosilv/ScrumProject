% Definindo fatos e predicados
% Sprint é representada por: sprint(ID, Nome, Duracao, Tarefas, IdCriador, EmpresaId)
% Tarefa é representada por: tarefa(ID, Titulo, Status, IdCriador, IdResponsavel, EmpresaId)
% Usuario é representado por: usuario(ID, Nome, TipoUsuario, EmpresaId)

% Definindo os tipos de status de tarefa
status_tarefa(backlog).
status_tarefa(pendente).

% Sprint criação
criar_sprint(Usuario, Sprint) :-
    write('Digite o ID da Sprint: '),
    read(Id),
    write('Digite o Nome da Sprint: '),
    read(Nome),
    write('Digite a Duracao da Sprint (em dias): '),
    read(Duracao),
    usuario_empresa_id(Usuario, EmpresaId), % Obtém o ID da empresa do usuário
    Sprint = sprint(Id, Nome, Duracao, [], 0, EmpresaId), % Cria a sprint com tarefas vazias
    write('Sprint criada: '), write(Sprint), nl.

% Listar sprints do usuário
listar_sprint(Usuario, Sprints, Tarefas) :-
    usuario_id(Usuario, UsuarioId),
    % Sprints criadas pelo usuário
    include(sprint_criada_por(UsuarioId), Sprints, SprintsCriadas),
    % Tarefas do usuário (criador ou responsável)
    include(tarefa_do_usuario(UsuarioId), Tarefas, TarefasUsuario),
    % IDs das sprints em que o usuário está envolvido
    maplist(tarefa_id, TarefasUsuario, IdsSprintsUsuario),
    % Sprints onde o usuário está envolvido
    include(sprint_envolvido(IdsSprintsUsuario), Sprints, SprintsEnvolvido),
    append(SprintsCriadas, SprintsEnvolvido, TodasSprints),
    remove_duplicatas(TodasSprints, SprintsUnicas),
    (   SprintsUnicas = []
    ->  write('O usuário não possui sprints.'), nl
    ;   write('Sprints do usuário:'), nl,
        maplist(exibir_sprint, SprintsUnicas)
    ).

% Sprint criada por determinado usuário
sprint_criada_por(UsuarioId, sprint(_, _, _, _, UsuarioId, _)).

% Tarefa do usuário (criador ou responsável)
tarefa_do_usuario(UsuarioId, tarefa(_, _, _, UsuarioId, _)).
tarefa_do_usuario(UsuarioId, tarefa(_, _, _, _, UsuarioId)).

% Sprint onde o usuário está envolvido
sprint_envolvido(IdsTarefasUsuario, sprint(_, _, _, SprintTarefas, _, _)) :-
    member(TarefaId, SprintTarefas),
    member(TarefaId, IdsTarefasUsuario).

% Exibir nome da sprint
exibir_sprint(sprint(_, Nome, _, _, _, _)) :- write(Nome), nl.

% Função auxiliar para remover duplicatas
remove_duplicatas(Lista, ListaUnica) :- remove_duplicatas(Lista, [], ListaUnica).
remove_duplicatas([], Acc, Acc).
remove_duplicatas([H|T], Acc, ListaUnica) :-
    (   member(H, Acc)
    ->  remove_duplicatas(T, Acc, ListaUnica)
    ;   remove_duplicatas(T, [H|Acc], ListaUnica)
    ).

% Listar sprints da empresa
listar_sprints_empresa(Usuario, Sprints, Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas) :-
    usuario_empresa_id(Usuario, EmpresaId),
    include(sprint_da_empresa(EmpresaId), Sprints, SprintsDaEmpresa),
    write('Sprints da Empresa:'), nl,
    maplist(exibir_sprint_com_id, SprintsDaEmpresa),
    write('Digite o ID da sprint para visualizar suas tarefas (ou -1 para voltar, 0 para criar uma nova sprint): '),
    read(Entrada),
    listar_opcao_sprint(Entrada, Usuario, SprintsDaEmpresa, Sprints, Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas).

% Exibir sprint com ID
exibir_sprint_com_id(sprint(Id, Nome, _, _, _, _)) :-
    write('ID: '), write(Id), write(', Nome: '), write(Nome), nl.

% Opções do menu de sprints
listar_opcao_sprint(-1, _, _, Sprints, _, Tarefas, Sprints, Tarefas).
listar_opcao_sprint(0, Usuario, _, Sprints, Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas) :-
    criar_sprint(Usuario, NovaSprint),
    listar_sprints_empresa(Usuario, [NovaSprint|Sprints], Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas).
listar_opcao_sprint(SprintIdEscolhida, Usuario, SprintsDaEmpresa, Sprints, Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas) :-
    (   member(sprint(SprintIdEscolhida, _, _, _, _, _), SprintsDaEmpresa)
    ->  acessar_sprint(Usuario, SprintIdEscolhida, Sprints, Tarefas, Usuarios, SprintsAtualizados, TarefasAtualizadas)
    ;   write('Sprint não encontrada.'), nl,
        listar_sprints_empresa(Usuario, Sprints, Usuarios, Tarefas, SprintsAtualizados, TarefasAtualizadas)
    ).

% Acessar uma sprint
acessar_sprint(Usuario, SprintId, Sprints, Tarefas, Usuarios, SprintsAtualizados, TarefasAtualizadas) :-
    % Seleciona a sprint
    member(Sprint, Sprints),
    Sprint = sprint(SprintId, Nome, _, SprintTarefas, _, _),
    write('Sprint Selecionada: '), write(Nome), nl,
    % Tarefas da sprint
    include(tarefa_da_sprint(SprintTarefas), Tarefas, TarefasDaSprint),
    write('Tarefas da Sprint:'), nl,
    maplist(exibir_tarefa, TarefasDaSprint),
    write('Escolha uma opção:\n1. Adicionar Tarefa à Sprint\n2. Atribuir Tarefa a um Usuário\n0. Voltar\n'),
    read(Escolha),
    executar_escolha_sprint(Escolha, Usuario, Sprint, Sprints, Tarefas, Usuarios, SprintsAtualizados, TarefasAtualizadas).

% Exibir tarefa
exibir_tarefa(tarefa(Id, Titulo, Status, _, _)) :-
    write('ID: '), write(Id), write(', Título: '), write(Titulo), write(', Status: '), write(Status), nl.

% Tarefa pertence à sprint
tarefa_da_sprint(SprintTarefas, tarefa(Id, _, _, _, _)) :-
    member(Id, SprintTarefas).

% Opções dentro da sprint
executar_escolha_sprint(1, Usuario, Sprint, Sprints, Tarefas, Usuarios, SprintsAtualizados, TarefasAtualizadas) :-
    adicionar_tarefa_sprint(Usuario, Sprint, Sprints, Tarefas, SprintsAtualizados, TarefasAtualizadas).
executar_escolha_sprint(2, Usuario, Sprint, Sprints, Tarefas, Usuarios, SprintsAtualizados, TarefasAtualizadas) :-
    atribuir_tarefa(Usuario, Tarefas, Usuarios, TarefasAtualizadas),
    acessar_sprint(Usuario, Sprint, Sprints, TarefasAtualizadas, Usuarios, SprintsAtualizados, TarefasAtualizadas).
executar_escolha_sprint(0, _, _, Sprints, Tarefas, _, Sprints, Tarefas).

% Adicionar tarefa à sprint
adicionar_tarefa_sprint(Usuario, Sprint, Sprints, Tarefas, SprintsAtualizados, TarefasAtualizadas) :-
    write('Digite o ID da tarefa para adicionar à sprint: '),
    read(TarefaIdEscolhida),
    (   member(tarefa(TarefaIdEscolhida, _, backlog, _, _), Tarefas)
    ->  maplist(atualizar_tarefa_status(TarefaIdEscolhida, pendente), Tarefas, TarefasAtualizadas),
        maplist(atualizar_sprint_tarefas(Sprint, TarefaIdEscolhida), Sprints, SprintsAtualizados),
        write('Tarefa adicionada à sprint e status atualizado com sucesso!'), nl
    ;   write('Tarefa não encontrada ou não está no status Backlog.'), nl,
        SprintsAtualizados = Sprints, TarefasAtualizadas = Tarefas
    ).

% Atualizar status da tarefa
atualizar_tarefa_status(Id, NovoStatus, tarefa(Id, Titulo, _, IdCriador, IdResponsavel), tarefa(Id, Titulo, NovoStatus, IdCriador, IdResponsavel)).
atualizar_tarefa_status(_, _, Tarefa, Tarefa).

% Atualizar lista de tarefas de uma sprint
atualizar_sprint_tarefas(sprint(Id, Nome, Duracao, Tarefas, Criador, EmpresaId), TarefaId, sprint(Id, Nome, Duracao, [TarefaId|Tarefas], Criador, EmpresaId)).
atualizar_sprint_tarefas(Sprint, _, Sprint).

% Atribuir tarefa a um usuário
atribuir_tarefa(Usuario, Tarefas, Usuarios, TarefasAtualizadas) :-
    write('Digite o ID da tarefa para atribuir a um usuário: '),
    read(TarefaIdEscolhida),
    (   member(tarefa(TarefaIdEscolhida, _, pendente, _, _), Tarefas)
    ->  write('Digite o ID do usuário para atribuir a tarefa: '),
        read(UsuarioIdEscolhido),
        (   member(usuario(UsuarioIdEscolhido, _, devteam, EmpresaId), Usuarios),
            tarefa_empresa_id(TarefaIdEscolhida, EmpresaId)
        ->  maplist(atualizar_responsavel_tarefa(TarefaIdEscolhida, UsuarioIdEscolhido), Tarefas, TarefasAtualizadas),
            write('Tarefa atribuída com sucesso!'), nl
        ;   write('Usuário não encontrado, não pertence à empresa ou não é um desenvolvedor.'), nl,
            TarefasAtualizadas = Tarefas
        )
    ;   write('Tarefa não encontrada ou não está no status Pendente.'), nl,
        TarefasAtualizadas = Tarefas
    ).

% Atualizar responsável da tarefa
atualizar_responsavel_tarefa(Id, NovoResponsavel, tarefa(Id, Titulo, Status, Criador, _), tarefa(Id, Titulo, Status, Criador, NovoResponsavel)).
atualizar_responsavel_tarefa(_, _, Tarefa, Tarefa).


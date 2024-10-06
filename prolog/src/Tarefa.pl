% Definição dos status da tarefa
status_tarefa(backlog).
status_tarefa(pendente).
status_tarefa(em_desenvolvimento).
status_tarefa(concluido).

% Estrutura da tarefa
tarefa(Id, Titulo, Descricao, Prioridade, Status, IdCriador, IdResponsavel, EmpresaId).

% Função para filtrar o backlog da empresa
backlog_empresa(Usuario, Tarefas, TarefasAtualizadas) :-
    findall(Tarefa, (member(Tarefa, Tarefas), Tarefa = tarefa(_, _, _, _, backlog, _, _, _)), TarefasBacklog),
    write('\nBacklog de Tarefas da Empresa:\n'),
    listar_tarefas(TarefasBacklog),
    write('\nOpções:\n1. Criar nova tarefa\n2. Voltar ao menu principal\nEscolha uma opção: '), nl,
    read(Opcao),
    (Opcao = 1 -> 
        adicionar_tarefa(Usuario, NovaTarefa),
        backlog_empresa(Usuario, [NovaTarefa | Tarefas], TarefasAtualizadas)
    ; Opcao = 2 -> 
        TarefasAtualizadas = Tarefas
    ; 
        write('Opção inválida, tente novamente.'), nl,
        backlog_empresa(Usuario, Tarefas, TarefasAtualizadas)
    ).

% Função para adicionar uma nova tarefa
adicionar_tarefa(Usuario, Tarefa) :-
    write('Digite o ID da Tarefa:'), nl,
    read(Id),
    write('Digite o Título da Tarefa:'), nl,
    read(Titulo),
    write('Digite a Descrição da Tarefa:'), nl,
    read(Descricao),
    write('Digite a Prioridade da Tarefa (1-5):'), nl,
    read(Prioridade),
    EmpresaId = Usuario.empresa_id,
    CriadorId = Usuario.usuario_id,
    Tarefa = tarefa(Id, Titulo, Descricao, Prioridade, backlog, CriadorId, 0, EmpresaId),
    write('Tarefa adicionada: '), write(Tarefa), nl.

% Função para listar tarefas de um usuário e modificar o status
listar_tarefas_usuario(Usuario, Tarefas, TarefasAtualizadas) :-
    findall(Tarefa, 
        (member(Tarefa, Tarefas), 
         Tarefa = tarefa(_, _, _, _, _, CriadorId, ResponsavelId, _), 
         (CriadorId = Usuario.usuario_id ; ResponsavelId = Usuario.usuario_id)), 
        TarefasUsuario),
    (TarefasUsuario = [] -> 
        write('O usuário não possui tarefas.'), nl, 
        TarefasAtualizadas = Tarefas
    ; 
        write('Tarefas do usuário:\n'),
        listar_tarefas(TarefasUsuario),
        write('Digite o ID da tarefa que deseja modificar o status ou 0 para sair:'), nl,
        read(TarefaIdEscolhida),
        (TarefaIdEscolhida = 0 -> 
            TarefasAtualizadas = Tarefas
        ; 
            ( member(Tarefa, TarefasUsuario),
              Tarefa = tarefa(TarefaIdEscolhida, _, _, _, _, _, _, _)) -> 
                write('Digite o novo status da tarefa (2 para Pendente, 3 para Em Desenvolvimento, 4 para Concluído):'), nl,
                read(NovoStatusEntrada),
                (NovoStatusEntrada = 2 -> NovoStatus = pendente;
                 NovoStatusEntrada = 3 -> NovoStatus = em_desenvolvimento;
                 NovoStatusEntrada = 4 -> NovoStatus = concluido;
                 NovoStatus = backlog), % Caso a entrada seja inválida, mantém o status backlog
                atualizar_status_tarefa(TarefaIdEscolhida, NovoStatus, Tarefas, TarefasAtualizadas)
            ; 
                write('Tarefa não encontrada.'), nl,
                listar_tarefas_usuario(Usuario, Tarefas, TarefasAtualizadas)
        )
    ).

% Função auxiliar para listar as tarefas
listar_tarefas([]).
listar_tarefas([tarefa(Id, Titulo, _, _, Status, _, _, _) | Tarefas]) :-
    write('ID: '), write(Id), write(', Título: '), write(Titulo), write(', Status: '), write(Status), nl,
    listar_tarefas(Tarefas).

% Função auxiliar para atualizar o status de uma tarefa
atualizar_status_tarefa(_, _, [], []).
atualizar_status_tarefa(IdTarefa, NovoStatus, [tarefa(Id, Titulo, Descricao, Prioridade, _, CriadorId, ResponsavelId, EmpresaId) | Resto], 
                        [tarefa(Id, Titulo, Descricao, Prioridade, NovoStatus, CriadorId, ResponsavelId, EmpresaId) | TarefasAtualizadas]) :-
    Id = IdTarefa,
    atualizar_status_tarefa(IdTarefa, NovoStatus, Resto, TarefasAtualizadas).
atualizar_status_tarefa(IdTarefa, NovoStatus, [OutraTarefa | Resto], [OutraTarefa | TarefasAtualizadas]) :-
    atualizar_status_tarefa(IdTarefa, NovoStatus, Resto, TarefasAtualizadas).

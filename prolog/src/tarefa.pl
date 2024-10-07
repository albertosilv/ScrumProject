:- module(tarefa, [adicionar_tarefa/2, listar_tarefas_usuario/2, atualizar_status_tarefa/2]).

:- use_module(usuario).

% Definição do status da tarefa
status_tarefa(backlog).
status_tarefa(pendente).
status_tarefa(em_desenvolvimento).
status_tarefa(concluido).

% Estrutura da tarefa
:- dynamic tarefa/7.

% Adicionar uma nova tarefa
adicionar_tarefa(Usuario, Tarefa) :-
    write('Digite o ID da Tarefa:'), nl,
    read(ID),
    write('Digite o Título da Tarefa:'), nl,
    read(Titulo),
    write('Digite a Descrição da Tarefa:'), nl,
    read(Descricao),
    write('Digite a Prioridade da Tarefa (1-5):'), nl,
    read(Prioridade),
    usuario:usuario_empresa_id(Usuario, EmpresaId),
    usuario:usuario_id(Usuario, CriadorId),
    assertz(tarefa(ID, Titulo, Descricao, Prioridade, backlog, CriadorId, 0, EmpresaId)),
    Tarefa = tarefa(ID, Titulo, Descricao, Prioridade, backlog, CriadorId, 0, EmpresaId),
    format('Tarefa adicionada: ~w~n', [Tarefa]).

% Listar as tarefas do usuário
listar_tarefas_usuario(Usuario, Tarefas) :-
    findall(_, tarefa(_, _, _, _, _, _, _, _), Tarefas),
    include(tarefa_de_usuario(Usuario), Tarefas, TarefasUsuario),
    (   TarefasUsuario = []
    ->  write('O usuário não possui tarefas.'), nl
    ;   write('Tarefas do usuário:'), nl,
        maplist(imprimir_tarefa, TarefasUsuario),
        write('Digite o ID da tarefa que deseja modificar o status ou 0 para sair:'), nl,
        read(TarefaIdEscolhida),
        (   TarefaIdEscolhida = 0
        ->  true
        ;   modificar_status(TarefaIdEscolhida)
        )
    ).

tarefa_de_usuario(Usuario, Tarefa) :-
    tarefa(_, Tarefa, _, _, _, CriadorId, ResponsavelId, _),
    (   CriadorId = Usuario
    ;   ResponsavelId = Usuario
    ).


imprimir_tarefa(tarefa(ID, Titulo, _, _, Status, _, _, _)) :-
    format('ID: ~w, Título: ~w, Status: ~w~n', [ID, Titulo, Status]).

% Modificar o status de uma tarefa
modificar_status(TarefaIdEscolhida) :-
    findall(_, tarefa(TarefaIdEscolhida, _, _, _, _, _, _, _), TarefaSelecionada),
    (   TarefaSelecionada = []
    ->  write('Tarefa não encontrada.'), nl
    ;   write('Digite o novo status da tarefa (2 para Pendente, 3 para Em Desenvolvimento, 4 para Concluído):'), nl,
        read(NovoStatusEntrada),
        (   NovoStatusEntrada = 2 -> NovoStatus = pendente
        ;   NovoStatusEntrada = 3 -> NovoStatus = em_desenvolvimento
        ;   NovoStatusEntrada = 4 -> NovoStatus = concluido
        ;   tarefa(TarefaIdEscolhida, _, _, _, StatusAtual, _, _, _),
            NovoStatus = StatusAtual
        ),
        atualizar_status_tarefa(TarefaIdEscolhida, NovoStatus),
        write('Status atualizado com sucesso!'), nl
    ).

% Atualiza o status de uma tarefa
atualizar_status_tarefa(ID, NovoStatus) :-
    retract(tarefa(ID, Titulo, Descricao, Prioridade, _, CriadorId, ResponsavelId, EmpresaId)),
    assertz(tarefa(ID, Titulo, Descricao, Prioridade, NovoStatus, CriadorId, ResponsavelId, EmpresaId)).

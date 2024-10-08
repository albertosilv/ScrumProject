:- module(tarefa, [adicionar_tarefa/1, listar_tarefas_backlog/1, listar_tarefas_usuario/1]).

:- use_module(usuario).

% Definição do status da tarefa
status_tarefa(backlog).
status_tarefa(pendente).
status_tarefa(em_desenvolvimento).
status_tarefa(concluido).

% Estrutura da tarefa
:- dynamic tarefa/7.


% Função para listar as tarefas com status de 'backlog' para uma empresa específica
listar_tarefas_backlog(Usuario) :- 
    usuario_empresa_id(Usuario, EmpresaId),  % Obtém o ID da empresa do usuário
    findall(tarefa(Id, Titulo, Descricao, Prioridade, backlog, CriadorId, ResponsavelId, EmpresaId), 
            tarefa(Id, Titulo, Descricao, Prioridade, backlog, CriadorId, ResponsavelId, EmpresaId), 
            Tarefas),
    (   Tarefas = []
    ->  writeln('Não há tarefas no backlog para esta empresa.')
    ;   listar_tarefas(Tarefas),
        writeln('\nOpções:'),
        writeln('1. Criar nova tarefa'),
        writeln('2. Voltar ao menu anterior'),
        read(Opcao),
        processar_opcao(Opcao, Usuario)
    ).

% Função para listar as tarefas
listar_tarefas([]).
listar_tarefas([Tarefa | Resto]) :- 
    imprimir_tarefa(Tarefa),
    listar_tarefas(Resto).

% Função para processar a opção escolhida
processar_opcao(1, Usuario) :- 
    adicionar_tarefa(Usuario),  % Chama a função para adicionar uma nova tarefa
    listar_tarefas_backlog(Usuario).  % Chama a função novamente para mostrar as tarefas atualizadas
processar_opcao(2, _) :- 
    writeln('Voltando ao menu anterior.').
processar_opcao(_, Usuario) :- 
    writeln('Opção inválida, tente novamente.'),
    listar_tarefas_backlog(Usuario).  % Chama a função novamente para mostrar as tarefas

% Função para adicionar uma nova tarefa
adicionar_tarefa(Usuario) :- 
    writeln('Digite o ID da Tarefa:'),
    read(Id),
    writeln('Digite o Título da Tarefa:'),
    read(Titulo),
    writeln('Digite a Descrição da Tarefa:'),
    read(Descricao),
    writeln('Digite a Prioridade da Tarefa (1-5):'),
    read(Prioridade),
    % Obtendo IDs da empresa e criador
    usuario_empresa_id(Usuario, EmpresaId),
    usuario_id(Usuario, CriadorId),
    % Criando a tarefa com status 'backlog'
    Tarefa = tarefa(Id, Titulo, Descricao, Prioridade, backlog, CriadorId, 0, EmpresaId),
    assertz(Tarefa),  % Adiciona a nova tarefa ao banco de dados dinâmico
    writeln('Tarefa adicionada com sucesso:'),
    imprimir_tarefa(Tarefa).

% Predicado para verificar se a tarefa pertence ao usuário (pelo CriadorId ou ResponsavelId)
tarefa_de_usuario(Usuario, tarefa(_, _, _, _, _, CriadorId, _, _)) :- 
    usuario_id(Usuario, CriadorId).
tarefa_de_usuario(Usuario, tarefa(_, _, _, _, _, _, ResponsavelId, _)) :- 
    usuario_id(Usuario, ResponsavelId).


% Função para listar as tarefas do usuário
listar_tarefas_usuario(Usuario) :- 
    findall(tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId), 
            tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId), 
            Tarefas),  % Obtém todas as tarefas
    include(tarefa_de_usuario(Usuario), Tarefas, TarefasUsuario),  % Filtra tarefas do usuário
    (   TarefasUsuario = []
    ->  write('O usuário não possui tarefas.'), nl
    ;   write('Tarefas do usuário:'), nl,
        maplist(imprimir_tarefa, TarefasUsuario),  % Imprime as tarefas do usuário
        write('Digite o ID da tarefa que deseja modificar o status ou 0 para sair:'), nl,
        read(TarefaIdEscolhida),
        (   TarefaIdEscolhida = 0
        ->  nl  % Apenas nova linha para melhor formatação
        ;   modificar_status(TarefaIdEscolhida)  % Chama a função para modificar o status
        )
    ).

% Função para imprimir uma tarefa
imprimir_tarefa(tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId, _)) :- 
    format('ID: ~w, Título: ~w, Descrição: ~w, Prioridade: ~w, Status: ~w, Criador: ~w, Responsável: ~w~n', 
           [Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId]).

% Função para modificar o status da tarefa
modificar_status(TarefaId) :- 
    retract(tarefa(TarefaId, Titulo, Descricao, Prioridade, _, CriadorId, ResponsavelId, EmpresaId)),  % Remove a tarefa antiga
    writeln('Digite o novo status para a tarefa (backlog, pendente, em_desenvolvimento, concluido):'),
    read(NovoStatus),
    (   status_tarefa(NovoStatus) ->  % Verifica se o novo status é válido
        assertz(tarefa(TarefaId, Titulo, Descricao, Prioridade, NovoStatus, CriadorId, ResponsavelId, EmpresaId)),  % Adiciona a tarefa com novo status
        writeln('Status da tarefa modificado com sucesso.')
    ;   writeln('Status inválido. A tarefa não foi alterada.')
    ).

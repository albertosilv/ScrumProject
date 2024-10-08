:- module(tarefa, [adicionar_tarefa/1, listar_tarefas_backlog/1, listar_tarefas_usuario/1]).

:- use_module(usuario).

status_tarefa(backlog).
status_tarefa(pendente).
status_tarefa(em_desenvolvimento).
status_tarefa(concluido).


:- dynamic tarefa/7.

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
        read_line_to_string(user_input, OpcaoStr),
        atom_number(OpcaoStr, Opcao),
        processar_opcao(Opcao, Usuario)
    ).

listar_tarefas([]).
listar_tarefas([Tarefa | Resto]) :- 
    imprimir_tarefa(Tarefa),
    listar_tarefas(Resto).

processar_opcao(1, Usuario) :- 
    adicionar_tarefa(Usuario),  
    listar_tarefas_backlog(Usuario). 
processar_opcao(2, _) :- 
    writeln('Voltando ao menu anterior.').
processar_opcao(_, Usuario) :- 
    writeln('Opção inválida, tente novamente.'),
    listar_tarefas_backlog(Usuario).  


adicionar_tarefa(Usuario) :- 
    writeln('Digite o ID da Tarefa:'),
    read_line_to_string(user_input, IdStr),
    atom_string(Id, IdStr),
    writeln('Digite o Título da Tarefa:'),
    read_line_to_string(user_input, Titulo),
    writeln('Digite a Descrição da Tarefa:'),
    read_line_to_string(user_input, Descricao),
    writeln('Digite a Prioridade da Tarefa (1-5):'),
    read_line_to_string(user_input, PrioridadeStr),
    atom_number(PrioridadeStr, Prioridade),
    usuario_empresa_id(Usuario, EmpresaId),
    usuario_id(Usuario, CriadorId),
    Tarefa = tarefa(Id, Titulo, Descricao, Prioridade, backlog, CriadorId, 0, EmpresaId),
    assertz(Tarefa),  
    writeln('Tarefa adicionada com sucesso:'),
    imprimir_tarefa(Tarefa).


tarefa_de_usuario(Usuario, tarefa(_, _, _, _, _, CriadorId, _, _)) :- 
    usuario_id(Usuario, CriadorId).
tarefa_de_usuario(Usuario, tarefa(_, _, _, _, _, _, ResponsavelId, _)) :- 
    usuario_id(Usuario, ResponsavelId).


listar_tarefas_usuario(Usuario) :- 
    findall(tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId), 
            tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId), 
            Tarefas),  % Obtém todas as tarefas
    include(tarefa_de_usuario(Usuario), Tarefas, TarefasUsuario),  
    (   TarefasUsuario = []
    ->  write('O usuário não possui tarefas.'), nl
    ;   write('Tarefas do usuário:'), nl,
        maplist(imprimir_tarefa, TarefasUsuario),  % Imprime as tarefas do usuário
        write('Digite o ID da tarefa que deseja modificar o status ou 0 para sair:'), nl,
        read_line_to_string(user_input, TarefaIdEscolhidaStr),
        atom_number(TarefaIdEscolhidaStr, TarefaIdEscolhida),
        (   TarefaIdEscolhida = 0
        ->  nl 
        ;   modificar_status(TarefaIdEscolhida)  
        )
    ).


imprimir_tarefa(tarefa(Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId, _)) :- 
    format('ID: ~w, Título: ~w, Descrição: ~w, Prioridade: ~w, Status: ~w, Criador: ~w, Responsável: ~w~n', 
           [Id, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId]).


modificar_status(TarefaId) :- 
    retract(tarefa(TarefaId, Titulo, Descricao, Prioridade, _, CriadorId, ResponsavelId, EmpresaId)),  
    writeln('Digite o novo status para a tarefa (backlog, pendente, em_desenvolvimento, concluido):'),
    read_line_to_string(user_input, NovoStatusStr),
    atom_string(NovoStatus, NovoStatusStr),
    (   status_tarefa(NovoStatus) -> 
        assertz(tarefa(TarefaId, Titulo, Descricao, Prioridade, NovoStatus, CriadorId, ResponsavelId, EmpresaId)),  
        writeln('Status da tarefa modificado com sucesso.')

    ;   writeln('Status inválido. A tarefa não foi alterada.')
    ).

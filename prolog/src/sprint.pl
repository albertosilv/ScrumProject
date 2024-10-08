:- module(sprint, [criar_sprint/1, listar_sprints_da_empresa/1, acessar_sprint/2, adicionar_tarefa_a_sprint/1, atribuir_tarefa/1]).

:- use_module(usuario).
:- use_module(tarefa).

:- dynamic sprint/6.

criar_sprint(Usuario) :-
    write('Digite o ID da Sprint:'), nl,
    read_line_to_string(user_input, IdStr),
    atom_number(IdStr, Id),  % Converte a string para número
    write('Digite o Nome da Sprint:'), nl,
    read_line_to_string(user_input, Nome),
    write('Digite a Duração da Sprint (em dias):'), nl,
    read_line_to_string(user_input, DuracaoStr),
    atom_number(DuracaoStr, Duracao),  % Converte a string para número
    usuario:usuario_empresa_id(Usuario, EmpresaId),
    assertz(sprint(Id, Nome, Duracao, [], usuario_id(Usuario), EmpresaId)),
    format('Sprint criada: ~w~n', [Id]).


listar_sprints_da_empresa(Usuario) :-
    usuario_empresa_id(Usuario, EmpresaId),
    findall(Sprint, sprint(_, Sprint, _, _, _, EmpresaId), SprintsDaEmpresa),
    (   SprintsDaEmpresa = []
    ->  writeln('Não há sprints nesta empresa.')
    ;   write('Sprints da Empresa:'), nl,
        maplist(print_sprint, SprintsDaEmpresa),
        write('Digite o ID da sprint para visualizar suas tarefas (ou -1 para voltar, 0 para criar uma nova sprint):'), nl,
        read_line_to_string(user_input, EntradaStr),
        atom_number(EntradaStr, Entrada),  % Converte a string para número
        processar_entrada(Entrada, Usuario)  % Processa a entrada do usuário
    ).


processar_entrada(-1, _) :- !.  
processar_entrada(0, Usuario) :- 
    criar_sprint(Usuario),
    listar_sprints_da_empresa(Usuario).
processar_entrada(Entrada, Usuario) :- 
    (   sprint(Entrada, SprintEscolhida, _, _, _, _) -> 
        acessar_sprint(Usuario, SprintEscolhida) 
    
    ;   writeln('Sprint não encontrada.'),
        listar_sprints_da_empresa(Usuario)  %
    ).


acessar_sprint(Usuario, Sprint) :-
    sprint(Sprint, Nome, _, _, _, _),
    format('Sprint Selecionada: ~w~n', [Nome]),
    write('Tarefas da Sprint:'), nl,


    findall(TarefaId, tarefa_id(TarefaId, Sprint), TarefasDaSprint),
    
    (   TarefasDaSprint = []
    ->  writeln('Nenhuma tarefa nesta sprint.')
    ;   maplist(print_tarefa, TarefasDaSprint)
    ),
    

    write('Escolha uma opção:'), nl,
    write('1. Adicionar Tarefa à Sprint'), nl,
    write('2. Atribuir Tarefa a um Usuário'), nl,
    write('0. Voltar'), nl,
    read_line_to_string(user_input, EscolhaStr),
    atom_number(EscolhaStr, Escolha),  % Converte a string para número
    
    (   Escolha = 1 -> 
        adicionar_tarefa_a_sprint(Sprint),  % Adiciona tarefa à sprint
        acessar_sprint(Usuario, Sprint)  % Rechama após adicionar tarefa
    ;   Escolha = 2 -> 
        atribuir_tarefa(Usuario),  % Atribui tarefa a um usuário
        acessar_sprint(Usuario, Sprint)  % Rechama após atribuir tarefa
    ;   Escolha = 0 -> 
        true  
    ;   writeln('Opção inválida, tente novamente.'),
        acessar_sprint(Usuario, Sprint)  % Tentar novamente em caso de erro
    ).


print_tarefa(TarefaId) :- 
    tarefa(TarefaId, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId, _),
    format('ID: ~w, Título: ~w, Descrição: ~w, Prioridade: ~w, Status: ~w, Criador: ~w, Responsável: ~w~n', 
           [TarefaId, Titulo, Descricao, Prioridade, Status, CriadorId, ResponsavelId]).


adicionar_tarefa_a_sprint(Sprint) :- 
    write('Digite o ID da tarefa a ser adicionada à sprint:'), nl,
    read_line_to_string(user_input, TarefaIdStr),
    atom_number(TarefaIdStr, TarefaId),  % Converte a string para número
    (   tarefa(TarefaId, _, _, _, backlog, CriadorId, _, EmpresaId) -> 
        retract(sprint(Sprint, Nome, Duracao, Tarefas, CriadorId, EmpresaId)),
        assertz(sprint(Sprint, Nome, Duracao, [TarefaId | Tarefas], CriadorId, EmpresaId)),
        writeln('Tarefa adicionada à sprint com sucesso!')
    ;   writeln('Tarefa não encontrada ou não está no status "Backlog".')
    ).

atribuir_tarefa(Usuario) :- 
    write('Digite o ID da tarefa que deseja atribuir:'), nl,
    read_line_to_string(user_input, TarefaIdStr),
    atom_number(TarefaIdStr, TarefaId),  % Converte a string para número
    usuario_empresa_id(Usuario, EmpresaId),  % Obtendo o EmpresaId do usuário

    (   tarefa(TarefaId, Titulo, Descricao, Prioridade, backlog, CriadorId, _, EmpresaId) ->  % Verifica se a tarefa existe e tem status 'backlog'
        write('Digite o ID do usuário para atribuir a tarefa:'), nl,
        read_line_to_string(user_input, UsuarioIdStr),
        atom_number(UsuarioIdStr, UsuarioId),  % Converte a string para número

        
        tipo_usuario(UsuarioId, 3),
        
        retract(tarefa(TarefaId, Titulo, Descricao, Prioridade, backlog, CriadorId, _, EmpresaId)),
        assertz(tarefa(TarefaId, Titulo, Descricao, Prioridade, backlog, CriadorId, UsuarioId, EmpresaId)),  % Adiciona a tarefa atualizada
        writeln('Tarefa atribuída com sucesso!')
    ;   writeln('Tarefa não encontrada ou não está no status correto.')
    ).

print_sprint(sprint(Id, Nome, Duracao, _, _, _)) :- 
    format('ID: ~w, Nome: ~w, Duração: ~w dias~n', [Id, Nome, Duracao]).

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
    read_line_to_string(user_input, EscolhaString),
    atom_number(EscolhaString, Escolha),  % Converte a string para número
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
        null
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
    read_line_to_string(user_input, EscolhaString),
    atom_number(EscolhaString, Escolha),  % Converte a string para número
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
        writeln('Saindo do menu...'),  % Mensagem opcional
        true  % Sai do menu sem repetir
    ;   
        writeln('Opção inválida, tente novamente.'),
        menu_comum(Usuario)  % Repete o menu em caso de erro
    ).

% Atualiza os dados do usuário
update_usuario(Usuario, UsuarioAtualizado) :-
    findall(U, usuario(U), Usuarios),  % Obtém a lista de todos os usuários
    maplist(replace_usuario(Usuario, UsuarioAtualizado), Usuarios, UsuariosAtualizados),
    retractall(usuario(_)),  % Remove todos os usuários antigos
    maplist(assertz, UsuariosAtualizados).  % Adiciona a lista atualizada de usuários

% Predicado auxiliar para substituir um usuário
replace_usuario(Usuario, UsuarioAtualizado, Usuario, UsuarioAtualizado) :- !.  % Substitui o usuário
replace_usuario(_, _, U, U).  % Mantém os outros usuários

:- module(usuario, [usuario/6, usuario_papel/2, cadastrar_usuario/0, login/2, modificar_usuario/2, usuario_empresa_id/2]).

:- dynamic usuario/6.


papel_string(1, 'Product Owner').
papel_string(2, 'Scrum Master').
papel_string(3, 'Dev Team').

% Função para cadastrar um usuário
cadastrar_usuario :- 
    writeln('Digite o ID do Usuário:'),
    read_line_to_string(user_input, IdStr),
    atom_string(Id, IdStr),
    writeln('Digite o Nome do Usuário:'),
    read_line_to_string(user_input, Nome),
    writeln('Digite o Email do Usuário:'),
    read_line_to_string(user_input, Email),
    writeln('Digite a senha do Usuário:'),
    read_line_to_string(user_input, Senha),
    writeln('Selecione o tipo de usuário (1 para Product Owner, 2 para Scrum Master, 3 para Dev Team):'),
    read_line_to_string(user_input, TipoStr),
    atom_number(TipoStr, Papel),  % Papel agora é numérico
    (Papel >= 1, Papel =< 3 -> true; Papel = 3),  % Padrão para Dev Team se entrada inválida
    writeln('Digite o ID da Empresa:'),
    read_line_to_string(user_input, EmpresaIdStr),
    atom_string(EmpresaId, EmpresaIdStr),
    assert(usuario(Id, Nome, Email, Senha, Papel, EmpresaId)),
    writeln('Usuário cadastrado: '),
    writeln(usuario(Id, Nome, Email)).

% Função para fazer login de um usuário
login(Usuario, PapelNum) :-
  writeln('Digite o Email do Usuário:'),
  read_line_to_string(user_input, Email),
  findall(usuario(Id, Nome, Email, Senha, PapelNum, EmpresaId), usuario(Id, Nome, Email, Senha, PapelNum, EmpresaId), Usuarios),
  (   Usuarios = [] ->
    writeln('Usuário não encontrado.'),
    Usuario = none,
    PapelNum = 0
  ;   Usuarios = [usuario(Id, Nome, Email, Senha, PapelNum, EmpresaId)] ->
    writeln('Digite a Senha:'),
    read_line_to_string(user_input, SenhaInput),
    (   SenhaInput == Senha ->
      format('Bem-vindo, ~w!~n', [Nome]),
      Usuario = usuario(Id, Nome, Email, Senha, PapelNum, EmpresaId)
    ;   writeln('Senha incorreta.'),
      Usuario = none,
      PapelNum = 0
    )
  ).

% Função para modificar os dados de um usuário
modificar_usuario(usuario(Id, Nome, Email, Senha, Papel, EmpresaId), UsuarioModificado) :-
    writeln('\nDados do Usuário:'),
    format('1. Nome: ~w~n', [Nome]),
    format('2. Email: ~w~n', [Email]),
    format('3. Senha: ~w~n', [Senha]),
    format('4. ID da Empresa (Não pode ser modificado): ~w~n', [EmpresaId]),
    writeln('Escolha o número do campo que deseja modificar (ou 0 para sair):'),
    read_line_to_string(user_input, EscolhaStr),
    atom_number(EscolhaStr, Escolha),
    (Escolha = 1 ->
        writeln('Digite o novo nome:'),
        read_line_to_string(user_input, NovoNome),
        UsuarioModificado = usuario(Id, NovoNome, Email, Senha, Papel, EmpresaId);
     Escolha = 2 ->
        writeln('Digite o novo email:'),
        read_line_to_string(user_input, NovoEmail),
        UsuarioModificado = usuario(Id, Nome, NovoEmail, Senha, Papel, EmpresaId);
     Escolha = 3 ->
        writeln('Digite a nova senha:'),
        read_line_to_string(user_input, NovaSenha),
        UsuarioModificado = usuario(Id, Nome, Email, NovaSenha, Papel, EmpresaId);
     Escolha = 0 ->
        UsuarioModificado = usuario(Id, Nome, Email, Senha, Papel, EmpresaId);
     writeln('Opção inválida, tente novamente.'),
     modificar_usuario(usuario(Id, Nome, Email, Senha, Papel, EmpresaId), UsuarioModificado)
    ).

% Função para obter o ID da empresa de um usuário
usuario_empresa_id(Usuario, EmpresaId) :- 
    usuario(Usuario, _, _, _, _, EmpresaId).

% Função para verificar o papel de um usuário
usuario_papel(Usuario, Papel) :- 
    usuario(Usuario, _, _, _, PapelNum, _),  
    format('Usuário: ~w, Papel numérico: ~w~n', [Usuario, PapelNum]),
    Papel = PapelNum.
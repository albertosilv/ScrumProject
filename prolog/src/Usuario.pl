% Definição dos tipos de usuários
tipo_usuario(product_owner).
tipo_usuario(scrum_master).
tipo_usuario(dev_team).

% Estrutura do usuário
usuario(Id, Nome, Email, Senha, Papel, EmpresaId).

% Função para cadastrar um usuário
cadastrar_usuario(Usuario) :-
    write('Digite o ID do Usuário:'), nl,
    read(Id),
    write('Digite o Nome do Usuário:'), nl,
    read(Nome),
    write('Digite o Email do Usuário:'), nl,
    read(Email),
    write('Digite a Senha do Usuário:'), nl,
    read(Senha),
    write('Selecione o tipo de usuário (1 para Product Owner, 2 para Scrum Master, 3 para Dev Team):'), nl,
    read(Tipo),
    (Tipo = 1 -> Papel = product_owner;
     Tipo = 2 -> Papel = scrum_master;
     Tipo = 3 -> Papel = dev_team;
     Papel = dev_team),  % Padrão para dev_team se a entrada for inválida
    write('Digite o ID da Empresa:'), nl,
    read(EmpresaId),
    Usuario = usuario(Id, Nome, Email, Senha, Papel, EmpresaId),
    write('Usuário cadastrado: '), write(Usuario), nl.

% Função para login de um usuário
login(Usuarios, Resultado) :-
    write('Digite o Email do Usuário:'), nl,
    read(Email),
    ( member(usuario(_, Nome, Email, Senha, _, _), Usuarios) ->
        write('Digite a Senha:'), nl,
        read(SenhaInserida),
        (SenhaInserida = Senha ->
            write('Login bem-sucedido! Bem-vindo, '), write(Nome), nl,
            Resultado = usuario(_, Nome, Email, Senha, _, _)
        ;
            write('Senha incorreta.'), nl,
            Resultado = none)
    ;
        write('Usuário não encontrado.'), nl,
        Resultado = none).

% Função para modificar um usuário
modificar_usuario(Usuario, UsuarioModificado) :-
    Usuario = usuario(Id, Nome, Email, Senha, Papel, EmpresaId),
    write('\nDados do Usuário:\n'),
    write('1. Nome: '), write(Nome), nl,
    write('2. Email: '), write(Email), nl,
    write('3. Senha: '), write(Senha), nl,
    write('4. ID da Empresa (Não pode ser modificado): '), write(EmpresaId), nl,
    write('Escolha o número do campo que deseja modificar (ou 0 para sair):'), nl,
    read(Escolha),
    (Escolha = 1 ->
        write('Digite o novo nome:'), nl,
        read(NovoNome),
        modificar_usuario(usuario(Id, NovoNome, Email, Senha, Papel, EmpresaId), UsuarioModificado)
    ; Escolha = 2 ->
        write('Digite o novo email:'), nl,
        read(NovoEmail),
        modificar_usuario(usuario(Id, Nome, NovoEmail, Senha, Papel, EmpresaId), UsuarioModificado)
    ; Escolha = 3 ->
        write('Digite a nova senha:'), nl,
        read(NovaSenha),
        modificar_usuario(usuario(Id, Nome, Email, NovaSenha, Papel, EmpresaId), UsuarioModificado)
    ; Escolha = 0 ->
        UsuarioModificado = Usuario
    ; 
        write('Opção inválida, tente novamente.'), nl,
        modificar_usuario(Usuario, UsuarioModificado)).

:- module(usuario, [usuario/6, cadastrar_usuario/1, login/2, modificar_usuario/2]).

:- dynamic usuario/6.

tipo_usuario(product_owner).
tipo_usuario(scrum_master).
tipo_usuario(dev_team).

cadastrar_usuario(usuario(Id, Nome, Email, Senha, Papel, EmpresaId)) :-
    write('Digite o ID do Usuário:'), nl,
    read(Id),
    write('Digite o Nome do Usuário:'), nl,
    read(Nome),
    write('Digite o Email do Usuário:'), nl,
    read(Email),
    write('Digite a senha do Usuário:'), nl,
    read(Senha),
    write('Selecione o tipo de usuário (1 para Product Owner, 2 para Scrum Master, 3 para Dev Team):'), nl,
    read(Tipo),
    (Tipo = 1 -> Papel = product_owner;
     Tipo = 2 -> Papel = scrum_master;
     Tipo = 3 -> Papel = dev_team;
     Papel = dev_team),  % Padrão para Dev Team se entrada inválida
    write('Digite o ID da Empresa:'), nl,
    read(EmpresaId),
    assert(usuario(Id, Nome, Email, Senha, Papel, EmpresaId)),
    write('Usuário cadastrado: '), write(usuario(Id, Nome, Email, Senha, Papel, EmpresaId)), nl.

login(Usuarios, Usuario) :-
    write('Digite o Email do Usuário:'), nl,
    read(Email),
    findall(usuario(Id, Nome, Email, Senha, Papel, EmpresaId), usuario(Id, Nome, Email, Senha, Papel, EmpresaId), Usuarios),
    (Usuarios = [] ->
        write('Usuário não encontrado.'), nl,
        Usuario = null;
        (Usuarios = [usuario(Id, Nome, Email, Senha, Papel, EmpresaId)] ->
            write('Digite a Senha:'), nl,
            read(SenhaInput),
            (SenhaInput == Senha ->
                write('Login bem-sucedido! Bem-vindo, '), write(Nome), nl,
                Usuario = usuario(Id, Nome, Email, Senha, Papel, EmpresaId);
                write('Senha incorreta.'), nl,
                Usuario = null
            )
        )
    ).

modificar_usuario(usuario(Id, Nome, Email, Senha, Papel, EmpresaId), UsuarioModificado) :-
    write('\nDados do Usuário:'), nl,
    write('1. Nome: '), write(Nome), nl,
    write('2. Email: '), write(Email), nl,
    write('3. Senha: '), write(Senha), nl,
    write('4. ID da Empresa (Não pode ser modificado): '), write(EmpresaId), nl,
    write('Escolha o número do campo que deseja modificar (ou 0 para sair):'), nl,
    read(Escolha),
    (Escolha = 1 ->
        write('Digite o novo nome:'), nl,
        read(NovoNome),
        modificar_usuario(usuario(Id, NovoNome, Email, Senha, Papel, EmpresaId), UsuarioModificado);
     Escolha = 2 ->
        write('Digite o novo email:'), nl,
        read(NovoEmail),
        modificar_usuario(usuario(Id, Nome, NovoEmail, Senha, Papel, EmpresaId), UsuarioModificado);
     Escolha = 3 ->
        write('Digite a nova senha:'), nl,
        read(NovaSenha),
        modificar_usuario(usuario(Id, Nome, Email, NovaSenha, Papel, EmpresaId), UsuarioModificado);
     Escolha = 0 ->
        UsuarioModificado = usuario(Id, Nome, Email, Senha, Papel, EmpresaId);
     write('Opção inválida, tente novamente.'), nl,
     modificar_usuario(usuario(Id, Nome, Email, Senha, Papel, EmpresaId), UsuarioModificado)
    ).

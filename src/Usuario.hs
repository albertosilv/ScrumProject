module Usuario (Usuario (..), TipoUsuario (..), cadastrarUsuario, login, modificarUsuario) where

data TipoUsuario = ProductOwner | ScrumMaster | DevTeam
  deriving (Show, Eq)

data Usuario = Usuario
  { usuarioId :: Int,
    usuarioNome :: String,
    usuarioEmail :: String,
    usuarioSenha :: String,
    usuarioPapel :: TipoUsuario,
    usuarioEmpresaId :: Int
  }
  deriving (Show, Eq)

cadastrarUsuario :: IO Usuario
cadastrarUsuario = do
  putStrLn "Digite o ID do Usuário:"
  id <- readLn
  putStrLn "Digite o Nome do Usuário:"
  nome <- getLine
  putStrLn "Digite o Email do Usuário:"
  email <- getLine
  putStrLn "Digite a senha do Usuário:"
  senha <- getLine
  putStrLn "Selecione o tipo de usuário (1 para Product Owner, 2 para Scrum Master, 3 para Dev Team):"
  tipo <- getLine
  let papel = case tipo of
        "1" -> ProductOwner
        "2" -> ScrumMaster
        "3" -> DevTeam
        _ -> DevTeam -- Padrão para DevTeam se entrada inválida
  putStrLn "Digite o ID da Empresa:"
  empresaId <- readLn
  let usuario = Usuario id nome email senha papel empresaId
  putStrLn $ "Usuário cadastrado: " ++ show usuario
  return usuario

login :: [Usuario] -> IO (Maybe Usuario)
login usuarios = do
  putStrLn "Digite o Email do Usuário:"
  email <- getLine
  let usuario = filter (\u -> usuarioEmail u == email) usuarios
  if null usuario
    then do
      putStrLn "Usuário não encontrado."
      return Nothing
    else do
      let user = head usuario
      putStrLn "Digite a Senha:"
      senha <- getLine
      if senha == usuarioSenha user
        then do
          putStrLn $ "Login bem-sucedido! Bem-vindo, " ++ usuarioNome user
          return $ Just user
        else do
          putStrLn "Senha incorreta."
          return Nothing

modificarUsuario :: Usuario ->  IO Usuario
modificarUsuario usuario = do
  putStrLn "\nDados do Usuário:"
  putStrLn $ "1. Nome: " ++ usuarioNome usuario
  putStrLn $ "2. Email: " ++ usuarioEmail usuario
  putStrLn $ "3. Senha: " ++ usuarioSenha usuario
  putStrLn $ "4. ID da Empresa(Não pode ser modificado): " ++ show (usuarioEmpresaId usuario)
  putStrLn "Escolha o número do campo que deseja modificar (ou 0 para sair):"
  escolha <- getLine
  case escolha of
    "1" -> do
      putStrLn "Digite o novo nome:"
      novoNome <- getLine
      modificarUsuario usuario { usuarioNome = novoNome }  
    "2" -> do
      putStrLn "Digite o novo email:"
      novoEmail <- getLine
      modificarUsuario usuario { usuarioEmail = novoEmail }  
    "3" -> do
      putStrLn "Digite a nova senha:"
      novaSenha <- getLine
      modificarUsuario usuario { usuarioSenha = novaSenha }  
    "0" -> return usuario  
    _   -> do
      putStrLn "Opção inválida, tente novamente."
      modificarUsuario usuario  -- Chama novamente em caso d
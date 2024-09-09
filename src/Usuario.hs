
module Usuario where

data Usuario = Usuario
  { usuarioId :: Int
  , usuarioNome :: String
  , usuarioEmail :: String
  , usuarioPapel :: String  
  } deriving (Show, Eq)

cadastrarUsuario :: IO Usuario
cadastrarUsuario = do
  putStrLn "Digite o ID do Usuário:"
  id <- readLn
  putStrLn "Digite o Nome do Usuário:"
  nome <- getLine
  putStrLn "Digite o Email do Usuário:"
  email <- getLine
  putStrLn "Digite o Papel do Usuário (e.g., Desenvolvedor, Product Owner):"
  papel <- getLine
  let usuario = Usuario id nome email papel
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
    else return $ Just (head usuario)
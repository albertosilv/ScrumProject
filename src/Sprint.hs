
module Sprint where

import Tarefa (Tarefa(..))

data Sprint = Sprint
  { sprintId :: Int
  , sprintNome :: String
  , sprintDuracao :: Int
  , sprintTarefas :: [Tarefa]
  } deriving (Show, Eq)

criarSprint :: IO Sprint
criarSprint = do
  putStrLn "Digite o ID da Sprint:"
  id <- readLn
  putStrLn "Digite o Nome da Sprint:"
  nome <- getLine
  putStrLn "Digite a Duração da Sprint (em dias):"
  duracao <- readLn
  let sprint = Sprint id nome duracao []
  putStrLn $ "Sprint criada: " ++ show sprint
  return sprint

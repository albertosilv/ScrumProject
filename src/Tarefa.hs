
module Tarefa where

data Tarefa = Tarefa
  { tarefaId :: Int
  , tarefaTitulo :: String
  , tarefaDescricao :: String
  , tarefaPrioridade :: Int
  , tarefaStatus :: String
  } deriving (Show, Eq)

adicionarTarefa :: IO Tarefa
adicionarTarefa = do
  putStrLn "Digite o ID da Tarefa:"
  id <- readLn
  putStrLn "Digite o Título da Tarefa:"
  titulo <- getLine
  putStrLn "Digite a Descrição da Tarefa:"
  descricao <- getLine
  putStrLn "Digite a Prioridade da Tarefa (1-5):"
  prioridade <- readLn
  let tarefa = Tarefa id titulo descricao prioridade "Pendente"
  putStrLn $ "Tarefa adicionada: " ++ show tarefa
  return tarefa

listarTarefas :: [Tarefa] -> IO ()
listarTarefas = mapM_ print

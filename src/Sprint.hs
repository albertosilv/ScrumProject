
module Sprint where

import Tarefa (Tarefa(..),StatusTarefa(..))
import Usuario (Usuario(..))
import Data.Maybe (fromMaybe)
import Data.List (find)  -- Importa a função find do módulo Data.List


data Sprint = Sprint
  { sprintId :: Int
  , sprintNome :: String
  , sprintDuracao :: Int
  , sprintTarefas :: [Int]
  , sprintIdCriador :: Int
  , sprintEmpresaId :: Int
  } deriving (Show, Eq)

criarSprint :: Usuario -> IO Sprint
criarSprint usuario = do
  putStrLn "Digite o ID da Sprint:"
  id <- readLn
  putStrLn "Digite o Nome da Sprint:"
  nome <- getLine
  putStrLn "Digite a Duração da Sprint (em dias):"
  duracao <- readLn
  let empresaId = usuarioEmpresaId usuario  -- Pega o ID da Empresa do usuário
  let sprint = Sprint id nome duracao [] 0 empresaId
  putStrLn $ "Sprint criada: " ++ show sprint 
  return sprint

listarSprint :: Usuario -> [Sprint] -> [Tarefa] -> IO ()
listarSprint usuario sprints tarefas = do
  let sprintsCriadas = filter (\s -> sprintIdCriador s == usuarioId usuario) sprints

  let tarefasUsuario = filter (\t -> tarefaIdCriador t == usuarioId usuario || tarefaIdResponsavel t == usuarioId usuario) tarefas

  let idsSprintsUsuario = map tarefaId tarefasUsuario

  let sprintsEnvolvido = filter (\sprint -> any (`elem` sprintTarefas sprint) idsSprintsUsuario) sprints

  let todasSprints = sprintsCriadas ++ sprintsEnvolvido

  let sprintsUnicas = removeDuplicatas todasSprints

  if null sprintsUnicas
    then putStrLn "O usuário não possui sprints."
    else do
      putStrLn "Sprints do usuário:"
      mapM_ (putStrLn . sprintNome) sprintsUnicas

-- Função auxiliar para remover duplicatas da lista de sprints
removeDuplicatas :: [Sprint] -> [Sprint]
removeDuplicatas = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Função para listar sprints da empresa
listarSprintsDaEmpresa :: Usuario -> [Sprint] -> [Usuario] -> [Tarefa] -> IO ([Sprint], [Tarefa])
listarSprintsDaEmpresa usuario sprints usuarios tarefas = do
  let sprintsDaEmpresa = filter (\s -> sprintEmpresaId s == usuarioEmpresaId usuario) sprints
  if null sprintsDaEmpresa
    then do 
      putStrLn "Nenhuma sprint encontrada para esta empresa."
      return (sprints, tarefas)  
    else do
      putStrLn "Sprints da Empresa:"
      mapM_ (\s -> putStrLn $ "ID: " ++ show (sprintId s) ++ ", Nome: " ++ sprintNome s) sprintsDaEmpresa
      putStrLn "Digite o ID da sprint para visualizar suas tarefas (ou 0 para voltar, 1 para criar uma nova sprint):"
      entrada <- getLine
      case entrada of
        "0" -> return (sprints, tarefas)
        "1" -> do
          novaSprint <- criarSprint usuario
          let sprintsAtualizados = novaSprint : sprints
          return (sprintsAtualizados, tarefas)
        _ -> do
          let sprintIdEscolhida = read entrada
          case find (\s -> sprintId s == sprintIdEscolhida) sprintsDaEmpresa of
            Nothing -> putStrLn "Sprint não encontrada." >> listarSprintsDaEmpresa usuario sprints usuarios tarefas
            Just sprint -> do
              (sprintsAtualizados, tarefasAtualizadas) <- acessarSprint usuario sprint sprints tarefas
              listarSprintsDaEmpresa usuario sprintsAtualizados usuarios tarefasAtualizadas



-- Função para acessar uma sprint e suas tarefas
acessarSprint :: Usuario -> Sprint -> [Sprint] -> [Tarefa] -> IO ([Sprint], [Tarefa])
acessarSprint usuario sprint sprints tarefas = do
  putStrLn $ "Sprint Selecionada: " ++ sprintNome sprint
  putStrLn "Tarefas da Sprint:"
  let tarefasDaSprint = filter (\t -> tarefaId t `elem` sprintTarefas sprint) tarefas
  mapM_ (\t -> putStrLn $ "ID: " ++ show (tarefaId t) ++ ", Título: " ++ tarefaTitulo t) tarefasDaSprint
  putStrLn "Digite o ID da tarefa para atribuir a um usuário (ou 0 para voltar):"
  entrada <- getLine
  if entrada == "0"
    then return (sprints, tarefas) 
    else do
      let tarefaIdEscolhida = read entrada
      case find (\t -> tarefaId t == tarefaIdEscolhida) tarefasDaSprint of
        Nothing -> putStrLn "Tarefa não encontrada." >> acessarSprint usuario sprint sprints tarefas
        Just tarefa -> do
          tarefaAtualizada <- atribuirTarefa usuario tarefa
          let tarefasAtualizadas = map (\t -> if tarefaId t == tarefaIdEscolhida then tarefaAtualizada else t) tarefas
          return (sprints, tarefasAtualizadas)

-- Função para atribuir uma tarefa a um usuário
atribuirTarefa :: Usuario -> Tarefa -> IO Tarefa
atribuirTarefa usuario tarefa = do
  putStrLn $ "Tarefa Selecionada: " ++ tarefaTitulo tarefa
  putStrLn "Digite o ID do usuário para atribuir a tarefa:"
  usuarioIdResponsavel <- readLn
  let tarefaAtualizada = tarefa { tarefaStatus = Pendente, tarefaIdResponsavel = usuarioIdResponsavel }
  putStrLn $ "Tarefa atribuída com sucesso! Novo status: " ++ show (tarefaStatus tarefaAtualizada)
  return tarefaAtualizada
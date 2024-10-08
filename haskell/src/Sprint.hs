module Sprint where

import Data.List (find) -- Importa a função find do módulo Data.List
import Data.Maybe (fromMaybe)
import Tarefa (StatusTarefa (..), Tarefa (..))
import Usuario (Usuario (..),TipoUsuario(..))

data Sprint = Sprint
  { sprintId :: Int,
    sprintNome :: String,
    sprintDuracao :: Int,
    sprintTarefas :: [Int],
    sprintIdCriador :: Int,
    sprintEmpresaId :: Int
  }
  deriving (Show, Eq)

criarSprint :: Usuario -> IO Sprint
criarSprint usuario = do
  putStrLn "Digite o ID da Sprint:"
  id <- readLn
  putStrLn "Digite o Nome da Sprint:"
  nome <- getLine
  putStrLn "Digite a Duração da Sprint (em dias):"
  duracao <- readLn
  let empresaId = usuarioEmpresaId usuario -- Pega o ID da Empresa do usuário
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
  putStrLn "Sprints da Empresa:"
  mapM_ (\s -> putStrLn $ "ID: " ++ show (sprintId s) ++ ", Nome: " ++ sprintNome s) sprintsDaEmpresa
  putStrLn "Digite o ID da sprint para visualizar suas tarefas (ou -1 para voltar, 0 para criar uma nova sprint):"
  entrada <- getLine
  case entrada of
    "-1" -> return (sprints, tarefas)
    "0" -> do
      novaSprint <- criarSprint usuario
      let sprintsAtualizados = novaSprint : sprints
      listarSprintsDaEmpresa usuario sprintsAtualizados usuarios tarefas
    _ -> do
      let sprintIdEscolhida = read entrada
      case find (\s -> sprintId s == sprintIdEscolhida) sprintsDaEmpresa of
        Nothing -> putStrLn "Sprint não encontrada." >> listarSprintsDaEmpresa usuario sprints usuarios tarefas
        Just sprint -> do
          (sprintsAtualizados, tarefasAtualizadas) <- acessarSprint usuario sprint sprints tarefas usuarios
          listarSprintsDaEmpresa usuario sprintsAtualizados usuarios tarefasAtualizadas

acessarSprint :: Usuario -> Sprint -> [Sprint] -> [Tarefa] ->[Usuario] -> IO ([Sprint], [Tarefa])
acessarSprint usuario sprint sprints tarefas usuarios = do
  putStrLn $ "Sprint Selecionada: " ++ sprintNome sprint
  putStrLn "Tarefas da Sprint:"
  let tarefasDaSprint = filter (\t -> tarefaId t `elem` sprintTarefas sprint) tarefas
  mapM_ (\t -> putStrLn $ "ID: " ++ show (tarefaId t) ++ ", Título: " ++ tarefaTitulo t ++ ", Status: " ++ show (tarefaStatus t)) tarefasDaSprint
  putStrLn "Escolha uma opção:"
  putStrLn "1. Adicionar Tarefa à Sprint"
  putStrLn "2. Atribuir Tarefa a um Usuário"
  putStrLn "0. Voltar"
  escolha <- getLine
  case escolha of
    "1" -> do
      (sprintAtualizada, sprintsAtualizados, tarefasAtualizadas) <- adicionarTarefaASprint usuario sprint sprints tarefas
      acessarSprint usuario sprintAtualizada sprintsAtualizados tarefasAtualizadas usuarios
    "2" -> do
      tarefasAtualizadas <- atribuirTarefa usuario tarefas usuarios
      acessarSprint usuario sprint sprints tarefasAtualizadas usuarios
    "0" -> return (sprints, tarefas)
    _   -> do
      putStrLn "Opção inválida, tente novamente."
      acessarSprint usuario sprint sprints tarefas usuarios
-- Função para adicionar uma tarefa à sprint
-- Função para adicionar uma tarefa à sprint
adicionarTarefaASprint :: Usuario -> Sprint -> [Sprint] -> [Tarefa] -> IO (Sprint, [Sprint], [Tarefa])
adicionarTarefaASprint usuario sprint sprints tarefas = do
  putStrLn "Digite o ID da tarefa para adicionar à sprint:"
  tarefaIdEscolhida <- readLn
  case find (\t -> tarefaId t == tarefaIdEscolhida && tarefaStatus t == Backlog) tarefas of
    Nothing -> do
      putStrLn "Tarefa não encontrada ou não está no status 'Backlog'."
      return (sprint, sprints, tarefas)
    Just tarefa -> do
      let tarefaAtualizada = tarefa { tarefaStatus = Pendente }
          tarefasAtualizadas = map (\t -> if tarefaId t == tarefaIdEscolhida then tarefaAtualizada else t) tarefas
          
          sprintAtualizada = sprint { sprintTarefas = tarefaIdEscolhida : sprintTarefas sprint }
          sprintsAtualizados = map (\s -> if sprintId s == sprintId sprint then sprintAtualizada else s) sprints
          
      putStrLn "Tarefa adicionada à sprint e status atualizado com sucesso!"
      return (sprintAtualizada, sprintsAtualizados, tarefasAtualizadas)
-- Função para atribuir uma tarefa a um usuário
atribuirTarefa :: Usuario -> [Tarefa] -> [Usuario] -> IO [Tarefa]
atribuirTarefa usuario tarefas usuarios = do
  putStrLn "Digite o ID da tarefa para atribuir a um usuário:"
  tarefaIdEscolhida <- readLn
  let tarefaSelecionada = find (\t -> tarefaId t == tarefaIdEscolhida && tarefaStatus t == Pendente) tarefas
  case tarefaSelecionada of
    Nothing -> do
      putStrLn "Tarefa não encontrada ou não está no status 'Pendente'."
      return tarefas -- Retorna as listas originais, pois a tarefa não foi encontrada
    Just tarefa -> do
      putStrLn "Digite o ID do usuário para atribuir a tarefa:"
      usuarioIdEscolhido <- readLn
      case find (\u -> usuarioId u == usuarioIdEscolhido && usuarioEmpresaId u == tarefaEmpresaId tarefa && usuarioPapel u == DevTeam) usuarios of
        Nothing -> do
          putStrLn "Usuário não encontrado, não pertence à empresa ou não é um desenvolvedor."
          return tarefas -- Retorna as listas originais, pois o usuário não é válido
        Just usuarioAtribuido -> do
          -- Atualiza a tarefa com o responsável e o status
          let tarefaAtualizada = tarefa { tarefaIdResponsavel =  usuarioId usuarioAtribuido, tarefaStatus = Pendente }
              tarefasAtualizadas = map (\t -> if tarefaId t == tarefaIdEscolhida then tarefaAtualizada else t) tarefas
          putStrLn "Tarefa atribuída com sucesso!"
          return tarefasAtualizadas
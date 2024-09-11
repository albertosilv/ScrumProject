
module Tarefa where
import Usuario (Usuario(..))


data StatusTarefa = Backlog | Pendente | EmDesenvolvimento | Concluido
  deriving (Eq)

instance Show StatusTarefa where
  show Backlog = "Backlog"
  show Pendente = "Pendente"
  show EmDesenvolvimento = "Em Desenvolvimento"
  show Concluido = "Concluído"
data Tarefa = Tarefa
  { tarefaId :: Int
  , tarefaTitulo :: String
  , tarefaDescricao :: String
  , tarefaPrioridade :: Int
  , tarefaStatus :: StatusTarefa
  , tarefaIdCriador :: Int
  , tarefaIdResponsavel :: Int
  , tarefaEmpresaId :: Int
  } deriving (Show, Eq)

backlogEmpresa :: Usuario -> [Tarefa] -> IO [Tarefa]
backlogEmpresa usuario tarefas = do
  let tarefasBacklog = filter (\t -> tarefaStatus t == Backlog) tarefas
  putStrLn "\nBacklog de Tarefas da Empresa:"
  mapM_ (\t -> putStrLn $ show (tarefaId t) ++ ": " ++ tarefaTitulo t ++ " - " ++ show (tarefaStatus t)) tarefasBacklog
  putStrLn "\nOpções:"
  putStrLn "1. Criar nova tarefa"
  putStrLn "2. Voltar ao menu principal"
  putStrLn "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      novaTarefa <- adicionarTarefa usuario
      backlogEmpresa usuario (novaTarefa : tarefas)  
    "2" -> return tarefas  
    _   -> do
      putStrLn "Opção inválida, tente novamente."
      backlogEmpresa usuario tarefas  

adicionarTarefa :: Usuario -> IO Tarefa
adicionarTarefa usuario = do
  putStrLn "Digite o ID da Tarefa:"
  id <- readLn
  putStrLn "Digite o Título da Tarefa:"
  titulo <- getLine
  putStrLn "Digite a Descrição da Tarefa:"
  descricao <- getLine
  putStrLn "Digite a Prioridade da Tarefa (1-5):"
  prioridade <- readLn
  let empresaId = usuarioEmpresaId usuario 
  let criadorId = usuarioId usuario 
  let tarefa = Tarefa id titulo descricao prioridade Backlog criadorId 0 empresaId
  putStrLn $ "Tarefa adicionada: " ++ show tarefa
  return tarefa

-- Função para listar as tarefas do usuário e permitir a modificação de status
listarTarefasUsuario :: Usuario -> [Tarefa] -> IO [Tarefa]
listarTarefasUsuario usuario tarefas = do
  let tarefasUsuario = filter (\t -> tarefaIdCriador t == usuarioId usuario || tarefaIdResponsavel t == usuarioId usuario) tarefas
  if null tarefasUsuario
    then do
      putStrLn "O usuário não possui tarefas."
      return tarefas -- Retorna a lista sem modificações
    else do
      putStrLn "Tarefas do usuário:"
      mapM_ (\t -> putStrLn $ "ID: " ++ show (tarefaId t) ++ ", Título: " ++ tarefaTitulo t ++ ", Status: " ++ show (tarefaStatus t)) tarefasUsuario
      putStrLn "Digite o ID da tarefa que deseja modificar o status ou 0 para sair:"
      tarefaIdStr <- getLine
      let tarefaIdEscolhida = read tarefaIdStr :: Int
      if tarefaIdEscolhida == 0
        then return tarefas -- Sai sem modificar
        else do
          -- Verifica se a tarefa existe
          let tarefaSelecionada = filter (\t -> tarefaId t == tarefaIdEscolhida) tarefasUsuario
          if null tarefaSelecionada
            then do
              putStrLn "Tarefa não encontrada."
              listarTarefasUsuario usuario tarefas -- Chama a função novamente
            else do
              putStrLn "Digite o novo status da tarefa (2 para Pendente, 3 para EmDesenvolvimento, 4 para Concluido):"
              novoStatusEntrada <- getLine
              let novoStatus = case novoStatusEntrada of
                    "2" -> Pendente
                    "3" -> EmDesenvolvimento
                    "4" -> Concluido
                    _   -> tarefaStatus (head tarefaSelecionada) -- Se a entrada for inválida, mantém o status atual
              let tarefasAtualizadas = map (atualizarStatusTarefa tarefaIdEscolhida novoStatus) tarefas
              putStrLn "Status atualizado com sucesso!"
              return tarefasAtualizadas

-- Instância Show para StatusTarefa para facilitar a conversão para String


-- Função auxiliar para atualizar o status de uma tarefa
atualizarStatusTarefa :: Int -> StatusTarefa -> Tarefa -> Tarefa
atualizarStatusTarefa idTarefa novoStatus tarefa =
  if tarefaId tarefa == idTarefa
    then tarefa { tarefaStatus = novoStatus }
    else tarefa

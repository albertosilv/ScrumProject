module TelaInicial where

import Sprint (Sprint, listarSprint, listarSprintsDaEmpresa)
import Tarefa (Tarefa, backlogEmpresa, listarTarefasUsuario)
import Usuario (Usuario (..), modificarUsuario)

-- Menu para Administrador
menuAdministrador :: Usuario -> [Usuario] -> [Tarefa] -> [Sprint] -> IO ([Usuario], [Tarefa], [Sprint])
menuAdministrador usuario usuarios tarefas sprints = do
  putStrLn "\nMenu Administrador:"
  putStrLn "1. Backlog"
  putStrLn "2. Sprint"
  putStrLn "3. Dados do Usuario"
  putStrLn "4. Logout"
  escolha <- getLine
  case escolha of
    "1" -> do
      tarefasAtualizado <- backlogEmpresa usuario tarefas
      menuAdministrador usuario usuarios tarefasAtualizado sprints
    "2" -> do
      (sprintsAtualizada, tarefasAtualizado) <- listarSprintsDaEmpresa usuario sprints usuarios tarefas
      menuAdministrador usuario usuarios tarefasAtualizado sprintsAtualizada
    "3" -> do
      usuarioAtualizado <- modificarUsuario usuario
      let usuariosAtualizados = map (\u -> if usuarioId u == usuarioId usuario then usuarioAtualizado else u) usuarios
      menuAdministrador usuarioAtualizado usuarios tarefas sprints
    "4" -> return (usuarios, tarefas, sprints)
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuAdministrador usuario usuarios tarefas sprints

-- Menu para Usuário Comum
menuComum :: Usuario -> [Usuario] -> [Tarefa] -> [Sprint] -> IO ([Usuario], [Tarefa], [Sprint])
menuComum usuario usuarios tarefas sprints = do
  putStrLn "\nMenu Usuário Comum:"
  putStrLn "1. Sprints"
  putStrLn "2. Tarefas"
  putStrLn "3. Dados do Usuario"
  putStrLn "4. Logout"
  escolha <- getLine
  case escolha of
    "1" -> do
      listarSprint usuario sprints tarefas
      menuComum usuario usuarios tarefas sprints
    "2" -> do
      listarTarefasUsuario usuario tarefas
      menuComum usuario usuarios tarefas sprints
    "3" -> do
      usuarioAtualizado <- modificarUsuario usuario
      let usuariosAtualizados = map (\u -> if usuarioId u == usuarioId usuario then usuarioAtualizado else u) usuarios
      menuComum usuarioAtualizado usuariosAtualizados tarefas sprints
    "4" -> return (usuarios, tarefas, sprints)
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      menuComum usuario usuarios tarefas sprints

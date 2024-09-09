
{-# LANGUAGE LambdaCase #-}
module Main where

import Usuario ( Usuario(usuarioNome), cadastrarUsuario, login )
import Tarefa ( Tarefa, adicionarTarefa, listarTarefas )
import Sprint ( Sprint, criarSprint )

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Gerenciamento Scrum"
  menuPrincipal [] [] []

menuPrincipal :: [Usuario] -> [Tarefa] -> [Sprint] -> IO ()
menuPrincipal usuarios tarefas sprints = do
  putStrLn "\nMenu Principal:"
  putStrLn "1. Cadastrar Usuário"
  putStrLn "2. Login"
  putStrLn "3. Adicionar Tarefa"
  putStrLn "4. Listar Tarefas"
  putStrLn "5. Criar Sprint"
  putStrLn "6. Sair"
  putStrLn "Escolha uma opção: "
  escolha <- getLine
  case escolha of
    "1" -> do
      usuario <- cadastrarUsuario
      menuPrincipal (usuario : usuarios) tarefas sprints
    "2" -> do
      login usuarios >>= \case
        Just usuario -> putStrLn $ "Logado como: " ++ usuarioNome usuario
        Nothing -> return ()
      menuPrincipal usuarios tarefas sprints
    "3" -> do
      tarefa <- adicionarTarefa
      menuPrincipal usuarios (tarefa : tarefas) sprints
    "4" -> do
      listarTarefas tarefas
      menuPrincipal usuarios tarefas sprints
    "5" -> do
      sprint <- criarSprint
      menuPrincipal usuarios tarefas (sprint : sprints)
    "6" -> putStrLn "Saindo..."
    _   -> do
      putStrLn "Opção inválida, tente novamente."
      menuPrincipal usuarios tarefas sprints

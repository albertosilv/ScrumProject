
{-# LANGUAGE LambdaCase #-}
module Main where
import Usuario (Usuario(..), TipoUsuario(..), cadastrarUsuario, login)
import Tarefa ( Tarefa (tarefaEmpresaId), adicionarTarefa )
import Sprint ( Sprint (sprintEmpresaId), criarSprint )
import TelaInicial ( menuComum, menuAdministrador)

-- Menu principal-
menuPrincipal :: [Usuario] -> [Tarefa] -> [Sprint] -> Maybe Usuario -> IO ()
menuPrincipal usuarios tarefas sprints usuarioLogado = do
    case usuarioLogado of
        Nothing -> do
            putStrLn "\nMenu Principal:"
            putStrLn "1. Cadastrar Usuário"
            putStrLn "2. Login"
            putStrLn "3. Sair"
            putStrLn "Escolha uma opção: "
            escolha <- getLine
            case escolha of
                "1" -> do
                    usuario <- cadastrarUsuario
                    menuPrincipal (usuario : usuarios) tarefas sprints Nothing
                "2" -> do
                    usuarioLogado <- login usuarios
                    case usuarioLogado of
                        Just usuario -> do
                            let tarefasFiltradas = filter (\t -> usuarioEmpresaId usuario == tarefaEmpresaId t) tarefas
                                sprintsFiltrados = filter (\s -> usuarioEmpresaId usuario == sprintEmpresaId s) sprints
                            putStrLn $ "Logado como: " ++ usuarioNome usuario
                            menuPrincipal usuarios tarefasFiltradas sprintsFiltrados (Just usuario)
                        Nothing -> do
                            putStrLn "Login falhou. Tente novamente."
                            menuPrincipal usuarios tarefas sprints Nothing
                "3" -> putStrLn "Saindo..."
                _   -> do
                    putStrLn "Opção inválida, tente novamente."
                    menuPrincipal usuarios tarefas sprints Nothing
        Just usuario -> do
            if usuarioPapel usuario == ProductOwner || usuarioPapel usuario == ScrumMaster
                then menuAdministrador usuario usuarios tarefas sprints
                else menuComum usuario usuarios tarefas sprints

-- Função principal
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Gerenciamento Scrum"
  menuPrincipal [] [] [] Nothing
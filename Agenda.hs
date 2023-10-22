import Data.List (find)

data Compromisso = Compromisso {
    titulo :: String,
    descricao :: String, 
    dataHora :: (Int, Int, Int, Int) -- (Dia, mes, Ano, Hora)
} deriving (Show, Read, Eq)

adicionarCompromisso :: Compromisso -> [Compromisso] -> [Compromisso]
adicionarCompromisso compromisso agenda = compromisso : agenda 

removerCompromisso :: String -> [Compromisso] -> [Compromisso]
removerCompromisso tit agenda = filter (\c -> tit /= titulo c) agenda

editarCompromisso :: String -> Compromisso -> [Compromisso] -> [Compromisso]
editarCompromisso tit novoCompromisso agenda =
    case find (\c -> tit == titulo c) agenda of
        Just _ -> map (\c -> if titulo c == tit then novoCompromisso else c) agenda
        Nothing -> agenda

visualizarCompromissos :: [Compromisso] -> IO ()
visualizarCompromissos agenda = mapM_ print agenda

filtrarPorMes :: Int -> [Compromisso] -> [Compromisso]
filtrarPorMes mes = filter (\(Compromisso _ _ (_, m, _, _)) -> m == mes)

filtrarPorDia :: Int -> [Compromisso] -> [Compromisso]
filtrarPorDia dia = filter (\(Compromisso _ _ (d, _, _, _)) -> d == dia)

main :: IO ()
main = do
    putStrLn "Bem-vindo à Agenda"
    menuPrincipal []

menuPrincipal :: [Compromisso] -> IO ()
menuPrincipal agenda = do
    putStrLn "Escolha uma opção: 1. Adicionar 2. Remover 3. Editar 4. Visualizar 5. Filtrar por dia 6. Filtrar por mês 7. Sair"
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Digite o título do compromisso:"
            tit <- getLine
            putStrLn "Digite a descrição do compromisso:"
            desc <- getLine
            putStrLn "Digite o dia do compromisso:"
            dia <- readLn
            putStrLn "Digite o mês do compromisso:"
            mes <- readLn
            putStrLn "Digite o ano do compromisso:"
            ano <- readLn
            putStrLn "Digite a hora do compromisso:"
            hora <- readLn
            let novoCompromisso = Compromisso tit desc (dia, mes, ano, hora)
            menuPrincipal (adicionarCompromisso novoCompromisso agenda)
        "2" -> do
            putStrLn "Digite o título do compromisso que deseja remover:"
            tit <- getLine
            menuPrincipal (removerCompromisso tit agenda)
        "3" -> do
            putStrLn "Digite o título do compromisso que deseja editar:"
            tit <- getLine
            putStrLn "Digite o novo título do compromisso:"
            novoTit <- getLine
            putStrLn "Digite a nova descrição do compromisso:"
            novaDesc <- getLine
            putStrLn "Digite o novo dia do compromisso:"
            novoDia <- readLn :: IO Int
            putStrLn "Digite o novo mês do compromisso:"
            novoMes <- readLn :: IO Int
            putStrLn "Digite o novo ano do compromisso:"
            novoAno <- readLn :: IO Int
            putStrLn "Digite a nova hora do compromisso:"
            novaHora <- readLn :: IO Int
            let novoCompromisso = Compromisso novoTit novaDesc (novoDia, novoMes, novoAno, novaHora)
            menuPrincipal (editarCompromisso tit novoCompromisso agenda)
        "4" -> do
            visualizarCompromissos agenda
            menuPrincipal agenda
        "5" -> do
            putStrLn "Digite o dia para filtrar compromissos:"
            dia <- readLn
            visualizarCompromissos $ filtrarPorDia dia agenda
            menuPrincipal agenda
        "6" -> do
            putStrLn "Digite o mês para filtrar compromissos:"
            mes <- readLn :: IO Int
            let compromissosFiltrados = filtrarPorMes mes agenda
            if null compromissosFiltrados
                then putStrLn "Nenhum compromisso encontrado para este mês."
                else do
                    putStrLn ("Compromissos para o mês " ++ show mes ++ ":")
                    visualizarCompromissos compromissosFiltrados
            menuPrincipal agenda
        "7" -> putStrLn "Até logo!"
        _   -> do
            putStrLn "Opção inválida!"           
            menuPrincipal agenda

module Main where

import Fileread
import Filesave
import Pairing
import Update
import System.IO (hFlush, stdout)

-- ==========================================
-- MAIN - Programa Principal
-- ==========================================

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "   GESTOR DE TORNEIOS DESPORTIVOS"
    putStrLn "=========================================="
    menuPrincipal

-- Menu Principal
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n--- MENU PRINCIPAL ---"
    putStrLn "1. Criar Torneio AVE"
    putStrLn "2. Criar Torneio Eliminatorio"
    putStrLn "3. Sair"
    putStr "Escolha uma opcao: "
    hFlush stdout
    opcao <- getLine
    
    case opcao of
        "1" -> criarTorneioAVE
        "2" -> criarTorneioElim
        "3" -> putStrLn "Ate breve!"
        _   -> do
            putStrLn "Opcao invalida!"
            menuPrincipal

-- ==========================================
-- TORNEIO AVE
-- ==========================================

criarTorneioAVE :: IO ()
criarTorneioAVE = do
    putStrLn "\n=== CRIAR TORNEIO AVE ==="
    putStr "Nome do torneio: "
    hFlush stdout
    nomeTorneio <- getLine
    
    putStr "Ficheiro CSV com participantes (ex: torneio_ave_vila_real.csv): "
    hFlush stdout
    ficheiro <- getLine
    
    -- Ler participantes do ficheiro
    torneio <- readTorneioAVE ficheiro
    putStrLn ("\nParticipantes carregados: " ++ show (length torneio))
    printTorneioAVE torneio
    
    -- Iniciar gestao do torneio
    gestaroTorneioAVE nomeTorneio torneio []

gestaroTorneioAVE :: String -> TorneioAVE -> ResultadosAVE -> IO ()
gestaroTorneioAVE nome torneio resultados = do
    putStrLn ("\n=== TORNEIO: " ++ nome ++ " ===")
    putStrLn "1. Gerar nova ronda"
    putStrLn "2. Registar resultado de jogo"
    putStrLn "3. Ver classificacao atual"
    putStrLn "4. Ver resultados"
    putStrLn "5. Apurar vencedores (Top 3)"
    putStrLn "6. Gravar torneio em ficheiro"
    putStrLn "7. Voltar ao menu principal"
    putStr "Escolha: "
    hFlush stdout
    opcao <- getLine
    
    case opcao of
        "1" -> do
            -- Gerar nova ronda
            let novosResultados = runAVEparing torneio resultados
            putStrLn "\n--- Nova Ronda Gerada ---"
            let novaRonda = drop (length resultados) novosResultados
            mapM_ print novaRonda
            gestaroTorneioAVE nome torneio novosResultados
            
        "2" -> do
            -- Registar resultado
            putStr "Nome do Jogador 1: "
            hFlush stdout
            j1 <- getLine
            putStr "Nome do Jogador 2: "
            hFlush stdout
            j2 <- getLine
            putStr "Pontos Jogador 1: "
            hFlush stdout
            p1 <- getLine
            putStr "Pontos Jogador 2: "
            hFlush stdout
            p2 <- getLine
            
            let pts1 = read p1 :: Int
            let pts2 = read p2 :: Int
            let (novoTorneio, novosResultados) = updateAVE (j1, j2, pts1, pts2) torneio resultados
            
            putStrLn "\nResultado registado com sucesso!"
            gestaroTorneioAVE nome novoTorneio novosResultados
            
        "3" -> do
            -- Ver classificacao
            putStrLn "\n--- CLASSIFICACAO ---"
            let classificacao = ordenarPorAVE torneio
            mapM_ mostrarClassificacao (zip [1..] classificacao)
            gestaroTorneioAVE nome torneio resultados
            
        "4" -> do
            -- Ver resultados
            printResultadosTorneioAVE resultados
            gestaroTorneioAVE nome torneio resultados
            
        "5" -> do
            -- Apurar top 3
            putStrLn "\n=========================================="
            putStrLn "           VENCEDORES DO TORNEIO"
            putStrLn "=========================================="
            let classificacao = ordenarPorAVE torneio
            let top3 = take 3 classificacao
            mapM_ mostrarPodio (zip [1..] top3)
            gestaroTorneioAVE nome torneio resultados
            
        "6" -> do
            -- Gravar ficheiros
            putStr "Nome base dos ficheiros (ex: meu_torneio): "
            hFlush stdout
            nomeBase <- getLine
            saveTorneioAVE (nomeBase ++ "_torneio.csv") torneio
            saveResultadosTorneioAVE (nomeBase ++ "_resultados.csv") resultados
            putStrLn "Ficheiros gravados!"
            gestaroTorneioAVE nome torneio resultados
            
        "7" -> menuPrincipal
        
        _ -> do
            putStrLn "Opcao invalida!"
            gestaroTorneioAVE nome torneio resultados

-- Ordenar jogadores por AVE (maior para menor)
ordenarPorAVE :: TorneioAVE -> TorneioAVE
ordenarPorAVE [] = []
ordenarPorAVE (x:xs) =
    let maiores = ordenarPorAVE [y | y <- xs, calculaAVE y >= calculaAVE x]
        menores = ordenarPorAVE [y | y <- xs, calculaAVE y < calculaAVE x]
    in maiores ++ [x] ++ menores
  where
    calculaAVE (_, vits, ders, _) =
        let total = vits + ders
        in if total > 0 
           then fromIntegral vits / fromIntegral total :: Double
           else 0.0

-- Mostrar classificacao
mostrarClassificacao :: (Int, (String, Int, Int, String)) -> IO ()
mostrarClassificacao (pos, (nome, vits, ders, genero)) = do
    let total = vits + ders
    let ave = if total > 0 
              then fromIntegral vits / fromIntegral total :: Double
              else 0.0
    putStrLn (show pos ++ "º - " ++ nome ++ " (" ++ genero ++ ") - " ++ 
              show vits ++ "V " ++ show ders ++ "D - AVE: " ++ show ave)

-- Mostrar podio
mostrarPodio :: (Int, (String, Int, Int, String)) -> IO ()
mostrarPodio (pos, (nome, vits, ders, _)) = do
    let medalha = case pos of
            1 -> "🥇"
            2 -> "🥈"
            3 -> "🥉"
            _ -> ""
    putStrLn (medalha ++ " " ++ show pos ++ "º LUGAR: " ++ nome ++ 
              " (" ++ show vits ++ " vitorias, " ++ show ders ++ " derrotas)")

-- ==========================================
-- TORNEIO ELIMINATORIO
-- ==========================================

criarTorneioElim :: IO ()
criarTorneioElim = do
    putStrLn "\n=== CRIAR TORNEIO ELIMINATORIO ==="
    putStr "Nome do torneio: "
    hFlush stdout
    nomeTorneio <- getLine
    
    putStr "Ficheiro CSV com equipas (ex: torneio_16_clubes.csv): "
    hFlush stdout
    ficheiro <- getLine
    
    -- Ler equipas do ficheiro
    torneio <- readTorneioElim ficheiro
    putStrLn ("\nEquipas carregadas: " ++ show (length torneio))
    printTorneioElim "Equipas Inscritas" torneio
    
    -- Gerar todas as rondas
    let resultados = runElimParing torneio
    putStrLn "\nEmparelhamento gerado!"
    
    -- Iniciar gestao do torneio
    gestaroTorneioElim nomeTorneio torneio resultados

gestaroTorneioElim :: String -> TorneioElim -> ResultadosElim -> IO ()
gestaroTorneioElim nome torneio resultados = do
    putStrLn ("\n=== TORNEIO ELIMINATORIO: " ++ nome ++ " ===")
    putStrLn "1. Ver emparelhamentos"
    putStrLn "2. Registar resultado de jogo"
    putStrLn "3. Ver situacao atual das equipas"
    putStrLn "4. Apurar vencedor final"
    putStrLn "5. Gravar torneio em ficheiro"
    putStrLn "6. Voltar ao menu principal"
    putStr "Escolha: "
    hFlush stdout
    opcao <- getLine
    
    case opcao of
        "1" -> do
            -- Ver emparelhamentos
            printResultadosTorneioElim resultados
            gestaroTorneioElim nome torneio resultados
            
        "2" -> do
            -- Registar resultado
            putStr "Ronda (ex: Oitavos de Final): "
            hFlush stdout
            ronda <- getLine
            putStr "Equipa A: "
            hFlush stdout
            eqA <- getLine
            putStr "Equipa B: "
            hFlush stdout
            eqB <- getLine
            putStr "Pontos Equipa A: "
            hFlush stdout
            pA <- getLine
            putStr "Pontos Equipa B: "
            hFlush stdout
            pB <- getLine
            
            let ptsA = read pA :: Int
            let ptsB = read pB :: Int
            let (novoTorneio, novosResultados) = updateElim (ronda, eqA, eqB, ptsA, ptsB) torneio resultados
            
            putStrLn "\nResultado registado com sucesso!"
            gestaroTorneioElim nome novoTorneio novosResultados
            
        "3" -> do
            -- Ver situacao
            putStrLn "\n--- SITUACAO DAS EQUIPAS ---"
            printTorneioElim "Estado Atual" torneio
            gestaroTorneioElim nome torneio resultados
            
        "4" -> do
            -- Apurar vencedor
            putStrLn "\n=========================================="
            putStrLn "        VENCEDOR DO TORNEIO"
            putStrLn "=========================================="
            let campeao = encontrarCampeao torneio
            case campeao of
                Just (clube, _, _) -> putStrLn ("🏆 CAMPEAO: " ++ clube ++ " 🏆")
                Nothing -> putStrLn "Ainda nao ha campeao definido!"
            gestaroTorneioElim nome torneio resultados
            
        "5" -> do
            -- Gravar ficheiros
            putStr "Nome base dos ficheiros (ex: meu_torneio): "
            hFlush stdout
            nomeBase <- getLine
            saveTorneioElim (nomeBase ++ "_torneio.csv") torneio
            saveResultadosTorneioElim (nomeBase ++ "_resultados.csv") resultados
            putStrLn "Ficheiros gravados!"
            gestaroTorneioElim nome torneio resultados
            
        "6" -> menuPrincipal
        
        _ -> do
            putStrLn "Opcao invalida!"
            gestaroTorneioElim nome torneio resultados

-- Encontrar campeao (equipa na fase "Campeao")
encontrarCampeao :: TorneioElim -> Maybe (String, String, String)
encontrarCampeao [] = Nothing
encontrarCampeao ((clube, "Campeao", adv):_) = Just (clube, "Campeao", adv)
encontrarCampeao (_:xs) = encontrarCampeao xs
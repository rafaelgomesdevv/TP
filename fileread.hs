module Fileread where

-- 1. TIPOS (Alterados para os tipos estruturados do professor)
type TorneioAVE = [(String, Int, Int, String)]
type ResultadosAVE = [(String, Int, Int, String, String)]
type TorneioElim = [(String, String, String)]
type ResultadosElim = [(String, Int, Int, String)]

-- 2. FUNÇÕES DE LEITURA (Alteradas para fazer "parse" seguro)

-- Ler o torneio AVE e devolver todas as linhas verdadeiras do CSV.
readTorneioAVE :: String -> IO TorneioAVE
readTorneioAVE nomeFicheiro = do
    conteudo <- readFile nomeFicheiro
    let linhas = lines conteudo
    -- Alterado: Em vez de 'return (lines conteudo)', filtramos e convertemos
    return [ valor | linha <- linhas, Just valor <- [paraTorneioAVE linha] ]
  where
    -- ADAPTADO: Lê 'torneio_ave_vila_real.csv' (5 colunas)
    paraTorneioAVE linha =
        case separar linha of
            -- Ficheiro real: [Nome, Género, Vitórias, Derrotas, AVE]
            [c1, c2, c3, c4, _c5] ->
                -- Mapeia para o type: (Nome, Vitórias, Derrotas, Género)
                case (lerInt c3, lerInt c4) of
                    (Just v, Just d) -> Just (c1, v, d, c2)
                    _ -> Nothing -- É o cabeçalho
            _ -> Nothing -- É metadado ou linha mal formatada

-- Ler os resultados do torneio AVE.
readResultadosTorneioAVE :: String -> IO ResultadosAVE
readResultadosTorneioAVE nomeFicheiro = do
    conteudo <- readFile nomeFicheiro
    let linhas = lines conteudo
    return [ valor | linha <- linhas, Just valor <- [paraResultadosAVE linha] ]
  where
    -- ADAPTADO: Lê 'resultados_torneio_ave_vila_real.csv' (5 colunas)
    paraResultadosAVE linha =
        case separar linha of
            -- Ficheiro real: [Ronda, Jogador 1, Jogador 2, Resultado, Vencedor]
            [_c1, c2, c3, c4, c5] -> -- Ignoramos a Ronda (c1)
                -- Tenta ler o Resultado (ex: "2–4")
                case parseScore c4 of 
                    -- Mapeia para o type: (J1, PtsA, PtsB, "Descricao", Vencedor)
                    Just (pa, pb) -> Just (c2, pa, pb, c2 ++ " vs " ++ c3, c5)
                    _ -> Nothing -- É o cabeçalho
            _ -> Nothing -- Linha mal formatada

-- Ler o torneio eliminatório.
readTorneioElim :: String -> IO TorneioElim
readTorneioElim nomeFicheiro = do
    conteudo <- readFile nomeFicheiro
    let linhas = lines conteudo
    return [ valor | linha <- linhas, Just valor <- [paraTorneioElim linha] ]
  where
    -- ADAPTADO: Lê 'torneio_16_clubes.csv' (2 colunas)
    paraTorneioElim linha =
        case separar linha of
            -- Ficheiro real: [Nº, Clube]
            [c1, c2] ->
                -- Mapeia para o type: (Clube, "Fase N/D", "Adversário N/D")
                case lerInt c1 of -- Tenta ler o Nº
                    Just _num -> Just (c2, "Oitavos", "N/D")
                    _ -> Nothing -- É o cabeçalho
            _ -> Nothing

-- Ler os resultados do torneio eliminatório.
readResultadosTorneioElim :: String -> IO ResultadosElim
readResultadosTorneioElim nomeFicheiro = do
    conteudo <- readFile nomeFicheiro
    let linhas = lines conteudo
    return [ valor | linha <- linhas, Just valor <- [paraResultadosElim linha] ]
  where
    -- ADAPTADO: Lê 'resultados_torneio_16_clubes.csv' (13+ colunas)
    paraResultadosElim linha =
        case separar linha of
            -- Ficheiro real: [Ronda, Jogo, EqA, EqB, PtsA, PtsB, ...]
            (c1 : _c2 : c3 : _c4 : c5 : c6 : _rest) ->
                -- 'lerIntOuZero' trata dos "###"
                let pa = lerIntOuZero c5
                    pb = lerIntOuZero c6
                in if c1 == "Ronda" -- Ignora o cabeçalho
                   then Nothing
                   else Just (c1, pa, pb, c3) -- (Ronda, PtsA, PtsB, EquipaA)
            _ -> Nothing -- Linha mal formatada

-- 3. FUNÇÕES DE IMPRESSÃO (Alteradas para 'print')

-- Imprimir o torneio AVE tal como está no ficheiro.
printTorneioAVE :: TorneioAVE -> IO ()
printTorneioAVE xs = do
    putStrLn "Torneio AVE - Vila Real"
    putStrLn "======================="
    -- Alterado: 'mapM_ print' imprime os tuplos
    mapM_ print xs

-- Imprimir os resultados do torneio AVE.
printResultadosTorneioAVE :: ResultadosAVE -> IO ()
printResultadosTorneioAVE xs = do
    putStrLn "Resultados Torneio AVE - Vila Real"
    putStrLn "=================================="
    mapM_ print xs

-- Imprimir o torneio eliminatório.
printTorneioElim :: String -> TorneioElim -> IO ()
printTorneioElim titulo xs = do
    putStrLn titulo
    putStrLn "================================"
    mapM_ print xs

-- Imprimir os resultados do torneio eliminatório.
printResultadosTorneioElim :: ResultadosElim -> IO ()
printResultadosTorneioElim xs = do
    putStrLn "Resultados Torneio Eliminatório"
    putStrLn "==============================="
    mapM_ print xs

-- 4. FUNÇÕES AUXILIARES (Adicionadas no fim)
--    (Estas têm de estar na coluna 1, sem indentação)

separar :: String -> [String]
separar [] = [""]
separar (c:cs)
    | c == ';' || c == ',' = "" : separar cs
    | otherwise = 
        let resto@(x:xs) = separar cs 
        in (c:x) : xs

lerInt :: String -> Maybe Int
lerInt str =
    case reads str of
        [(valor, "")] -> Just valor
        _ -> Nothing

lerIntOuZero :: String -> Int
lerIntOuZero str =
    case lerInt str of
        Just val -> val
        _ -> 0 -- Se for "###" ou "Pontuação A", devolve 0

parseScore :: String -> Maybe (Int,Int)
parseScore s =
  let t = map (\c -> if c == '–' then '-' else c) s -- Trata o '–' (traço longo)
      (a,rest) = break (=='-') t
  in case rest of
       ('-':b) -> do x <- lerInt a
                     y <- lerInt b
                     Just (x,y)
       _       -> Nothing
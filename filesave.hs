module Filesave where
import Fileread

-- T4.1 – Gravar torneio AVE em ficheiro CSV
saveTorneioAVE :: String -> TorneioAVE -> IO ()
saveTorneioAVE nomeFicheiro torneio = do
    let cabecalho = "Nome;Genero;Vitorias;Derrotas;AVE"
    let linhas = map converteLinhaTorneioAVE torneio
    let conteudo = cabecalho : linhas
    writeFile nomeFicheiro (unlines conteudo)
    putStrLn ("Ficheiro " ++ nomeFicheiro ++ " gravado com sucesso!")
  where
    -- Converte (Nome, Vitorias, Derrotas, Genero) para linha CSV
    converteLinhaTorneioAVE :: (String, Int, Int, String) -> String
    converteLinhaTorneioAVE (nome, vits, ders, genero) =
        let totalJogos = vits + ders
            ave = if totalJogos > 0 
                  then arredondar2Casas (fromIntegral vits / fromIntegral totalJogos)
                  else "0.0"
        in nome ++ ";" ++ genero ++ ";" ++ show vits ++ ";" ++ show ders ++ ";" ++ ave
    
    -- Arredonda para 2 casas decimais (versao simples)
    arredondar2Casas :: Double -> String
    arredondar2Casas x = 
        let inteiro = floor (x * 100)
            parteInt = inteiro `div` 100
            parteDec = inteiro `mod` 100
        in show parteInt ++ "." ++ (if parteDec < 10 then "0" else "") ++ show parteDec

-- T4.1 – Gravar resultados do torneio AVE em ficheiro CSV
saveResultadosTorneioAVE :: String -> ResultadosAVE -> IO ()
saveResultadosTorneioAVE nomeFicheiro resultados = do
    let cabecalho = "Ronda;Jogador 1;Jogador 2;Resultado;Vencedor"
    let linhas = zipWith converteLinhaResultadosAVE [1..] resultados
    let conteudo = cabecalho : linhas
    writeFile nomeFicheiro (unlines conteudo)
    putStrLn ("Ficheiro " ++ nomeFicheiro ++ " gravado com sucesso!")
  where
    -- Converte (J1, PtsA, PtsB, Descricao, Vencedor) para linha CSV
    converteLinhaResultadosAVE :: Int -> (String, Int, Int, String, String) -> String
    converteLinhaResultadosAVE ronda (j1, ptsA, ptsB, descricao, vencedor) =
        let j2 = extraiJogador2 descricao j1
            resultado = show ptsA ++ "-" ++ show ptsB
        in "Ronda " ++ show ronda ++ ";" ++ j1 ++ ";" ++ j2 ++ ";" ++ resultado ++ ";" ++ vencedor
    
    -- Extrai o nome do jogador 2 da descricao "J1 vs J2"
    extraiJogador2 :: String -> String -> String
    extraiJogador2 descricao j1 =
        let resto = drop (length j1 + 4) descricao
        in if resto == "" then "N/D" else resto

-- T4.2 – Gravar torneio Eliminatorio em ficheiro CSV
saveTorneioElim :: String -> TorneioElim -> IO ()
saveTorneioElim nomeFicheiro torneio = do
    let cabecalho = "Numero;Clube"
    let linhas = zipWith converteLinhaTorneioElim [1..] torneio
    let conteudo = cabecalho : linhas
    writeFile nomeFicheiro (unlines conteudo)
    putStrLn ("Ficheiro " ++ nomeFicheiro ++ " gravado com sucesso!")
  where
    -- Converte (Clube, Fase, Adversario) para linha CSV
    converteLinhaTorneioElim :: Int -> (String, String, String) -> String
    converteLinhaTorneioElim num (clube, _fase, _adv) =
        show num ++ ";" ++ clube

-- T4.2 – Gravar resultados do torneio Eliminatorio em ficheiro CSV
saveResultadosTorneioElim :: String -> ResultadosElim -> IO ()
saveResultadosTorneioElim nomeFicheiro resultados = do
    let cabecalho = "Ronda;Jogo;Equipa A;Equipa B;Pontuacao A;Pontuacao B;Penaltis A;Penaltis B;Vencedor;APD A;APD B;Melhor Jogador;Classificacao"
    let linhas = zipWith converteLinhaResultadosElim [1..] resultados
    let conteudo = cabecalho : linhas
    writeFile nomeFicheiro (unlines conteudo)
    putStrLn ("Ficheiro " ++ nomeFicheiro ++ " gravado com sucesso!")
  where
    -- Converte (Ronda, PtsA, PtsB, EquipaA) para linha CSV
    converteLinhaResultadosElim :: Int -> (String, Int, Int, String) -> String
    converteLinhaResultadosElim jogo (ronda, ptsA, ptsB, equipaA) =
        let equipaB = "N/D"
            vencedor = if ptsA > ptsB then equipaA
                       else if ptsB > ptsA then equipaB
                       else "Empate"
        in ronda ++ ";Jogo " ++ show jogo ++ ";" ++ equipaA ++ ";" ++ equipaB ++ ";" ++
           show ptsA ++ ";" ++ show ptsB ++ ";###;###;" ++
           vencedor ++ ";###;###;###;###"
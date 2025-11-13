module Pairing where
import Fileread 

-- AVE: pares adjacentes, 0–0, "Por Jogar"
runAVEparing :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
runAVEparing torneio resultados =
  resultados ++
    [ ("Mesa" ++ show i, 0, 0, a ++ " vs " ++ b, "Por Jogar")
    | ((a,b), i) <- zip (pares [ n | (n,_,_,_) <- torneio ]) [1..] ]
  where
    pares (x:y:xs) = (x,y) : pares xs
    pares _        = []

-- ELIM: gera TODAS as rondas (Oitavos → Quartos → Meias → Final)
runElimParing :: TorneioElim -> ResultadosElim
runElimParing inscritos = concat (geraRondas nomes 1)
  where
    nomes = [ clube | (clube,_,_) <- inscritos ]

    geraRondas :: [String] -> Int -> [[(String,Int,Int,String)]]
    geraRondas xs base
      | length xs <= 1 = []
      | otherwise =
          let ps     = pares xs
              fase   = etiqueta (length xs)
              jogos  = [ (fase, 0, 0, a ++ " vs " ++ b) | (a,b) <- ps ]
              venced = [ "Vencedor J" ++ show (base+i) | (i,_) <- zip [1..] ps ]
          in jogos : geraRondas venced (base + length ps)

    pares (x:y:rest) = (x,y) : pares rest
    pares _          = []

    etiqueta n
      | n >= 16  = "Oitavos de Final"
      | n == 8   = "Quartos de Final"
      | n == 4   = "Meias-finais"
      | n == 2   = "Final"
      | otherwise= "Ronda"

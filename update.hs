module Update where

import Fileread

-- (Jogador1, Jogador2, Pontos1, Pontos2)
type DadosJogoAVE  = (String, String, Int, Int)

-- (Ronda, EquipaA, EquipaB, PontosA, PontosB)
type DadosJogoElim = (String, String, String, Int, Int)

-------------------------------------------------
-- T3.1 – Atualizar torneio AVE
-------------------------------------------------

updateAVE :: DadosJogoAVE -> TorneioAVE -> ResultadosAVE -> (TorneioAVE, ResultadosAVE)
updateAVE (j1, j2, p1, p2) torneio resultados =
  let vencedor
        | p1 > p2   = j1
        | p2 > p1   = j2
        | otherwise = "Empate"

      descJogo = j1 ++ " vs " ++ j2

      novoResultado :: ResultadosAVE
      novoResultado =
        resultados ++ [(j1, p1, p2, descJogo, vencedor)]

      novoTorneio :: TorneioAVE
      novoTorneio =
        case compare p1 p2 of
          GT -> atualizaEstatisticas j1 j2 torneio
          LT -> atualizaEstatisticas j2 j1 torneio
          EQ -> torneio
  in
      (novoTorneio, novoResultado)

-- vencedor ganha +1 vitória; vencido +1 derrota
atualizaEstatisticas :: String -> String -> TorneioAVE -> TorneioAVE
atualizaEstatisticas vencedor vencido =
  map atualiza
  where
    atualiza (nome, vits, ders, genero)
      | nome == vencedor = (nome, vits + 1, ders    , genero)
      | nome == vencido  = (nome, vits    , ders + 1, genero)
      | otherwise        = (nome, vits    , ders    , genero)

-------------------------------------------------
-- T3.2 – Atualizar torneio Eliminatório
-------------------------------------------------

updateElim :: DadosJogoElim -> TorneioElim -> ResultadosElim -> (TorneioElim, ResultadosElim)
updateElim (ronda, eqA, eqB, pA, pB) torneio resultados =
  let vencedor
        | pA > pB   = eqA
        | pB > pA   = eqB
        | otherwise = ""

      perdedor
        | vencedor == eqA = eqB
        | vencedor == eqB = eqA
        | otherwise       = ""

      proximaFase =
        case ronda of
          "Oitavos de Final" -> "Quartos de Final"
          "Quartos de Final" -> "Meias-finais"
          "Meias-finais"     -> "Final"
          "Final"            -> "Campeão"
          _                  -> ronda

      novoTorneio :: TorneioElim
      novoTorneio = map (atualizaClube vencedor perdedor proximaFase) torneio

      novoResultados :: ResultadosElim
      novoResultados = map atualizaResultado resultados

  in (novoTorneio, novoResultados)
  where
    atualizaClube :: String -> String -> String -> (String,String,String) -> (String,String,String)
    atualizaClube venc perd prox (nome, fase, adv)
      | nome == venc                = (nome, prox      , adv)
      | nome == perd && perd /= ""  = (nome, "Eliminado", adv)
      | otherwise                   = (nome, fase      , adv)

    -- Atualiza um único registo de resultados: (Ronda, PtsA, PtsB, EquipaA)
    atualizaResultado :: (String, Int, Int, String) -> (String, Int, Int, String)
    atualizaResultado (r, ptsA, ptsB, equipaA)
      | r == ronda && equipaA == eqA = (r, pA, pB, equipaA)
      | otherwise                    = (r, ptsA, ptsB, equipaA)

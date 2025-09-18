module Fib where

import GHC.Natural (Natural)

-- Aufgabe 1: fib (ohne fibseq), endrekursiv mit Akkumulatoren
fib :: Natural -> Natural
fib n = go n 0 1
  where
    go :: Natural -> Natural -> Natural -> Natural
    go 0 !a _  = a
    go k !a !b = go (k - 1) b (a + b)


-- Aufgabe 2: fibseq endrekursiv, nur (:) verwenden und höchstens einmal reverse
-- Liefert die Liste [F0, F1, ..., Fn]
fibseq :: Natural -> [Natural]
fibseq 0 = [0]
fibseq n = reverse (go 1 0 1 [1, 0])
  where
    go :: Natural -> Natural -> Natural -> [Natural] -> [Natural]
    go !i !a !b acc
      | i >= n    = acc                          -- acc enthält aktuell [F1, F0] (bei n=1) bzw. bis Fn in umgekehrter Reihenfolge
      | otherwise =
          let !c = a + b
          in go (i + 1) b c (c : acc)            -- kons an den Anfang, dadurch O(1) pro Schritt

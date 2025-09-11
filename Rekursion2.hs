module Rekursion2 where

import Numeric.Natural (Natural)

-- Erste n Fibonacci-Zahlen als Liste: [F_0, ..., F_n]
fibseq :: Natural -> [Natural]
fibseq n = go 0 0 1 []
  where
    -- go i a b acc: i = aktuelle Position, a = F_i, b = F_{i+1}, acc in umgekehrter Reihenfolge
    go :: Natural -> Natural -> Natural -> [Natural] -> [Natural]
    go i a b acc
      | i > n     = reverse acc
      | otherwise = go (i + 1) b (a + b) (a : acc)

-- n-te Fibonacci-Zahl (F_n)
fib :: Natural -> Natural
fib n = last (fibseq n)

-- Liste von (Position, Fibonacci-Zahl) für 0..n
fiblist :: Natural -> [(Natural, Natural)]
fiblist n = zip [0 .. n] (fibseq n)

-- Pascal-Dreieck / Binomialkoeffizient: c n k = C(n, k)
-- Rand = 1, außerhalb = 0
c :: Int -> Int -> Int
c n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = c (n - 1) (k - 1) + c (n - 1) k

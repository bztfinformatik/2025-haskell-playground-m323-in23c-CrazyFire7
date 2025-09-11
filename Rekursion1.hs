module Rekursion1 where

import Numeric.Natural (Natural)

-- Exponentielles Wachstum mit jährlicher Einlage am Jahresanfang
-- C(0) = 0
-- C(t) = (C(t-1) + d) * (1 + i/100)
capital :: forall a. Fractional a => a -> a -> Natural -> a
capital d i t
  | t == 0    = 0
  | otherwise = (capital d i (t - 1) + d) * (1 + i / 100)

-- Zeitreihe des Medikamentenspiegels:
-- Start: Tag 0 -> 0.0
-- An jedem Tag: new = (prev + m) * (1 - r/100)
-- Ergebnisliste in der Reihenfolge: (t, ... , 1, 0)
druglevelTimeSeries :: forall a. Fractional a => a -> a -> Natural -> [(Natural, a)]
druglevelTimeSeries m r t = go 1 0 []
  where
    keep = 1 - r / 100
    -- go day prev acc: day = aktueller Tag (1..t), prev = Level am Ende von (day-1)
    -- acc sammelt in absteigender Reihenfolge, indem wir stets vorn anfügen
    go :: Natural -> a -> [(Natural, a)] -> [(Natural, a)]
    go day prev acc
      | day > t   = acc ++ [(0, 0)]
      | otherwise =
          let level = (prev + m) * keep
          in go (day + 1) level ((day, level) : acc)

-- Euklidischer Algorithmus für den ggT
egcd :: Integral a => a -> a -> a
egcd p q
  | q == 0    = abs p
  | otherwise = egcd q (p `mod` q)
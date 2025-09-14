import Numeric.Natural

-- Funktion capital
capital :: Fractional a => a -> a -> Natural -> a
capital d i t
  | t == 0 = 0
  | otherwise = previousCapital + d + yield
  where
    previousCapital = capital d i (t - 1)
    yield = (previousCapital + d) * (i / 100)
  
-- Funktion druglevelTimeSeries
-- create a time series of drug levels
-- m: daily administered dose
-- r: brake down rate in percentage
-- t: days (length of time series)
druglevelTimeSeries :: Fractional a => a -> a -> Natural -> [(Natural, a)]
-- pattern for day 0, ensure to terminate recursion
druglevelTimeSeries _ _ 0 = [(0, 0)]
-- pattern for subsequent days, prepend current day to previous days
druglevelTimeSeries m r t = (t, currentLevel) : previousDays
  where
    previousDays = druglevelTimeSeries m r (t - 1)
    -- use pattern matching to get second element of first tuple in list
    ((_, previousLevel) : _) = previousDays
    currentLevel = (previousLevel + m) * (1 - r / 100)

-- egcd (ggT)

egcd :: Integral a => a -> a -> a
egcd 0 0 = 0
egcd p 0 = abs p
egcd 0 q = abs q
egcd p q
  | remainder == 0 = n
  | otherwise = egcd n remainder
  where
    m = max (abs p) (abs q)
    n = min (abs p) (abs q)
    remainder = mod m n
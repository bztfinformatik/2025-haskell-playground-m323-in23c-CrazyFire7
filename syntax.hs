-- Grundsyntax
-- Kommentar
x=5
y = x + 3

add :: Int -> Int-> Int
add a b = a + b

a = add 3 4

-- Listen

nums = [1,2,3,4,5]
chars = ['a','b','c']

b = head nums  -- erstes Element
c = tail nums  -- Liste ohne erstes Element
d = nums !! 2  -- Indexzugriff, beginnt bei 0
e = length nums -- Länge der Liste

-- Tupel

person = ("Anna", 18)
f = fst person
g = snd person

-- List Comprehensions

h = [x*2 | x <- [1..5]] -- [2,4,6,8,10]
i = [x | x <- [1..10], even x] -- [2,4,6,8,10]

-- Polymorphismus

meineId :: a -> a
meineId x = x

-- Rekursion

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Pure Functions

addPureFunction a b = a + b      -- rein
-- print (a + b)        -- nicht rein (hat Nebeneffekt)

-- Guards (Sind wie if-else aber übersichtlicher)

bmi :: Double -> String
bmi x
  | x < 18.5 = "Untergewicht"
  | x < 25.0 = "Normal"
  | otherwise = "Uebergewicht"
-- Exercise - "Geometrische Formen"
data Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float
  deriving (Show)

aCircle :: Shape
aCircle = Circle 5.0

aRectangle :: Shape
aRectangle = Rectangle 4.0 6.0

aTriangle :: Shape
aTriangle = Triangle 3.0 4.0


-- Exercise - "Tabelle (Relation)"
data User = User
  { userId    :: Int
  , surname   :: String
  , firstname :: String
  , username  :: String
  } deriving (Show)

userTable :: [User]
userTable =
  [ User 100 "Wirth"   "Niklaus" "nw@inf.ethz.ch"
  , User 101 "Booch"   "Grady"   "gbo@oonet.com"
  , User 102 "Ritchie" "Dennis"  "dr@bell.org"
  ]
  
-- Exercise - "Summe Quadratzahlen"
sumSquares :: Integer
sumSquares = sum [x^2 | x <- [1..100]]

-- Exercise - "Koordinatenpaare ohne Diagonale"
coordinates :: [(Int, Int)]
coordinates = [(x, y) | x <- [-10..10], y <- [-10..10], x /= y, x /= -y]

-- Exercise - "Quader"
-- Alle (l,b,h) mit l <= b <= h und l^2 + b^2 + h^2 = 10000
cuboids :: [(Int, Int, Int)]
cuboids =
  [ (l, b, h)
  | l <- [1..100]
  , b <- [l..100]
  , h <- [b..100]
  , l*l + b*b + h*h == 10000
  ]

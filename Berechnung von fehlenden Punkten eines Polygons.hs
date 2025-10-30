-- Berechnung von fehlenden Punkten eines Polygons
-- Dieses Modul implementiert Funktionen zur Berechnung von fehlenden Eckpunkten
-- von geometrischen Figuren (gleichseitiges Dreieck, Quadrat, Rechteck)

-- Import des Vector-Typs aus geometry.hs
-- Falls geometry.hs ein Modul exportiert, müsste es importiert werden
-- Für diese Lösung definieren wir Vector hier inline

data Vector where
  Vector :: Double -> Double -> Vector
  deriving (Show)

-- ============================================================================
-- Hilfsfunktionen für Vektoroperationen
-- ============================================================================

-- Vektoraddition
vadd :: Vector -> Vector -> Vector
vadd (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

-- Vektorsubtraktion
vsub :: Vector -> Vector -> Vector
vsub (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)

-- Vektorrotation (Drehung um einen Winkel in Grad gegen den Uhrzeigersinn)
vrotate :: Double -> Vector -> Vector
vrotate angle (Vector x y) = Vector x' y'
  where
    -- Konvertierung von Grad zu Radiant
    rad = angle * pi / 180
    -- Rotationsmatrix anwenden
    x' = x * cos rad - y * sin rad
    y' = x * sin rad + y * cos rad

-- ============================================================================
-- Gleichseitiges Dreieck (Equilateral Triangle)
-- ============================================================================

-- Berechnet den dritten Eckpunkt C eines gleichseitigen Dreiecks
-- Gegeben: Punkte A und B
-- Der Winkel im gleichseitigen Dreieck beträgt 60°
equilateralTriangleCornerThree :: Vector -> Vector -> Vector
equilateralTriangleCornerThree vecA vecB = vecC
  where
    -- Schritt 1: Berechne den Vektor von A nach B
    vecAB = vsub vecB vecA
    -- Schritt 2: Drehe den Vektor AB um 60° gegen den Uhrzeigersinn
    vecT = vrotate 60 vecAB
    -- Schritt 3: Berechne den Eckpunkt C durch Addition von Vektor T zu Punkt A
    vecC = vadd vecA vecT

-- ============================================================================
-- Quadrat (Square)
-- ============================================================================

-- Berechnet den dritten Eckpunkt C eines Quadrats
-- Gegeben: Punkte A und B (erste beiden Eckpunkte)
-- Beim Quadrat sind alle Seiten gleich lang und alle Winkel 90°
-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecC - third corner of square
squareCornerThree :: Vector -> Vector -> Vector
squareCornerThree vecA vecB = vecC
  where
    -- Schritt 1: Berechne den Vektor von A nach B
    vecAB = vsub vecB vecA
    -- Schritt 2: Drehe den Vektor AB um 90° gegen den Uhrzeigersinn
    vecT = vrotate 90 vecAB
    -- Schritt 3: Berechne den Eckpunkt C durch Addition von Vektor T zu Punkt B
    vecC = vadd vecB vecT

-- Berechnet den vierten Eckpunkt D eines Quadrats
-- Gegeben: Punkte A und B (erste beiden Eckpunkte)
-- first argument: vecA - first corner of square
-- second argument: vecB - second corner of square
-- result: vecD - fourth corner of square
squareCornerFour :: Vector -> Vector -> Vector
squareCornerFour vecA vecB = vecD
  where
    -- Schritt 1: Berechne den Vektor von A nach B
    vecAB = vsub vecB vecA
    -- Schritt 2: Drehe den Vektor AB um 90° gegen den Uhrzeigersinn
    vecT = vrotate 90 vecAB
    -- Schritt 3: Berechne den Eckpunkt D durch Addition von Vektor T zu Punkt A
    vecD = vadd vecA vecT

-- ============================================================================
-- Rechteck (Rectangle)
-- ============================================================================

-- Berechnet den dritten Eckpunkt C eines Rechtecks
-- Gegeben: Punkte A und B (erste beiden Eckpunkte) sowie die Höhe des Rechtecks
-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecC - third corner of rectangle
rectangleCornerThree :: Vector -> Vector -> Double -> Vector
rectangleCornerThree vecA vecB height = vecC
  where
    -- Schritt 1: Berechne den Vektor von A nach B
    vecAB = vsub vecB vecA
    -- Schritt 2: Drehe den Vektor AB um 90° und normalisiere auf die Höhe
    vecABRotated = vrotate 90 vecAB
    -- Berechne die Länge von AB, um den Skalierungsfaktor zu bestimmen
    (Vector x y) = vecABRotated
    lengthAB = sqrt (x * x + y * y)
    scaleFactor = height / lengthAB
    -- Skaliere den rotierten Vektor auf die gewünschte Höhe
    vecT = Vector (x * scaleFactor) (y * scaleFactor)
    -- Schritt 3: Berechne den Eckpunkt C durch Addition von Vektor T zu Punkt B
    vecC = vadd vecB vecT

-- Berechnet den vierten Eckpunkt D eines Rechtecks
-- Gegeben: Punkte A und B (erste beiden Eckpunkte) sowie die Höhe des Rechtecks
-- first argument: vecA - first corner of rectangle
-- second argument: vecB - second corner of rectangle
-- third argument: height - height of rectangle
-- result: vecD - fourth corner of rectangle
rectangleCornerFour :: Vector -> Vector -> Double -> Vector
rectangleCornerFour vecA vecB height = vecD
  where
    -- Schritt 1: Berechne den Vektor von A nach B
    vecAB = vsub vecB vecA
    -- Schritt 2: Drehe den Vektor AB um 90° und normalisiere auf die Höhe
    vecABRotated = vrotate 90 vecAB
    -- Berechne die Länge von AB, um den Skalierungsfaktor zu bestimmen
    (Vector x y) = vecABRotated
    lengthAB = sqrt (x * x + y * y)
    scaleFactor = height / lengthAB
    -- Skaliere den rotierten Vektor auf die gewünschte Höhe
    vecT = Vector (x * scaleFactor) (y * scaleFactor)
    -- Schritt 3: Berechne den Eckpunkt D durch Addition von Vektor T zu Punkt A
    vecD = vadd vecA vecT

-- ============================================================================
-- Beispiele und Tests
-- ============================================================================

-- Beispiel für gleichseitiges Dreieck
exampleTriangle :: IO ()
exampleTriangle = do
  let a = Vector 0 0
  let b = Vector 4 0
  let c = equilateralTriangleCornerThree a b
  putStrLn $ "Gleichseitiges Dreieck:"
  putStrLn $ "  A = " ++ show a
  putStrLn $ "  B = " ++ show b
  putStrLn $ "  C = " ++ show c

-- Beispiel für Quadrat
exampleSquare :: IO ()
exampleSquare = do
  let a = Vector 0 0
  let b = Vector 4 0
  let c = squareCornerThree a b
  let d = squareCornerFour a b
  putStrLn $ "Quadrat:"
  putStrLn $ "  A = " ++ show a
  putStrLn $ "  B = " ++ show b
  putStrLn $ "  C = " ++ show c
  putStrLn $ "  D = " ++ show d

-- Beispiel für Rechteck
exampleRectangle :: IO ()
exampleRectangle = do
  let a = Vector 0 0
  let b = Vector 6 0
  let height = 3
  let c = rectangleCornerThree a b height
  let d = rectangleCornerFour a b height
  putStrLn $ "Rechteck:"
  putStrLn $ "  A = " ++ show a
  putStrLn $ "  B = " ++ show b
  putStrLn $ "  C = " ++ show c
  putStrLn $ "  D = " ++ show d

-- Alle Beispiele ausführen
main :: IO ()
main = do
  exampleTriangle
  putStrLn ""
  exampleSquare
  putStrLn ""
  exampleRectangle


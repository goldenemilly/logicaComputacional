import System.Console.Terminfo hiding (Point)
import qualified Distribution.SPDX as SPDX
data Shape = Circle Float | Square Float | Rectangle Float Float | Triangle Float| Trapeze Float Float Float deriving (Show)
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Square l) = l^2
area (Rectangle b h) = b * h
area (Triangle l) = (l^2) * sqrt 3 / 4
area (Trapeze b1 b2 h) = (b1 + b2) * h / 2
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Rectangle b h) = 2*b + 2*h
perimeter (Triangle l) = 3 * l
perimeter (Trapeze b1 b2 h) = b1 + b2 + 2 * sqrt (h^2 + ((b2 - b1) / 2)^2)

instance Eq Shape where
    s1 == s2 = area s1 == area s2

instance Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)

type Point = (Float, Float)
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
from0 :: Point -> Float
from0 (x, y) = distance (0, 0) (x, y)

data Haskellium = Haskellium {nombre :: String, apellido1 :: String, apellido2 :: String, lugar :: Point, casa :: Shape} deriving (Show)
son :: Haskellium -> Haskellium -> String -> Haskellium
son padre madre nombre1 = Haskellium {nombre = nombre1, apellido1 = apellido1 padre, apellido2 = apellido1 madre, lugar= lugar madre, casa = casa madre}
timeToWork :: Haskellium -> Float
timeToWork dude = 
    let distance = from0  (lugar dude)
    in if distance < 300
       then distance/30
       else distance/70

part1 = (Haskellium {nombre = "ade", apellido1 ="non", apellido2="om", lugar= (10, 12), casa = Circle 7} )
part2 = (Haskellium {nombre = "adeade", apellido1 ="nonnon", apellido2="omom", lugar= (5000, 12000), casa = Circle 9} )

houseCost :: Haskellium -> Float
houseCost ricardo = (perimeter (casa ricardo) * 2.5) + area (casa ricardo)

reverseFr :: [a] -> [a]
reverseFr [] = []
reverseFr (x:xs) = reverseFr xs ++ [x]

isPal :: String -> Bool
isPal s = s == reverseFr s

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs 

pascalN :: Int -> [Int]
pascalN 0 = [1]
pascalN n = zipWith (+) ([0] ++ prev) (prev ++ [0])
  where prev = pascalN (n - 1)
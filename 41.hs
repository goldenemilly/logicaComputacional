import System.Console.Terminfo hiding (Point)
import qualified Distribution.SPDX as SPDX
-- Tipos de datos Algebraicos.
-- Tipo de dato llamado Shape que representa a las figuras geométricas.
data Shape = Circle Float | Square Float | Rectangle Float Float | Triangle Float| Trapeze Float Float Float deriving (Show)

--Función area: Calcula el área de una figura.
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Square l) = l^2
area (Rectangle b h) = b * h
area (Triangle l) = (l^2) * sqrt 3 / 4
area (Trapeze b1 b2 h) = (b1 + b2) * h / 2

-- Función perimeter: Calcula el perimetro de una figura dada.
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
    
-- Crea un tipo de dato llamado Point que representa un punto en el plano cartesiano.
type Point = (Float, Float)
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
from0 :: Point -> Float
from0 (x, y) = distance (0, 0) (x, y)

-- Tipo de dato Haskellium que representa a un Haskellium.
data Haskellium = Haskellium {nombre :: String, apellido1 :: String, apellido2 :: String, lugar :: Point, casa :: Shape} deriving (Show)

-- Dados dos Haskellium y un String de nombre, regresa un Haskellium que sería hijo de los dos Haskelliums con el nombre dado.
son :: Haskellium -> Haskellium -> String -> Haskellium
son padre madre nombre1 = Haskellium {nombre = nombre1, apellido1 = apellido1 padre, apellido2 = apellido1 madre, lugar= lugar madre, casa = casa madre}

-- 
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

-- Listas y Funciones

--Función isPal: Indica si dada una cadena de texto es o no un palindromo.
isPal :: String -> Bool
isPal s = s == reversaFr s

--Función concat' (recursiva): Dada una lista de listas regresa la concatenación de todas las listas contenidas por esta.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs 

--Función pascalN: Regresa la n-ésima fila del triángulo de Pascal.
pascalN :: Int -> [Int]
pascalN 0 = [1]
pascalN n = zipWith (+) ([0] ++ prev) (prev ++ [0])
  where prev = pascalN (n - 1)

--Función reversaFr: Dada una lista, regresa la lista con los mismos elementos pero en orden opuesto.
reversaFr :: [a] -> [a]
reversaFr xs = foldr (\x acc -> acc++[x]) [] xs

-- Árboles 

-- Tipo de dato algebraico OneTwoTree a.
data OneTwoTree a = Void
                  | Node a (OneTwoTree a)
                  | Branch a (OneTwoTree a) (OneTwoTree a)
                  deriving (Show, Eq)

-- Función suma: Regresa la suma de todos los elementos del árbol.
suma :: OneTwoTree Int -> Int 
suma Void = 0
suma (Node a hijo) = a + (suma hijo)
suma (Branch a sub1 sub2) = a + (suma sub1) +  (suma sub2)

-- Función sinCero: Devuelve True si no hay ningún 0 en el árbol.
sinCero :: OneTwoTree Int -> Bool
sinCero Void = True
sinCero (Node a hijo) = (a /= 0) && (sinCero hijo) 
sinCero (Branch a sub1 sub2) = (a /= 0) && (sinCero sub1) && (sinCero sub2)





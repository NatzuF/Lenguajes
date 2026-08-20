module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 x y = ((x - media)^2 + (y - media)^2) / 2
    where
        media = (x + y) / 2

clasificaTemperatura :: Int -> String
clasificaTemperatura temperatura
    | temperatura <= 0    = "frio extremo"
    | temperatura <= 15   = "frio"
    | temperatura <= 25   = "templado"
    | temperatura <= 35   = "calido"
    | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [x] = [x]
intercala sep (x:xs) = x : sep : intercala sep xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n)          = n
evalua (Suma e1 e2)     = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2

module Utils where
import Data.Bits

-- Fibonacci memoizado
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib :: Int -> Integer
fib = (fibs !!)

-- Realiza exponenciacao rapida(binaria) mod p
binExp :: Integer -> Integer -> Integer -> Integer
binExp a b p = f' 1 a b where
    f' r a b
      | b == 0 = r
      | odd b = f' (r*a `mod` p) (a^2 `mod` p) (shiftR b 1)
      | otherwise = f' r (a^2 `mod` p) (shiftR b 1)

-- Calcula o gcd extendido de a e b, retorna (gcd(a, b), x, y) tais
-- que ax + by = gcd(a, b)
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b =
    let (g, x1, y1) = egcd b r
    in (g, y1, x1 - y1 * q)
       where
         (q, r) = divMod a b

-- Calcula o inverso modular de a mod m
invMod :: Integer -> Integer -> Integer
invMod a m =
    let (g, x, y) = egcd a m
    in case g of
      1 -> (x `mod` m + m) `mod` m
      _ -> error $ "invMod expects coprime numbers, received " ++ show a ++ " " ++ show m
         

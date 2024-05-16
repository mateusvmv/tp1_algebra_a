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

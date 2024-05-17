module Utils where
import Data.Bits
import System.Random
import Control.Arrow
import qualified Data.Set as Set

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


-- Permutacao da lista [fst .. snd] 
randomVals :: (RandomGen t, Integral a, Random a) => (a, a) -> t -> [a]
randomVals (f, s) gen = nextVal gen Set.empty 0
    where 
        maxv = fromIntegral s - fromIntegral f + 1
        nextVal g used szset
          | szset == maxv = []
          | x `Set.member` used = nextVal nextgen used szset
          | otherwise = x : nextVal nextgen (Set.insert x used) (szset+1)
          where (x, nextgen) = randomR (f, s) g 
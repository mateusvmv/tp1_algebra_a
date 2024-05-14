module Utils where
import Data.Bits
import Data.List
import Debug.Trace
import qualified Data.Set as Set

-- Fibonacci memoizado
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fib :: Int -> Integer
fib = (fibs !!)

-- Primos
nth_prime :: Int -> Integer = (primes !!)
primes :: [Integer] = 2 : 3 : sieve 5 where
    sieve :: Integer -> [Integer]
    sieve n =
        let
            top = min (n + 4096) (2 + n*n - 4*n)
            ps' = takeWhile (\p -> p*p < top) (tail primes)
            muls p = [l, l+p .. top] where l = p * ((n+p-1) `div` p .|. 1)
            setBits :: Integer -> [Integer] -> Integer
            setBits n = foldl' (\b x -> setBit b (fromIntegral (x-n))) 0
            nonMultiples = complement (setBits n (concat $ map muls ps'))
            integerPrimes = [i | i <- [n, n+2 .. top], testBit nonMultiples (fromIntegral (i-n))]
        in integerPrimes ++ (sieve (top+2))

-- Realiza exponenciacao rapida(binaria) mod p
binExp :: Integer -> Integer -> Integer -> Integer
binExp a b p = 
    let loop acc a' b' = if b' > 0 then 
            (if b' .&. 1 == 1 then 
                loop (acc * a' `mod` p) a' (b'-1) 
            else 
                loop acc (a' * a' `mod` p) (b' `shiftR` 1))
        else 
            acc
    in loop 1 a b

-- Retorna a lista de fatores de um numero
factorize :: Integer -> [Integer]
factorize 1 = []
factorize n = factorize' n primes where
    factorize' :: Integer -> [Integer] -> [Integer]
    factorize' n (p:ps) = if p*p > n then [n]
        else if n `rem` p == 0 then p : factorize' (n `div` p) (p:ps)
        else factorize' n ps

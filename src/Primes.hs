module Primes where
import Utils
import Data.Bits
import Data.List
import Debug.Trace

-- Primos
nthPrime :: Int -> Integer = (primes !!)
primes :: [Integer] = [2,3] ++ sieve 5 where
    sieve n =
        let
            top = min (n + 4096) (2 + n*n - 4*n)
            ps' = takeWhile (\p -> p*p < top) (tail primes)
            muls p = [l, l+2*p .. top] where l = p * ((n+p-1) `div` p .|. 1)
            multiples = foldl' (\b x -> setBit b (fromIntegral (x-n))) 0 (concat $ map muls ps') :: Integer
            integerPrimes = [i | i <- [n, n+2 .. top], not (testBit multiples (fromIntegral (i-n)))]
        in integerPrimes ++ (sieve (top+2))

-- Roda de fatoração
-- Lista de coprimos dos n primeiros primos, antes do ciclo
factWheel :: Int -> [Integer]
factWheel n =
    let ps = take n primes
        p = product ps
        coprimes = [x | x <- [p+1, p+3..p+p], all (\p -> x `mod` p /= 0) ps]
    in coprimes
wheel5 :: [Integer] = factWheel 5
wheel5Cycle :: Integer = product $ take 5 primes

-- Miller Rabin
isPrime :: Integer -> Bool
isPrime n
  | n < 10^12 = length (factorize n) == 1
  | any (\p -> n `mod` p == 0) (take 1000 primes) = False
  | otherwise = all (not . witness) (take 50 primes) where
    (s, d) = f' (0, n-1) where
        f' (s, n)
          | even n = f' (s+1, shiftR n 1)
          | otherwise = (s, n)
    witness a =
        let
            x = binExp a d n
            squares = iterate (\x -> x^2 `mod` n) x
        in all (\x -> x /= 1 && x /= n-1) (take (s+1) squares)

-- Encontra o próximo primo, dado um número N
firstPrimeGT :: Integer -> Integer
firstPrimeGT n
    | n <= wheel5Cycle = head $ dropWhile (\x -> x <= n) primes
    | otherwise = let
        x = n - (n `mod` wheel5Cycle)
        wheel = concatMap (\y -> map (\x -> x+y) wheel5) [x-wheel5Cycle, x..]
        candidates = dropWhile (\x -> x <= n) wheel
    in head [x | x <- candidates, isPrime x]

-- Retorna a lista de fatores de um numero
factorize :: Integer -> [Integer]
factorize 1 = []
factorize n = factorize' n primes where
    factorize' :: Integer -> [Integer] -> [Integer]
    factorize' n (p:ps)
        | p*p > n = [n]
        | n `rem` p == 0 = p : factorize' (n `div` p) (p:ps)
        | otherwise =  factorize' n ps

-- Fatoração parcial
data Factorization = Full [Integer] | Partial [Integer]

instance Show Factorization where
    show (Full fs) = "Full factorization: " ++ show fs
    show (Partial fs) = "Partial factorization: " ++ show fs

factorizeHuge :: Integer -> Factorization
factorizeHuge n = factorize' n primes
  where
    factorize' :: Integer -> [Integer] -> Factorization
    factorize' n (p:ps)
      | p > 10^5 = Partial []
      | p * p > n = Full [n]
      | n `rem` p == 0 = case factorize' (n `div` p) (p:ps) of
        Full fs -> Full (p:fs)
        Partial fs -> Partial (p:fs)
      | otherwise = factorize' n ps

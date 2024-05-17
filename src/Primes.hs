module Primes where
import Utils
import Data.Bits
import Data.List
import Debug.Trace
import Data.Array.Unboxed

-- Segmento do crivo
-- Coprimos de primos em ps entre a e b, a%2=1 e b%2=1
sieveSeg a' b' ps = [i*2+1 | i <- [a..b], coprime ! i] where
    a = shiftR a' 1; b = shiftR b' 1
    muls p = [l, l+p .. b] where l = shiftR (p * ((a'+p-1) `div` p .|. 1)) 1
    coprime = accumArray (\_ b -> b) True (a, b) (map (,False) $ concatMap muls ps) :: UArray Integer Bool
-- Primos
primes :: [Integer] = [2,3] ++ sieve 5 where
    sieve n = sieveSeg n top ps ++ sieve (top+2) where
        top = min (n + 2^15) (2 + n*n - 4*n)
        ps = takeWhile (\p -> p*p < top) (tail primes)
-- Roda de fatoração
-- Lista de coprimos dos n primeiros primos, antes do ciclo
factWheel :: Int -> [Integer]
factWheel k = sieveSeg (n+1) (n+n-1) ps where
    ps = tail $ take k primes
    n = 2 * product ps
wheel5 :: [Integer] = factWheel 5
wheel5Cycle :: Integer = product $ take 5 primes

-- Miller Rabin
isPrime :: Integer -> Bool
isPrime n
  | n < 10^12 = length (factorize n) == 1
  | any (\p -> n `mod` p == 0) (take 1000 primes) = False
  | otherwise = (not . any witness) (take 50 primes) where
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
    | n <= wheel5Cycle = head $ dropWhile (<= n) primes
    | otherwise = let
        x = n - (n `mod` wheel5Cycle)
        wheel = concatMap (\y -> map (+y) wheel5) [x-wheel5Cycle, x..]
        candidates = dropWhile (<= n) wheel
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
      | p > 10^6 = Partial []
      | p * p > n = Full [n]
      | n `rem` p == 0 = case factorize' (n `div` p) (p:ps) of
        Full fs -> Full (p:fs)
        Partial fs -> Partial (p:fs)
      | otherwise = factorize' n ps

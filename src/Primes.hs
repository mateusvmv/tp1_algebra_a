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
  | n < 10^12 = isCoprime n (takeWhile (\p -> p*p <= n) primes)
  | not $ isCoprime n (take 1000 primes) = False
  | otherwise = (not . any witness) (take 50 primes) where
    isCoprime n ps = not $ any (\p -> mod n p == 0) ps
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

-- Retorna um fator com pollardRho
pollardRho :: Integer -> Integer -> Integer
pollardRho n lim = step 1 2 n 2 2 where
    step i k n x y
        | d /= 1 && d /= n = d
        | i >= lim = 1
        | i == k = step (i+1) (2*k) n x' x'
        | otherwise = step (i+1) k n x' y where
            d = gcd n (abs y-x)
            x' = (\x n -> mod (x*x-1) n) x n

-- Retorna a lista de fatores de um numero
factorize :: Integer -> [Integer]
factorize' n ps
    | r == 1 = fs
    | otherwise = sort $ fs ++ factorize' f ps' ++ factorize' (div r f) ps'
    where
        f = pollardRho r r
        (fs, ps', r) = trialDiv n ps
factorize n = sort $ factorize' n primes

-- Fatoração parcial
data Factorization = Full [Integer] | Partial [Integer] Integer

instance Show Factorization where
    show (Full fs) = "Full factorization: " ++ show fs
    show (Partial fs r) = "Partial factorization: " ++ show fs ++ " * " ++ show r

trialDiv :: Integer -> [Integer] -> ([Integer], [Integer], Integer)
trialDiv n (p:ps)
    | n == 1 = ([], [], 1)
    | p*p > n = ([n], [], 1)
    | mod n p == 0 = (p:fs', ps', r)
    | p < 10^6 = trialDiv n ps
    | isPrime n = ([n], [], 1)
    | otherwise = ([], ps, n)
    where (fs', ps', r) = trialDiv (div n p) (p:ps)

factorizeHuge :: Integer -> Factorization
factorizeHuge' n ps
    | r == 1 || f == 1 = fs
    | otherwise = sort $ (fs ++ factorizeHuge' f ps') ++ factorizeHuge' (div r f) ps'
    where
        f = pollardRho r (10^6)
        (fs, ps', r) = trialDiv n ps
factorizeHuge n = if p == n then Full fs else Partial fs (div n p) where
    p = product fs
    fs = factorizeHuge' n primes

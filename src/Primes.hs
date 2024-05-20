module Primes where
import Utils
import Data.Bits
import Data.List
import Debug.Trace
import Data.Array.Unboxed
import Data.Maybe

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
pollardRho :: Integer -> Maybe Integer -> Integer
pollardRho n lim
    | isPrime n = n
    | otherwise = step 1 2 2 (\x->x*x-1) where
    l = fromMaybe (toInteger.ceiling.sqrt.fromIntegral$n) lim
    step i x y f
        | d /= 1 && d /= n = d
        | i > l = if maybe True (>l) lim then step 1 2 2 (+1) else 1
        | otherwise = step (i+1) x' y' f where
            d = gcd n (x-y)
            x' = mod (f x) n; y' = mod (f.f$y) n

-- Retorna a lista de fatores de um numero
factorizeRho n ps lim
    | n == 1 || p == 1 = []
    | otherwise = sort $ factorizeDiv p ps lim ++ factorizeDiv (div n p) ps lim
    where p = pollardRho n lim
factorizeDiv n ps lim
    | r == 1 = fs
    | otherwise = sort $ fs ++ factorizeRho r ps' lim
    where (fs, ps', r) = trialDiv n ps
trialDiv :: Integer -> [Integer] -> ([Integer], [Integer], Integer)
trialDiv n (p:ps)
    | n == 1 = ([], [], 1)
    | p*p > n = ([n], [], 1)
    | mod n p == 0 = (p:fs', ps', r)
    | p < 2^20 = trialDiv n ps
    | isPrime n = ([n], [], 1)
    | otherwise = ([], ps, n)
    where (fs', ps', r) = trialDiv (div n p) (p:ps)
data Factorization = Full [Integer] | Partial [Integer] Integer
instance Show Factorization where
    show (Full fs) = intercalate "*" (map show fs)
    show (Partial fs r) = intercalate "*" (map show fs) ++ "*" ++ show r ++ " (fatoração parcial)"
defactorize fs = case fs of
    Full fs -> product fs
    Partial fs r -> product fs * r
factorize :: Integer -> [Integer]
factorize n = sort $ factorizeDiv n primes Nothing
factorizePartial :: Integer -> Factorization
factorizePartial n = if n == p then Full fs else Partial fs (div n p) where
    p = product fs
    fs = sort $ factorizeDiv n primes (Just$2^20)

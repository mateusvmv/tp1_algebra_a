module Primes where
import Utils
import Data.Bits
import Data.List
import Debug.Trace
import Data.Array.Unboxed
import Data.Maybe

-- Segmento do Crivo de Eratosthenes
-- Coprimos de primos em ps ∪ {2} entre a e b, a≅1 mod 2 e b≅1 mod 2
-- Em O(n*log(log(n))) com n = b-a
sieveSeg a' b' ps = [i*2+1 | i <- [a..b], coprime ! i] where
    a = shiftR a' 1; b = shiftR b' 1
    muls p = [l, l+p .. b] where l = shiftR (p * ((a'+p-1) `div` p .|. 1)) 1
    coprime = accumArray (\_ b -> b) True (a, b) (map (,False) $ concatMap muls ps) :: UArray Integer Bool

-- Primos
-- Gera e armazena primos um segmento por vez
-- Em O(n*log(log(n))) com n sendo o maior primo gerado
primes :: [Integer] = [2,3] ++ sieve 5 where
    sieve n = sieveSeg n top ps ++ sieve (top+2) where
        top = min (n + 2^15) (2 + n*n - 4*n)
        ps = takeWhile (\p -> p*p < top) (tail primes)

-- Roda de fatoração
-- Lista do primeiro ciclo de coprimos dos k primeiros primos
-- Em O(n*log(log(n))) com n sendo o produto dos k primeiros primos
factWheel :: Int -> [Integer]
factWheel k = sieveSeg (n+1) (n+n-1) ps where
    ps = tail $ take k primes
    n = 2 * product ps
wheel5 :: [Integer] = factWheel 5
wheel5Cycle :: Integer = product $ take 5 primes

-- Determina se um número é primo
-- Caso ele seja menor que o limite do crivo ao quadrado, testamos seus fatores
-- Caso contrário, asseguramos que ele não é múltiplo de nenhum número no crivo
-- Então rodamos o Miller Rabin usando os 50 primeiros primos como testemunhas
isPrime :: Integer -> Bool
isPrime n
  | n < (softLimit^2) = isCoprime n (takeWhile (\p -> p*p <= n) primes)
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

-- Encontra o próximo primo, dado um número n
-- Usa uma roda para filtrar números coprimos
-- Caso o número possa ser um primo gerador da roda, verifica se ele está no crivo
-- Então, para cada candidato, determina se é primo
firstPrimeGT :: Integer -> Integer
firstPrimeGT n
    | n <= wheel5Cycle = head $ dropWhile (<= n) primes
    | otherwise = let
        x = n - (n `mod` wheel5Cycle)
        wheel = [y+x-wheel5Cycle | y <- wheel5] ++ map (+wheel5Cycle) wheel
        candidates = dropWhile (<= n) wheel
    in head [x | x <- candidates, isPrime x]

-- Retorna um fator com o algorítmo de pollardRho
-- Este algorítmo busca por ciclos usando o algorítmo de Floyd
-- O ciclo é gerado por uma função pseudo-aleatória, que varia para evitar casos degenerados
-- Um ciclo será encontrado caso gcd n (x-y) seja diferente de 1, nesse caso é multiplo de p
-- O esperado, pelo paradoxo do aniversário, é que em sqrt(p) iterações se encontre um ciclo
pollardRho :: Integer -> Maybe Integer -> Integer
pollardRho n lim
    | isPrime n = n
    | otherwise = fromMaybe 1 ds where
        cap = toInteger (maxBound :: Int)
        exp = fromInteger.min cap.ceiling.(*4).sqrt.sqrt.fromIntegral$n
        max = maybe maxBound (fromInteger.min cap) lim
        f k x = mod (x*x+k) n
        xs = concatMap (\k -> take exp $ iterate (f k) 2) [1..]
        ys = concatMap (\k -> take exp $ iterate (f k.f k) 2) [1..]
        ds = find (\d -> d /= 1 && d /= n)
            . take max
            $ zipWith (\x y -> gcd n (x-y)) xs ys

-- Fatora um número usando Pollard Rho para os fatores maiores
factorizeRho n ps lim
    | n == 1 || p == 1 = []
    | otherwise = sort $ factorizeDiv p ps lim ++ factorizeDiv (div n p) ps lim
    where p = pollardRho n lim

-- Fatora um número por divisão, removendo os seus fatores pequenos, e então usando Pollard Rho para os fatores maiores
factorizeDiv n ps lim
    | r == 1 = fs
    | otherwise = sort $ fs ++ factorizeRho r ps' lim
    where (fs, ps', r) = trialDiv n ps

-- Tenta dividir um número pelos primos no crivo, retornando os fatores encontrados, os não testados e o resto
trialDiv :: Integer -> [Integer] -> ([Integer], [Integer], Integer)
trialDiv n (p:ps)
    | n == 1 = ([], [], 1)
    | p*p > n = ([n], [], 1)
    | mod n p == 0 = (p:fs', ps', r)
    | p < softLimit = trialDiv n ps
    | isPrime n = ([n], [], 1)
    | otherwise = ([], ps, n)
    where (fs', ps', r) = trialDiv (div n p) (p:ps)

data Factorization = Full [Integer] | Partial [Integer] Integer
instance Show Factorization where
    show (Full fs) = intercalate "*" (map show fs)
    show (Partial fs r) = intercalate "*" (map show $ fs ++ [r]) ++ " (fatoração parcial)"

defactorize fs = case fs of
    Full fs -> product fs
    Partial fs r -> product fs * r

factorize :: Integer -> [Integer]
factorize n = sort $ factorizeDiv n primes Nothing

-- Realiza a fatoração de um número, com um limite de iterações no Pollard Rho
factorizePartial :: Integer -> Factorization
factorizePartial n = if n == p then Full fs else Partial fs (div n p) where
    p = product fs
    fs = sort $ factorizeDiv n primes (Just$2^20)

-- O maior número armazenado no crivo
softLimit :: Integer = 2^20
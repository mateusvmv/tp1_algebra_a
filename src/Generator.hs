module Generator where
import Utils
import Primes
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import System.Random

data OrderBounds = Exact Integer | Bounded Integer Integer
instance Show OrderBounds where
    show (Exact o) = "ordem = " ++ show o
    show (Bounded a b) = show a ++ " <= ordem <= " ++ show b

reduceOrder :: Integer -> [Integer] -> Integer -> Maybe Integer
reduceOrder g fs p
    | binExp g 2 p == g = Just 1
    | binExp g 3 p == g = Just 2
    | binExp g o p /= 1 = Nothing
    | otherwise = Just $ f' o fs
    where
        o = product fs
        f' o [] = o
        f' o (f:fs)
            | binExp g o' p == 1 = f' o' fs
            | otherwise = f' o fs
            where o' = div o f

orderEstimate :: Integer -> Factorization -> OrderBounds
orderEstimate g fs = case fs of
    Full fs -> Exact $ fromMaybe (p-1) $ reduceOrder g fs p
    Partial fs r -> if gcd hi r == 1 then Exact hi else Bounded lo hi where
        lo = div (hi * firstPrimeGT (2^41)) r
        hi = case reduceOrder g (r:fs) p of
            Just o -> o
            Nothing -> p-1
    where p = defactorize fs + 1

highOrderElement :: Factorization -> (Integer, OrderBounds)
highOrderElement f = (g, o) where
    p = defactorize f + 1
    (fs, o) = case f of
        Full fs -> (fs, Exact (p-1))
        Partial fs r -> (r:fs, Bounded (product fs * firstPrimeGT (2^41)) (p-1))
    g = foldl (\a b -> mod (a*b) p) 1 bs
    bs = map findB.multiplicity$fs
    findB (f, k) = head
        . filter (\b -> binExp b (div (p-1) f) p /= 1)
        . map (\a -> binExp a (div (p-1) (f^k)) p)
        $ primes
    multiplicity = map (\f -> (head f, length f)).group.sort

smallHighOrderElement :: Factorization -> (Integer, OrderBounds)
smallHighOrderElement f = (g, o) where
    p = defactorize f + 1
    g = head $ filter isHighOrder primes
    (fs, o) = case f of
        Full fs -> (fs, Exact (p-1))
        Partial fs r -> (r:fs, Bounded (product fs * firstPrimeGT (2^41)) (p-1))
    isHighOrder g = all
      ((\f -> binExp g (div (p - 1) f) p /= 1) . head) (group . sort $ fs)

-- Retorna um gerador do corpo (Z/pZ)* 
-- ou um elemento de ordem alta quando 
-- não se tem a fatoração de p-1 e p não é um primo seguro, p é primo.
generator :: Integer -> Integer
generator p
    | isPrime q = safeTest q
    | otherwise = defaultTest p
    where
        candidates = randomVals (2, p-1) (mkStdGen 99)
        q = div (p-1) 2
        defaultTest p' =
            let phi = p-1
                f = factorizePartial phi
                fact = case f of
                    Full fs -> fs
                    Partial fs _ -> fs

                factors = Set.toList . Set.fromList $ fact

                test :: [Integer] -> [Integer] -> Integer -> Integer
                test [] _ _ = -1
                test (x : xs) factors phi
                    | all (\fac_cur -> binExp x (phi `div` fac_cur) p /= 1) factors = x
                    | otherwise = test xs factors phi
            in test candidates factors phi
        safeTest q' =
            let
                test:: [Integer] -> Integer
                test [] = -1
                test (x : xs)
                    | x /= (p-1) && (binExp x q' p == (p-1)) = x
                    | otherwise = test xs
            in test candidates

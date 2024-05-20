module Generator where
import Utils
import Primes
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import System.Random

data OrderBounds = Exact Integer | LessThan Integer
instance Show OrderBounds where
    show (Exact o) = "ordem = " ++ show o
    show (LessThan o) = "ordem <= " ++ show o
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
orderEstimate g fs = case fs of
    Full fs -> Exact $ fromMaybe (p-1) $ reduceOrder g fs p
    Partial fs r -> case reduceOrder g fs p of
        Just o -> Exact o
        Nothing -> LessThan $ fromMaybe (p-1) $ reduceOrder g (r:fs) p
    where p = defactorize fs + 1

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

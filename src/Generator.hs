module Generator where
import Utils
import Primes
import Data.List
import qualified Data.Set as Set
import System.Random

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
                f = factorizeHuge phi
                fact = case f of
                    Full fs -> fs
                    Partial fs -> fs

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

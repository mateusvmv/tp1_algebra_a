module Generator where
import Utils
import Primes
import Data.List

-- Retorna um gerador do corpo (Z/pZ)*, p eh primo!
generator :: Integer -> Integer
generator p =
    let phi = p-1
        factors = nub (factorize phi)
        testedNumbers = [2..p] -- 2..p -- Seria interessante embaralhar ou algo do tipo aqui

        test :: [Integer] -> [Integer] -> Integer -> Integer
        test [] _ _ = -1
        test (x : xs) factors phi
            | all (\fac_cur -> binExp x (phi `div` fac_cur) p /= 1) factors = x
            | otherwise = test xs factors phi
    in test testedNumbers factors phi
        

module Generator where
import Utils
import Primes
import Data.List
import qualified Data.Set as Set
import System.Random

-- A ideia eh:
-- Faco a verificação com os fatores que tenho, 
-- Dado que eh fatoração total, tenho um gerador 
-- caso nao seja, 

-- Não usavel ainda, preciso arrumar a randomização e quando n tenho a fatoração total (tirar dúvida com o professor)
-- Retorna um gerador do corpo (Z/pZ)*, p eh primo!
generator :: Integer -> Integer
generator p =
    let phi = p-1
        factors = Set.toList . Set.fromList . factorize $ phi
        (testedNumbers, _) = uniqueRandomInts (2, p-1) 100 (mkStdGen 99) -- Need to transform it to a infinite list :(
        -- testedNumbers = [2..(p-1)]

        test :: [Integer] -> [Integer] -> Integer -> Integer
        test [] _ _ = -1
        test (x : xs) factors phi
            | all (\fac_cur -> binExp x (phi `div` fac_cur) p /= 1) factors = x
            | otherwise = test xs factors phi
    in test testedNumbers factors phi
        
